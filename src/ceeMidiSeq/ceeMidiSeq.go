package main

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"

	"github.com/pkg/errors"
)

//
// G E N E R I C   U T I L I T I E S
//

func unless(condition bool, format string, args ...interface{}) {
	if !condition {
		die(format, args...)
	}
}

func die(format string, args ...interface{}) {
	message := fmt.Errorf(format, args...)
	err := errors.WithStack(message)
	panic(err)
}

func run(format string, args ...interface{}) {
	cmd := fmt.Sprintf(format, args...)
	err := exec.Command("bash", "-c", cmd).Run()
	unless(err == nil, "Failed: %q: %v", cmd, err)
}

func backtick(format string, args ...interface{}) []string {
	cmd := fmt.Sprintf(format, args...)
	out, err := exec.Command("bash", "-c", cmd).Output()
	unless(err == nil, "FAILED COMMAND `%v`: %v", cmd, err)

	spl := strings.Split(string(out), "\n")
	if len(spl) > 0 && spl[len(spl)-1] == "" {
		spl = spl[:len(spl)-1]
	}
	return spl
}

func asStr(i int) string {
	return fmt.Sprintf("%d", i)
}

func strChoice(ifCondition bool, ifString, elseString string) string {
	if ifCondition {
		return ifString
	} else {
		return elseString
	}
}

func try(unit func()) (err error) {
	defer func() {
		if e := recover(); e != nil {
			if e2 := e.(error); e2 != nil {
				err = e2
			} else {
				err = fmt.Errorf("%v", e)
			}
		}
	}()
	unit()
	return nil
}

type Stanza struct {
	name      string
	body      string
	comment   string
	procedure func(ex *Expand, text string)
}

type StanzaMap map[string]Stanza

type SymbolTable map[string]string

//
// E X P A N S I O N
//
const reBracketLiteral = `\[\[[_a-zA-Z0-9]+\]\]`

var reBracketVariable = regexp.MustCompile(reBracketLiteral)
var reBracketWithSpace = regexp.MustCompile(`^(\s*)(` + reBracketLiteral + `)`)

var reSpaceBeginOfLine = regexp.MustCompile(`^\s*\S`)

func retabLines(lines []string, tab string) []string {
	if len(lines) == 0 {
		return nil
	}
	// First find the shortest tab length
	minLen := 1000000
	cuts := make([]int, len(lines))
	for index, line := range lines {
		s := reSpaceBeginOfLine.FindString(line)
		if s == "" {
			// Empty lines do not count
			cuts[index] = 0
			continue
		}

		cuts[index] = -1
		if len(s) < minLen {
			minLen = len(s) - 1 // Remember our RE has 1 non-space character in it, we just want the width in spaces

		}
	}

	for index := range cuts {
		if cuts[index] < 0 {
			cuts[index] = minLen
		}
	}

	var ret []string
	for index, line := range lines {
		ret = append(ret, tab+line[cuts[index]:])
	}

	return ret
}

func retabBuffer(body, tab string) string {
	// Strip the leading and trailing empty lines
	lines := strings.Split(body, "\n")
	for len(lines) > 0 && lines[0] == "" {
		lines = lines[1:]
	}
	for len(lines) > 0 && lines[len(lines)-1] == "" {
		lines = lines[:len(lines)-1]
	}
	lines = retabLines(lines, tab)
	return strings.Join(lines, "\n")
}

type Expand struct {
	// Public fields
	output      io.Writer
	stab        SymbolTable
	stanzas     StanzaMap
	slots       []SequenceSlot
	currentSlot SequenceSlot

	optConsole bool

	// Private
	depth       int
	stabStack   []SymbolTable
	stanzaStack []StanzaMap
	lastTab     string
}

var reTabToSpace = regexp.MustCompile(`\t`)

var defaultStanzaList = []Stanza{
	stz_NULL,
	stz_ALL_STOP,
	stz_NOTE_OFF_ND_LIST,
	stz_CC_OFF_ND_LIST,
	stz_NOTE_OFF_LATTICE_FOR_SEQ,
	stz_NOTE_OFF_LATTICE,
	stz_NOTE_ON_ND_LIST,
	stz_CC_ND_LIST,
	stz_NOTE_ON_LATTICE_FOR_ND,
	stz_RESET_MASTER_CLOCK_SEQUENCES,
	stz_STOP_MASTER_CLOCK,
	stz_USE_MASTER_CLOCK,
	stz_DONT_USE_MASTER_CLOCK,
	stz_NOTE_ON_LATTICE_FOR_SEQ,
	stz_NOTE_ON_LATTICE,
	stz_MIDI_ARRAY,
	stz_DECLARE_CONSOLE,
	stz_CLEAR_STATE_BODY,
	stz_QUEUE_STATE_BODY,
	stz_RUN_WAIT_BODY,
	stz_INCREMENT_PLAYHEAD,
	stz_PLAY_EVENT_BODY,
	stz_MIDI_PLAYER_INITIALIZE_LATTICE,
	stz_END_OF_SCRIPT_METADATA,
	stz_EMIT_SCRIPT,
	stz_NOTE_OFF_LATTICE_FOR_ND,
}

func newDefaultExpand(slots []SequenceSlot) *Expand {
	mp := map[string]Stanza{}
	for i := range defaultStanzaList {
		s := defaultStanzaList[i]
		s.body = reTabToSpace.ReplaceAllString(s.body, "    ")
		mp[defaultStanzaList[i].name] = s

	}
	return &Expand{
		output:     os.Stdout,
		stab:       map[string]string{},
		stanzas:    mp,
		slots:      slots,
		optConsole: true,
	}
}

var reComment = regexp.MustCompile(`(?m)^(\s*)\#\#(.*)$`)

var reSingleQuoteVariable = regexp.MustCompile("'[_a-zA-Z0-9]+")

func (ex *Expand) symbolExpand(text string) string {
	return reBracketVariable.ReplaceAllStringFunc(text, func(in string) string {
		name := in[2 : len(in)-2]
		v, ok := ex.stab[name]
		unless(ok, "symbolExpand failed to find key %v", name)
		return v
	})
}

func (ex *Expand) commentExpand(text string) string {
	return reComment.ReplaceAllString(text, "${1}{#${2} }")
}

var tickAlias = map[string]string{
	"seq_delta":    "?seq_delta[[[CG]]]",
	"curr_mark":    "%n[4*%i[[[CG]]]] + %loop[[[CG]]]*%seq_length_ticks[[[CG]]]",
	"curr_command": "%n[4*%i[[[CG]]]+1]",
	"curr_bb1":     "%n[4*%i[[[CG]]]+2]",
	"curr_bb2":     "%n[4*%i[[[CG]]]+3]",
}

func (ex *Expand) tickExpand(text string) string {
	return reSingleQuoteVariable.ReplaceAllStringFunc(text, func(in string) string {
		name := in[1:]
		if alias, ok := tickAlias[name]; ok {
			return alias
		}

		return "%" + name + "[[[CG]]]"
	})
}

func (ex *Expand) allExpand(s string) string {
	return ex.symbolExpand(ex.tickExpand(ex.commentExpand(s)))
}

func (ex *Expand) stanzaExpand(stanzaName string, tab string) {
	origTab := ex.lastTab
	ex.lastTab = tab
	stanza, ok := ex.stanzas[stanzaName]
	unless(ok, "stanzaExpand failed to find stanza %s", stanzaName)
	body := retabBuffer(stanza.body, tab)
	if stanza.procedure != nil {
		stanza.procedure(ex, body)
	} else {
		ex.expand(body)
	}
	ex.lastTab = origTab
	return
}

func (ex *Expand) stanzaNameSubstitution(oldAndNewNameList ...string) {
	unless(len(oldAndNewNameList)%2 == 0, "Odd number of arguments passed to stanzaNameSubstitution")
	ss := []Stanza{}
	for i := 0; i < len(oldAndNewNameList); i += 2 {
		ss = append(ss, Stanza{
			name: oldAndNewNameList[i],
			body: ex.lastTab + "[[" + oldAndNewNameList[i+1] + "]]",
		})
	}
	ex.pushStanzaDefinitions(ss...)
}

func (ex *Expand) pushStab(kvs ...string) {
	unless(len(kvs)%2 == 0, "Odd number of arguments passed to pushStab")

	origStab := ex.stab
	newStab := map[string]string{}

	for k, v := range origStab {
		newStab[k] = v
	}

	// Notice we ONLY expand the new kvs
	for i := 0; i < len(kvs); i += 2 {
		newStab[kvs[i]] = ex.allExpand(kvs[i+1])
	}

	ex.stabStack = append(ex.stabStack, origStab)
	ex.stab = newStab
}

func (ex *Expand) pushStanzaDefinitions(stanzas ...Stanza) {
	newStanzaMap := map[string]Stanza{}
	for k, v := range ex.stanzas {
		newStanzaMap[k] = v
	}
	for _, s := range stanzas {
		newStanzaMap[s.name] = s
	}
	ex.stanzaStack = append(ex.stanzaStack, ex.stanzas)
	ex.stanzas = newStanzaMap
}

func (ex *Expand) popStab() {
	unless(len(ex.stabStack) > 0, "popStab on an empty stabStack")
	ex.stab = ex.stabStack[len(ex.stabStack)-1]
	ex.stabStack = ex.stabStack[:len(ex.stabStack)-1]
}

func (ex *Expand) popStanzaDefinitions() {
	unless(len(ex.stanzaStack) > 0, "popStanza on an empty stanzaStack")
	ex.stanzas = ex.stanzaStack[len(ex.stanzaStack)-1]
	ex.stanzaStack = ex.stanzaStack[:len(ex.stanzaStack)-1]
}

func (ex *Expand) expand(input string) {
	const maxExpandDepth = 50
	ex.depth++
	defer func() { ex.depth-- }()

	unless(ex.depth < maxExpandDepth, "Exceeded %d iterations for expand", maxExpandDepth)

	// Strip the leading and trailing empty lines
	lines := strings.Split(input, "\n")
	for len(lines) > 0 && lines[0] == "" {
		lines = lines[1:]
	}
	for len(lines) > 0 && lines[len(lines)-1] == "" {
		lines = lines[:len(lines)-1]
	}

	// For each line, either expand the stanza, or expand text
	for _, line := range lines {
		match := reBracketWithSpace.FindStringSubmatch(line)
		if match != nil {
			name := match[2]
			name = name[2 : len(name)-2]
			stanza, ok := ex.stanzas[name]
			if ok {
				origTab := ex.lastTab
				ex.lastTab = match[1]
				body := retabBuffer(stanza.body, ex.lastTab)
				if stanza.procedure != nil {
					stanza.procedure(ex, body)
				} else {
					ex.expand(body)
				}
				ex.lastTab = origTab
				continue
			}
		}
		fmt.Fprintln(ex.output, ex.allExpand(line))
	}
}

//
// E M I T       S T A N Z A S
//

var stz_NULL = Stanza{
	name:    "NULL",
	comment: "The empty stanza",
}

var stz_ALL_STOP = Stanza{
	name:    "ALL_STOP",
	comment: "Stops all loops: master and on-shot loops.",
	body: `
	           $stJ := 0
	           while ($stJ < 16)
	               if ('callback # 0)
	                   %state[$stJ] := $PLAYER_CLEAR
	                   stop_wait('callback, 0)
	               end if
	               inc($stJ)
	           end while`,
}

var stz_NOTE_OFF_ND_LIST = Stanza{
	name: "NOTE_OFF_ND_LIST",
	body: `
            if (%offs[[[CHANNEL]]*128 + [[BB1]]] >= 0)
                set_midi([[CHANNEL]], $MIDI_COMMAND_NOTE_OFF, [[BB1]], 0)
                %offs[[[CHANNEL]]*128 + [[BB1]]] := $NO_CHOKE_GROUP
            end if`,
	procedure: func(ex *Expand, text string) {
		offs := map[int]bool{}
		for _, m := range ex.currentSlot.midi {
			if m.typ != Note_off_c {
				continue
			}
			offs[m.bb1] = true
		}
		offarr := []int{}
		for k := range offs {
			offarr = append(offarr, k)
		}
		sort.Ints(offarr)

		for _, n := range offarr {
			ex.pushStab("BB1", asStr(n))
			ex.expand(text)
			ex.popStab()
		}
	},
}

var stz_CC_OFF_ND_LIST = Stanza{
	name: "CC_OFF_ND_LIST",
	body: `
        if (%ccs[[[CHANNEL]]*128 + [[BB1]]] = $NON_DYNAMIC_CHOKE_GROUP)
            %ccs[[[CHANNEL]]*128 + [[BB1]]] := $NO_CHOKE_GROUP
            set_midi([[CHANNEL]], $MIDI_COMMAND_NOTE_CC, [[BB1]], %background_cc[[[CHANNEL]]*128 + [[BB1]]])
        end if`,
	procedure: func(ex *Expand, text string) {
		ccs := map[int]bool{}
		for _, m := range ex.currentSlot.midi {
			if m.typ != Control_c {
				continue
			}
			ccs[m.bb1] = true
		}
		ccarr := []int{}
		for k := range ccs {
			ccarr = append(ccarr, k)
		}
		sort.Ints(ccarr)
		for _, cc := range ccarr {
			ex.pushStab("BB1", asStr(cc))
			ex.expand(text)
			ex.popStab()
		}
	},
}

var stz_NOTE_OFF_LATTICE_FOR_ND = Stanza{
	name: "NOTE_OFF_LATTICE_FOR_ND",
	body: `
        [[NOTE_OFF_ND_LIST]]
        [[CC_OFF_ND_LIST]]`,
}

var stz_NOTE_OFF_LATTICE_FOR_SEQ = Stanza{
	name: "NOTE_OFF_LATTICE_FOR_SEQ",
	body: `
        if ('is_endgroup = 1 and 'trigger_held = [[SEQ]])
            'is_endgroup := 0
            ## Setting to the empty sequence here means we will clear existing notes, but immediately shift into
            ## $PLAYER_DONE. XXX: in the future, we should just push $PLAYER_CLEAR onto the stack
            'state := $PLAYER_QUEUE
            'i   := 0
            'end := 0
            stop_wait('callback, 0)
        end if
        'trigger_held := -1`,
}

var stz_NOTE_OFF_LATTICE = Stanza{
	name:    "NOTE_OFF_LATTICE",
	comment: "",
	body: `
            case [[SEQ]]
                [[BODY]]`,
	procedure: func(ex *Expand, text string) {
		for _, slot := range ex.slots {
			ex.currentSlot = slot
			ex.pushStab(
				"SEQ", asStr(slot.seq_number),
				"CG", asStr(ex.currentSlot.meta.cg_ChokeGroup),
				"SEQ_LENGTH_TICKS", asStr(ex.currentSlot.length_ticks),
				"SEQUENCE_START", asStr(ex.currentSlot.sequence_start),
				"SEQUENCE_END", asStr(ex.currentSlot.sequence_end),
				"CHANNEL", asStr(ex.currentSlot.meta.ch_OutputChannel),
				"VELOCITY_RADIUS", asStr(ex.currentSlot.meta.vr_VelocityRadius),
				"VELOCITY_DECAY", asStr(ex.currentSlot.meta.vd_VelocityDecay))
			ex.stanzaNameSubstitution(
				"BODY", strChoice(slot.meta.nd_NonDynamic, "NOTE_OFF_LATTICE_FOR_ND",
					"NOTE_OFF_LATTICE_FOR_SEQ"),
			)
			ex.expand(text)
			ex.popStanzaDefinitions()
			ex.popStab()
		}
	},
}

var stz_NOTE_ON_ND_LIST = Stanza{
	name: "NOTE_ON_ND_LIST",
	body: `
       ## NoteOn [[BB1]] [[BB2]]
       if (%offs[[[CHANNEL]]*128 + [[BB1]]] >= 0)
           set_midi([[CHANNEL]], $MIDI_COMMAND_NOTE_OFF, [[BB1]], 0)
       end if
       $stVelocity := [[BB2]] + $stVelocityDelta
       if ($stVelocity < 1)
           $stVelocity := 1
       end if
       if ($stVelocity > 127)
           $stVelocity := 127
       end if
       set_midi([[CHANNEL]], $MIDI_COMMAND_NOTE_ON, [[BB1]], $stVelocity)
       %offs[[[CHANNEL]]*128 + [[BB1]]] := $NON_DYNAMIC_CHOKE_GROUP`,
	procedure: func(ex *Expand, text string) {
		ex.pushStab("CHANNEL", asStr(ex.currentSlot.meta.ch_OutputChannel))
		for _, m := range ex.currentSlot.midi {
			if m.typ != Note_on_c {
				continue
			}
			ex.pushStab("BB1", asStr(m.bb1), "BB2", asStr(m.bb2))
			ex.expand(text)
			ex.popStab()
		}
		ex.popStab()
	},
}

var stz_CC_ND_LIST = Stanza{
	name: "CC_ND_LIST",
	body: `
       ## CC [[BB1]] [[BB2]]
       %ccs[[[CHANNEL]]*128 + [[BB1]]] := $NON_DYNAMIC_CHOKE_GROUP
       set_midi([[CHANNEL]], $MIDI_COMMAND_NOTE_CC, [[BB1]], [[BB2]])`,
	procedure: func(ex *Expand, text string) {
		ex.pushStab("CHANNEL", asStr(ex.currentSlot.meta.ch_OutputChannel))
		for _, m := range ex.currentSlot.midi {
			if m.typ != Control_c {
				continue
			}
			ex.pushStab("BB1", asStr(m.bb1), "BB2", asStr(m.bb2))
			ex.expand(text)
			ex.popStab()
		}
		ex.popStab()
	},
}

var stz_NOTE_ON_LATTICE_FOR_ND = Stanza{
	name:    "NOTE_ON_LATTICE_FOR_ND",
	comment: "The input note on dispatch select",
	body: `
       ## Non dynamic NoteOn
       $stVelocityDelta := real_to_int([[VELOCITY_RADIUS]]*int_to_real($MIDI_BYTE_1-64)/63.0)
       [[NOTE_ON_ND_LIST]]
       [[CC_ND_LIST]]
       $stDoinit := -1`,
}

var stz_RESET_MASTER_CLOCK_SEQUENCES = Stanza{
	name: "RESET_MASTER_CLOCK_SEQUENCES",
	body: `
        $stJ := 0
        while ($stJ < 16)
            if ($stJ # [[CG]] and %use_master_clock[$stJ] = 1 and 'callback # 0)
                %state[$stJ] := $PLAYER_CLEAR
                stop_wait('callback, 0)       
            end if
            inc($stJ)
        end while`,
}

var stz_STOP_MASTER_CLOCK = Stanza{
	name: "STOP_MASTER_CLOCK",
	body: `
            [[MAYBE_RESET_MASTER_CLOCK_SEQUENCES]]
            $master_clock_start := 0`,
	procedure: func(ex *Expand, text string) {
		ex.stanzaNameSubstitution(
			"MAYBE_RESET_MASTER_CLOCK_SEQUENCES", strChoice(ex.currentSlot.meta.as_AllStop,
				"NULL", "RESET_MASTER_CLOCK_SEQUENCES"))
		ex.popStanzaDefinitions()
	},
}

var stz_USE_MASTER_CLOCK = Stanza{
	name: "USE_MASTER_CLOCK",
	body: `
        'seq_decay_time   := 0
        'use_master_clock := 1`,
}

var stz_DONT_USE_MASTER_CLOCK = Stanza{
	name: "DONT_USE_MASTER_CLOCK",
	body: `
        'use_master_clock := 0
        'seq_delta        := [[VELOCITY_RADIUS]].0*int_to_real($MIDI_BYTE_1-64)/63.0
        'seq_decay_time   := [[VELOCITY_DECAY]]`,
}

var stz_NOTE_ON_LATTICE_FOR_SEQ = Stanza{
	name: "NOTE_ON_LATTICE_FOR_SEQ",

	body: `
        [[MAYBE_STOP_MASTER_CLOCK]]
        ## This is a sequence launch
        'i                := [[SEQUENCE_START]]
        'start            := [[SEQUENCE_START]]
        'end              := [[SEQUENCE_END]]
        'state            := $PLAYER_QUEUE
        'channel          := [[CHANNEL]]
        
        'seq_length_ticks := [[SEQ_LENGTH_TICKS]]
        'trigger_held     := [[SEQ]]
        'is_endgroup      := 0

        [[MASTER_CLOCK_CONFIG]]

        $stDoinit := -1
        if ('needsinit = 1)
            $stDoinit  := [[CG]]
            'needsinit := 0
        else 
            stop_wait('callback, 0)
        end if
        ## END case [[SEQ]]`,
	procedure: func(ex *Expand, text string) {
		ex.stanzaNameSubstitution(
			"MAYBE_STOP_MASTER_CLOCK",
			strChoice(ex.currentSlot.meta.sm_StopMasterClock, "STOP_MASTER_CLOCK", "NULL"),

			"MASTER_CLOCK_CONFIG",
			strChoice(ex.currentSlot.meta.um_UseMasterClock, "USE_MASTER_CLOCK", "DONT_USE_MASTER_CLOCK"))
		ex.expand(text)
		ex.popStanzaDefinitions()
	},
}

var stz_NOTE_ON_LATTICE = Stanza{
	name:    "NOTE_ON_LATTICE",
	comment: "The input note on dispatch select",
	body: `
            case [[SEQ]]
                [[MAYBE_ALL_STOP]]
                [[BODY]]`,
	procedure: func(ex *Expand, text string) {
		for _, slot := range ex.slots {
			ex.currentSlot = slot
			ex.pushStab(
				"SEQ", asStr(slot.seq_number),
				"CG", asStr(ex.currentSlot.meta.cg_ChokeGroup),
				"SEQ_LENGTH_TICKS", asStr(ex.currentSlot.length_ticks),
				"SEQUENCE_START", asStr(ex.currentSlot.sequence_start),
				"SEQUENCE_END", asStr(ex.currentSlot.sequence_end),
				"CHANNEL", asStr(ex.currentSlot.meta.ch_OutputChannel),
				"VELOCITY_RADIUS", asStr(ex.currentSlot.meta.vr_VelocityRadius),
				"VELOCITY_DECAY", asStr(ex.currentSlot.meta.vd_VelocityDecay))
			ex.stanzaNameSubstitution(
				"MAYBE_ALL_STOP", strChoice(slot.meta.as_AllStop, "ALL_STOP", "NULL"),
				"BODY", strChoice(slot.meta.nd_NonDynamic, "NOTE_ON_LATTICE_FOR_ND",
					"NOTE_ON_LATTICE_FOR_SEQ"),
			)
			ex.expand(text)
			ex.pushStanzaDefinitions()
			ex.popStab()
		}
	},
}

var stz_MIDI_ARRAY = Stanza{
	name: "MIDI_ARRAY",
	procedure: func(ex *Expand, text string) {
		arr := []string{}
		for _, slot := range ex.slots {
			if !slot.meta.em_Empty && !slot.meta.nd_NonDynamic {
				for _, row := range slot.midi {
					arr = append(arr, asStr(row.time), row.typ.ksbFormat(), asStr(row.bb1), asStr(row.bb2))
				}
			}
		}
		lastIndex := len(arr) - 1
		tab := "        "
		ex.output.Write([]byte(tab))
		for i := 0; i < len(arr); i++ {
			ex.output.Write([]byte(arr[i]))
			if (i+1)%16 == 0 {
				if i < lastIndex {
					ex.output.Write([]byte(", ...\n" + tab))
				} else {
					ex.output.Write([]byte("...\n"))
				}
			} else if i < lastIndex {
				ex.output.Write([]byte(", "))
			}
		}
	},
}

var stz_DECLARE_CONSOLE = Stanza{
	name: "DECLARE_CONSOLE",
	body: `
        declare ui_label $cons (6,6)
        { # Add text to the console with add_text_line($cons, <TEXT>) }`,
	procedure: func(ex *Expand, text string) {
		if ex.optConsole {
			ex.expand(retabBuffer(text, ex.lastTab))
		}
	},
}

var stz_CLEAR_STATE_BODY = Stanza{
	name: "CLEAR_STATE_BODY",
	body: `
        ## Clear ChokeGroup if needed    
        if ('previous_channel >= 0)
            $stJ := 0
            while ($stJ < 128)
                if (%offs['previous_channel*128 + $stJ] = [[CG]])
                    %offs['previous_channel*128 + $stJ] := $NO_CHOKE_GROUP
                    set_midi('previous_channel, $MIDI_COMMAND_NOTE_OFF, $stJ, 0)
                end if
                if (%ccs['previous_channel*128 + $stJ] = [[CG]])
                    %ccs['previous_channel*128 + $stJ] := $NO_CHOKE_GROUP
                    set_midi('previous_channel, $MIDI_COMMAND_CC, $stJ, ...
                             %background_cc['previous_channel*128 + $stJ])
                end if
                inc($stJ)                    
            end while
            if (%pitch_bend['previous_channel] = [[CG]])
                %pitch_bend['previous_channel] := $NO_CHOKE_GROUP
                set_midi('previous_channel, $MIDI_COMMAND_PITCH_BEND, $bend_center_msb, $bend_center_lsb)
            end if
        end if

        if ('state = $PLAYER_CLEAR)
            'state := $PLAYER_DONE
        end if`,
}

var stz_QUEUE_STATE_BODY = Stanza{
	name: "QUEUE_STATE_BODY",
	body: `
        'previous_channel := 'channel
        'state          := $PLAYER_RUNNING
        if ('use_master_clock = 1)
            if ($master_clock_start = 0)
                $master_clock_start := $ENGINE_UPTIME
            end if
            'start_time := $master_clock_start
            'ticks := real_to_int( 960.0*1000.0 ...
                                       * int_to_real($ENGINE_UPTIME-'start_time) ...
                                       / int_to_real($DURATION_QUARTER))
            'loop  := 'ticks/'seq_length_ticks
            ## Fast forward
            while ('i < 'end and 'curr_mark < 'ticks)
                inc('i)
            end while
            if ('i >= 'end)
                ## I don't think that this should ever happen, b/c there should always be a CYCLE instruction
                ## that is at time $seq_length_ticks. But this is purely defensive.
                inc('loop)
                'i := 'start
            end if
        else
            'start_time := $ENGINE_UPTIME
            'loop     := 0
        end if

        if ('i >= 'end)
            ## The sequence is empty
            'state := $PLAYER_DONE
        end if`,
}
var stz_RUN_WAIT_BODY = Stanza{
	name: "RUN_WAIT_BODY",
	body: `
        ## Player wait if needed
        'ticks := real_to_int( 960.0*1000.0 ...
                                   * int_to_real($ENGINE_UPTIME-'start_time) ...
                                   / int_to_real($DURATION_QUARTER))
        'mark  := 'curr_mark
        if ('mark > 'ticks)
            wait_ticks('mark - 'ticks)
        end if`,
}
var stz_INCREMENT_PLAYHEAD = Stanza{
	name: "INCREMENT_PLAYHEAD",
	body: `
        ## Increment the playhead
        inc('i)
        if ('i >= 'end)
            'state := $PLAYER_DONE
        end if`,
}

var stz_PLAY_EVENT_BODY = Stanza{
	name: "PLAY_EVENT_BODY",
	body: `
        ## Player plays notes if needed        
        select (%n[4*'i+1])
            case $MIDI_COMMAND_NOTE_ON
                if (%offs['channel*128 + %n[4*'i+2]] >= 0)
                    set_midi('channel, $MIDI_COMMAND_NOTE_OFF, %n[4*'i+2], 0)
                end if
                $stVelocity := %n[4*'i+3]
                ## NOTE: $mark does not need to be adjusted by $loop*$seq_length_ticks b/c currently we only
                ## support velocity adjustment for single shots (i.e. not using master clock), so $loop is 
                ## always 0
                if ('mark < 'seq_decay_time)
                    $stVelocity := $stVelocity + real_to_int('seq_delta ...
                                                            * int_to_real('seq_decay_time - 'mark) ...
                                                            / int_to_real('seq_decay_time))
                    if ($stVelocity < 1)
                        $stVelocity := 1
                    end if
                    if ($stVelocity  > 127)
                        $stVelocity := 127 
                    end if                           
                end if  
                set_midi('channel, %n[4*'i+1], %n[4*'i+2], $stVelocity)
                %offs['channel*128 + %n[4*'i+2]] := [[CG]]
                [[INCREMENT_PLAYHEAD]]

            case $MIDI_COMMAND_NOTE_OFF
                if ('is_endgroup = 0)
                    set_midi('channel, %n[4*'i+1], %n[4*'i+2], 0)
                    %offs['channel*128 + %n[4*'i+2]] := $NO_CHOKE_GROUP
                end if
                [[INCREMENT_PLAYHEAD]]

            case $MIDI_COMMAND_CC
                set_midi('channel, %n[4*'i+1], %n[4*'i+2], %n[4*'i+3])
                %ccs['channel*128 + %n[4*'i+2]] := [[CG]]
                [[INCREMENT_PLAYHEAD]]

            case $MIDI_COMMAND_PITCH_BEND
                set_midi('channel, %n[4*'i+1], %n[4*'i+2], %n[4*'i+3])
                %pitch_bend['channel] := [[CG]]
                [[INCREMENT_PLAYHEAD]]

            case $CUSTOM_COMMAND_CYCLE
                inc('loop)
                'i := 'start                 
                if ('i >= 'end)
                    'state := $PLAYER_DONE
                end if

            case $CUSTOM_COMMAND_ENDGROUP
                if ('trigger_held >= 0)
                    'is_endgroup := 1
                end if
        end select`,
}

var stz_MIDI_PLAYER_INITIALIZE_LATTICE = Stanza{
	name: "MIDI_PLAYER_INITIALIZE_LATTICE",
	body: `
        case [[CG]]
            'callback := $NI_CALLBACK_ID
            while (1 = 1)
                if ('state = $PLAYER_QUEUE or 'state = $PLAYER_CLEAR)  
                    [[CLEAR_STATE_BODY]]
                end if

                if ('state = $PLAYER_QUEUE)  
                    [[QUEUE_STATE_BODY]]
                end if

                if ('state = $PLAYER_RUNNING)  
                    [[RUN_WAIT_BODY]]
                end if

                if ('state = $PLAYER_RUNNING)  
                    [[PLAY_EVENT_BODY]]
                end if

                while ('state = $PLAYER_DONE)
                    ## Player waits until new sequence is selected
                    wait($long_time)
                end while
            end while`,
	procedure: func(ex *Expand, text string) {
		needCg := map[int]bool{}
		for _, slot := range ex.slots {
			if slot.meta.nd_NonDynamic {
				continue
			}
			needCg[slot.meta.cg_ChokeGroup] = true
		}
		cgs := []int{}
		for k := range needCg {
			cgs = append(cgs, k)
		}
		sort.Ints(cgs)
		for cg := range cgs {
			ex.pushStab("CG", asStr(cg))
			ex.expand(text)
			ex.popStab()
		}
	},
}

var stz_END_OF_SCRIPT_METADATA = Stanza{
	name: "END_OF_SCRIPT_METADATA",
	body: `
            pwd  = [[PWD]],
            time = [[TIME]]`,
	procedure: func(ex *Expand, text string) {
		pwd := backtick("pwd")
		time := backtick(`date +"%s %s"`, "%F", "%T")
		ex.pushStab(
			"PWD", pwd[0],
			"TIME", time[0],
		)
		ex.expand(text)
		ex.popStab()
	},
}

var stz_EMIT_SCRIPT = Stanza{
	name:    "EMIT_SCRIPT",
	comment: "Top level emit routine",
	body: `
        on init
            declare const $OO := $MIDI_COMMAND_NOTE_ON
            declare const $XX := $MIDI_COMMAND_NOTE_OFF
            declare const $PP := $MIDI_COMMAND_PITCH_BEND
            declare const $CC := $MIDI_COMMAND_CC

            declare const $PLAYER_QUEUE   := 1
            declare const $PLAYER_DONE    := 2
            declare const $PLAYER_RUNNING := 3
            declare const $PLAYER_CLEAR   := 4

            declare const $CUSTOM_COMMAND_ENDGROUP := [[CUSTOM_COMMAND_ENDGROUP]]
            declare const $CUSTOM_COMMAND_CYCLE    := [[CUSTOM_COMMAND_CYCLE]]
            declare const $NO_CHOKE_GROUP          := -1
            declare const $NON_DYNAMIC_CHOKE_GROUP := 20
            declare $long_time                     := 1000*1000*1000
            declare %n[[[N_MIDI]]] := ( ...
                [[MIDI_ARRAY]]
            )
            declare $master_clock_start := 0

            declare %offs[128*16]          := ($NO_CHOKE_GROUP)
            declare %ccs[128*16]           := ($NO_CHOKE_GROUP)
            declare %background_cc[128*16] := (0)
            declare %pitch_bend[16]        := ($NO_CHOKE_GROUP)
            declare %use_master_clock[16]  := (0)
            declare %callback[16]          := (0)
            declare %state[16]             := ($PLAYER_DONE)
            declare $bend_center_msb := msb(8192) {Should be 64}
            declare $bend_center_lsb := lsb(8192) {Should be 0}
            declare $stDoinit        := 0
            declare $stJ
            declare $stVelocity
            declare $stVelocityDelta
            declare $stIndex

            ## Midi player variables
            declare %previous_channel[16] := (-1)
            declare %channel[16]
            declare %mark[16]
            declare %ticks[16]
            declare %loop[16]
            declare %needsinit[16] := (1)
            declare %seq_length_ticks[16]
            declare %i[16]
            declare %start_time[16]
            declare %end[16]
            declare %start[16]
            declare ?seq_delta[16]
            declare %seq_decay_time[16]
            declare %trigger_held[16]
            declare %is_endgroup[16]

            [[DECLARE_CONSOLE]]

            message("")
        end on

        on midi_in
            ignore_midi
            $stIndex := $MIDI_CHANNEL*128 + $MIDI_BYTE_1
            if ($MIDI_COMMAND = $MIDI_COMMAND_NOTE_ON and $MIDI_BYTE_1 > 0)
                select ($stIndex)
                case -1
                    ## Empty
                [[NOTE_ON_LATTICE]]
                end select

                select ($stDoinit)
                case -1
                    ## Empty
                [[MIDI_PLAYER_INITIALIZE_LATTICE]]
                end select
            end if

            if ($MIDI_COMMAND = $MIDI_COMMAND_NOTE_OFF or ($MIDI_COMMAND = $MIDI_COMMAND_NOTE_ON and $MIDI_BYTE_1 = 0))
                select ($stIndex)
                case -1
                    ## Empty
                [[NOTE_OFF_LATTICE]]
                end select
            end if
        end on

        ## *** META ***
        {
        [[END_OF_SCRIPT_METADATA]]
        }`,

	procedure: func(ex *Expand, text string) {
		n_midi := 0
		for _, slot := range ex.slots {
			if slot.meta.nd_NonDynamic {
				continue
			}
			n_midi += len(slot.midi)
		}
		ex.pushStab(
			"N_MIDI", asStr(4*n_midi),
			"CUSTOM_COMMAND_CYCLE", asStr(CUSTOM_COMMAND_CYCLE),
			"CUSTOM_COMMAND_ENDGROUP", asStr(CUSTOM_COMMAND_ENDGROUP))
		ex.expand(text)
		ex.popStab()
	},
}

//
//
//

var MidiCsvPath = os.Getenv("HOME") + "/CeeGuitar/midicsv-1.1/midicsv"
var CsvMidiPath = os.Getenv("HOME") + "/CeeGuitar/midicsv-1.1/csvmidi"

const (
	LogFile = "log.txt"

	// Maschines transpose knob only supports -48 semitones -> +48 semitones. So when
	// we import we only consider this note range. Compile supports all notes.
	MinImportNote = 12
	MaxImportNote = 108

	CUSTOM_COMMAND_ENDGROUP = 256
	CUSTOM_COMMAND_CYCLE    = 257
)

type EventType int

const (
	Unknown_c EventType = iota
	Note_on_c
	Note_off_c
	Control_c
	Pitch_bend_c

	Endgroup_c
	Cycle_c
)

func (et EventType) String() string {
	switch et {
	case Unknown_c:
		return "Unknown_c"
	case Note_on_c:
		return "Note_on_c"
	case Note_off_c:
		return "Note_off_c"
	case Control_c:
		return "Control_c"
	case Pitch_bend_c:
		return "Pitch_bend_c"
	case Endgroup_c:
		return "Endgroup_c"
	case Cycle_c:
		return "Cycle_c"
	}

	die("Unknown EventType %d", et)
	return ""
}

func (et EventType) ksbFormat() string {
	switch et {
	case Unknown_c:
		die("ksbFormat cannot produce Unknown_c in ksbFormat")
	case Note_on_c:
		return "$OO"
	case Note_off_c:
		return "$XX"
	case Control_c:
		return "$CC"
	case Pitch_bend_c:
		return "$PP"
	case Endgroup_c:
		return asStr(CUSTOM_COMMAND_ENDGROUP)
	case Cycle_c:
		return asStr(CUSTOM_COMMAND_CYCLE)
	}
	die("ksbFormat called on unknown eventype %d", et)
	return ""
}

func _initLetters() []string {
	ret := []string{}
	for i := 0; i < 26; i++ {
		ret = append(ret, fmt.Sprintf("%c", 'A'+i))
	}
	return ret
}

var Letters []string = _initLetters()

//
// S L O T   M E T A D A T A
//
type NullableInteger struct {
	value int
	valid bool
}

type Meta struct {
	sn_SliceNote NullableInteger

	ic_InputChannel  int
	cg_ChokeGroup    int
	ch_OutputChannel int

	// Remember ln_Length is the specified length. You usually want to use length_ticks
	ln_Length int

	vr_VelocityRadius int

	// On input vd is measured in quarter notes, but we store it as ticks
	vd_VelocityDecay int

	sl_Slice NullableInteger

	em_Empty           bool
	me_MarkEndGroup    bool
	nd_NonDynamic      bool
	um_UseMasterClock  bool
	sm_StopMasterClock bool
	as_AllStop         bool

	tg_Tag string
}

var defaultMeta = Meta{
	vr_VelocityRadius: 10,
	vd_VelocityDecay:  2,
}

type ConfigField struct {
	shortName string
	longName  string
	arg       string
	desc      string
}

var KnownConfigFields = []ConfigField{
	// XXX: Currently this is documentation only: work should be done to make sure that it is always in sync with
	// scan_filename_meta
	// Import options
	ConfigField{
		shortName: "sn",
		longName:  "SliceNote",
		arg:       "Integer specifies which note to consider the split note",
	},
	ConfigField{
		shortName: "ic",
		longName:  "InputChannel",
		arg:       "Integer specifies which input channel the midi file MUST be imported into",
	},

	// Compile options
	ConfigField{
		shortName: "cg",
		longName:  "ChokeGroup",
		arg:       "Integer specifies choke group (0-15)",
	},
	ConfigField{
		shortName: "ch",
		longName:  "OutputChannel",
		arg:       "Integer specifies output channel (0-15)",
	},
	ConfigField{
		shortName: "ln",
		longName:  "Length",
		arg:       "Integer describes length (in quarter notes) of the sequence",
	},
	ConfigField{
		shortName: "sl",
		longName:  "Slice",
		arg:       "Integer specifies where to slice a loop (given in ticks).",
		desc:      "Note that this is just a tag, compile doesn't do anything with it",
	},
	ConfigField{
		shortName: "em",
		longName:  "Empty",
		desc:      "Note that this is just a tag, compile doesn't do anything with it",
	},
	ConfigField{
		shortName: "me",
		longName:  "MarkEndgroup",
		desc:      "Mark the sequence to use the endgroup feature",
	},
	ConfigField{
		shortName: "nd",
		longName:  "NonDynamic",
		desc:      "Means that time dependence of midi sequence is ignored",
	},
	ConfigField{
		shortName: "um",
		longName:  "UseMasterClock",
		desc:      "Means that the sequence should use the master clock",
	},
	ConfigField{
		shortName: "sm",
		longName:  "StopMasterClock",
		desc:      "Means that before you start the sequence stop the master clock. NOTE: sm DOES NOT imply um",
	},
	ConfigField{
		shortName: "as",
		longName:  "AllStop",
		desc:      "Stop all sequences.",
	},
	ConfigField{
		shortName: "vr",
		longName:  "VelocityRadius",
		arg:       "An integer radius with value between 1 and 127",
		desc:      "Set the velocity sensitivity of loop",
	},
	ConfigField{
		shortName: "vd",
		longName:  "VelocityDecay",
		arg:       "And integer specifiying the number of quarter notes to decay the velocity delta",
		desc:      "Set the rate that the velocity sensitivity peeters",
	},
	ConfigField{
		shortName: "tg",
		longName:  "Tag",
		arg:       "Text that helps identify the sequence",
		desc:      "Just allows a hunk of text to id sequence",
	},
}

func scan_filename_meta(filename string) Meta {
	if filename[len(filename)-4:] == ".mid" {
		filename = filename[:len(filename)-4]
	}

	parseInt := func(full, text string, min, max int) int {
		unless(text != "", "Config %s requires an argument", full)
		i64, err := strconv.ParseInt(text, 10, 32)
		unless(err == nil, "Failed to parse argument in %s: %v", full, err)
		i := int(i64)
		unless(i >= min, "Integer argument in %s is too small", full)
		unless(i <= max, "Integer argument in %s is too large", full)
		return i
	}

	parseExists := func(full, arg string) bool {
		unless(arg == "", "No argument allowed for %s", full)
		return true
	}

	fields := strings.Split(filename, "_")
	if len(fields) == 0 {
		return Meta{}
	}

	fields = fields[1:]
	assigned := map[string]bool{}
	var meta Meta = defaultMeta
	for _, f := range fields {
		if f == "" {
			continue
		}
		unless(len(f) >= 2, "Field too short %s: %s", f, filename)
		name := f[:2]
		value := f[2:]
		assigned[name] = true
		switch name {
		case "sn":
			meta.sn_SliceNote = NullableInteger{value: noteFromText(value), valid: true}
		case "ic":
			meta.ic_InputChannel = parseInt(f, value, 0, 15)
		case "cg":
			meta.cg_ChokeGroup = parseInt(f, value, 0, 15)
		case "ch":
			meta.ch_OutputChannel = parseInt(f, value, 0, 15)
		case "ln":
			meta.ln_Length = parseInt(f, value, 0, 1000)
		case "sl":
			meta.sl_Slice = NullableInteger{value: parseInt(f, value, 0, 1000000), valid: true}
		case "em":
			meta.em_Empty = parseExists(f, value)
		case "me":
			meta.me_MarkEndGroup = parseExists(f, value)
		case "nd":
			meta.nd_NonDynamic = parseExists(f, value)
		case "um":
			meta.um_UseMasterClock = parseExists(f, value)
		case "sm":
			meta.sm_StopMasterClock = parseExists(f, value)
		case "as":
			meta.as_AllStop = parseExists(f, value)
		case "vr":
			meta.vr_VelocityRadius = parseInt(f, value, 0, 127)
		case "vd":
			// vd is measured in quarter notes, but we convert it to ticks right away
			meta.vd_VelocityDecay = 960 * parseInt(f, value, 0, 10000)
		case "tg":
			meta.tg_Tag = value
		default:
			die("Unknown config variable in %q", f)
		}
	}
	unless(!assigned["me"] || !assigned["um"], "Configuration can't have me and um")
	unless(!assigned["sn"] || assigned["ln"], "Configuration that uses sn MUST use ln")
	unless(!(assigned["um"] && (assigned["sn"] || assigned["sl"])), "Configuration that uses um CANNOT use sn or sl")
	return meta
}

//
// S E Q U E N C E    S L O T S
//

// These two regexp MUST be consistent
var reParseChanNote = regexp.MustCompile(`^...\.(\d{1,2})\.(\d{1,3})`)
var reIdentifySlots = regexp.MustCompile(`^[A-Z][A-Z][A-Z]\.\d{1,2}\.\d{1,3}[-+]\d{1,2}`)

// readSequenceSlotsFromCwd returns the slots found in the current directory, and the non-slot midi files.
func readSequenceSlotsFromCwd() ([]string, []string) {
	entries, err := ioutil.ReadDir(".")
	unless(err == nil, "Failed to ReadDir: %v", err)
	var slots, notSlots []string
	for _, ioent := range entries {
		f := ioent.Name()
		if ioent.IsDir() || !strings.HasSuffix(f, ".mid") {
			continue
		}

		if reIdentifySlots.MatchString(f) {
			slots = append(slots, f)
		} else {
			notSlots = append(notSlots, f)
		}
	}
	sort.Strings(slots)
	sort.Strings(notSlots)
	return slots, notSlots
}

type MidiEvent struct {
	track   int
	time    int
	typ     EventType
	channel int
	bb1     int
	bb2     int

	// Length is only defined for Note_on_c, in which case it is the number of ticks to this events Note_off_c
	length int

	// Only defined for Note_on_c and Note_off_c. For Note_on_c it points at the associated Note_off_c, and vice versa
	// for Note_off_c. NOTE: after partnerIndex is set the length of the source []MidiEvent MUST NOT CHANGE.
	partnerIndex int
}

type SequenceSlot struct {
	path     string
	filename string

	midi []MidiEvent
	meta Meta

	length_ticks int

	input_note    int
	input_channel int
	seq_number    int

	sequence_start int
	sequence_end   int
}

func (ss *SequenceSlot) debugPrintMidi() {
	for _, m := range ss.midi {
		fmt.Printf("%15.2f%15s%15d%15d\n", float32(m.time)/960.0, m.typ.String(), m.bb1, m.bb2)
	}
}

func (ss *SequenceSlot) markEndgroup() {
	if len(ss.midi) == 0 {
		return
	}

	lastIndex := -1
	for i := len(ss.midi) - 1; i >= 0; i-- {
		m := ss.midi[i]
		if m.typ == Note_on_c {
			lastIndex = i
			break
		}
	}
	if lastIndex < 0 {
		// This is the case where there are NO notes. In this case, the endgroup starts
		// when the sequence is launched
		lastIndex = 0
	}

	nwmid := make([]MidiEvent, len(ss.midi)+1)
	copy(nwmid, ss.midi[:lastIndex+1]) //include the lastIndex note
	nwmid[lastIndex+1] = MidiEvent{
		typ:  Endgroup_c,
		time: ss.midi[lastIndex].time,
	}
	copy(nwmid[lastIndex+2:], ss.midi[lastIndex+1:])
	ss.midi = nwmid
}

func (ss *SequenceSlot) markCycle() {
	lt := ss.length_ticks
	unless(ss.midi[len(ss.midi)-1].time <= lt, "INTERNAL ERROR: length_ticks corrupted")
	ss.midi = append(ss.midi, MidiEvent{
		time: lt,
		typ:  Cycle_c,
	})
}

func (ss *SequenceSlot) markNoteLengthAndPartnerIndex() {
	// NOTE: anytime you change the length of the midi sequence, you should call this function
	pendingNoteOns := map[int]int{}
	for i, ev := range ss.midi {
		switch ev.typ {
		case Note_on_c:
			_, found := pendingNoteOns[ev.bb1]
			unless(!found, "Found two Note_on_c in a row with no Note_off_c for note %d", ev.bb1)
			pendingNoteOns[ev.bb1] = i

		case Note_off_c:
			index, found := pendingNoteOns[ev.bb1]
			unless(found, "Found an unparied Note_off_c for note %d", ev.bb1)
			ss.midi[index].length = ev.time - ss.midi[index].time
			ss.midi[i].partnerIndex = index
			ss.midi[index].partnerIndex = i
			delete(pendingNoteOns, ev.bb1)
		}
	}

}

func (ss *SequenceSlot) findSliceTimes(note int) []int {
	sliceTimes := []int{}
	for _, m := range ss.midi {
		if m.typ == Note_on_c && m.bb1 == note {
			sliceTimes = append(sliceTimes, m.time)
		}
	}
	return sliceTimes
}

func (ss *SequenceSlot) filterSliceNote(note int) {
	midi := []MidiEvent{}
	for _, m := range ss.midi {
		switch m.typ {
		case Note_on_c:
			if m.bb1 != note {
				midi = append(midi, m)
			}
		case Note_off_c:
			if m.bb1 != note {
				midi = append(midi, m)
			}
		default:
			midi = append(midi, m)
		}
	}
	ss.midi = midi
	ss.markNoteLengthAndPartnerIndex()
}

func parseForNoteChannel(filename string) (int, int, int) {
	// Parse the input_channel, intput_note out of the file name
	match := reParseChanNote.FindStringSubmatch(filename)
	unless(match != nil, "Failed to parse channel, note out of filename %s", filename)
	i64, err := strconv.ParseInt(match[1], 10, 32)
	unless(err == nil, "Failed to parse channel: %v", err)
	unless(i64 >= 0 && i64 < 16, "Bad channel %d", int(i64))
	input_channel := int(i64)

	i64, err = strconv.ParseInt(match[2], 10, 32)
	unless(err == nil, "Failed to parse input_note: %v", err)
	unless(i64 >= 0 && i64 < 128, "Bad input_note")
	input_note := int(i64)
	seq_number := 128*input_channel + input_note

	return input_channel, input_note, seq_number
}

func (ss *SequenceSlot) setIndexFromFilename() {
	// Parse the input_channel, intput_note out of the file name
	ss.input_channel, ss.input_note, ss.seq_number = parseForNoteChannel(ss.filename)
}

func (ss *SequenceSlot) writeToFile(filename string) {
	if ss.meta.em_Empty {
		fd, err := os.Create(filename)
		fd.Close()
		unless(err == nil, "os.Open failed %v", err)
		return
	}

	const writeChannel = 0
	fd, err := ioutil.TempFile("", "writeToFile")
	unless(err == nil, "TempFile errored %v", err)
	fmt.Fprintln(fd, "0, 0, Header, 1, 1, 960")
	fmt.Fprintln(fd, "1, 0, Start_track")
	latestTime := 0
	if len(ss.midi) > 0 {
		for _, m := range ss.midi {
			fmt.Fprintf(fd, "1, %d, %s, %d, %d, %d\n", m.time, m.typ.String(), writeChannel, m.bb1, m.bb2)
		}
		latestTime = ss.midi[len(ss.midi)-1].time
	}
	fmt.Fprintf(fd, "1, %d, End_track\n", latestTime)
	fmt.Fprintf(fd, "0, 0, End_of_file")
	fd.Close()
	defer func() {
		// os.Remove(fd.Name())
		// if e := recover(); e != nil {
		// 	os.Remove(filename)
		// 	panic(e)
		// }
	}()
	run(fmt.Sprintf("%s %s > %s", CsvMidiPath, fd.Name(), filename))
}

func makeSequenceSlot(path string) SequenceSlot {
	ss := fetchSequenceSlot(path)
	ss.setIndexFromFilename()
	return ss
}

func fetchSequenceSlot(path string) SequenceSlot {
	var ss SequenceSlot
	ss.path = path
	ss.filename = filepath.Base(path)
	if strings.HasSuffix(ss.filename, ".mid") {
		ss.filename = ss.filename[:len(ss.filename)-4]
	}
	_, err := os.Stat(ss.path)
	unless(!os.IsNotExist(err), "File %s does not exists", ss.path)

	TPQN := 960
	ss.midi = []MidiEvent{}
	ss.meta = scan_filename_meta(ss.filename)
	if !ss.meta.em_Empty {
		for lineIndex, line := range backtick(fmt.Sprintf("%s %s", MidiCsvPath, ss.path)) {
			parseInt := func(text string) int {
				i64, err := strconv.ParseInt(text, 10, 32)
				unless(err == nil, "Read failed to parse integer %q at line %d", text, lineIndex)
				return int(i64)
			}

			f := strings.Split(line, ",")
			for i := range f {
				f[i] = strings.TrimSpace(f[i])
			}

			unless(len(f) >= 3, "Bad midi count [1]")
			ev := MidiEvent{}
			ev.track = parseInt(f[0])
			ev.time = parseInt(f[1])
			command := f[2]

			if TPQN != 960 {
				ev.time = int((float32(ev.time) / float32(TPQN)) * 960.0)
			}

			switch command {
			case "Note_on_c":
				unless(len(f) >= 6, "Bad midi count [2]")
				ev.typ = Note_on_c
				ev.channel = parseInt(f[3])
				ev.bb1 = parseInt(f[4])
				ev.bb2 = parseInt(f[5])
				if ev.bb2 == 0 {
					ev.typ = Note_off_c
				}
				ss.midi = append(ss.midi, ev)

			case "Note_off_c":
				unless(len(f) >= 6, "Bad midi count [3]")
				ev.typ = Note_off_c
				ev.channel = parseInt(f[3])
				ev.bb1 = parseInt(f[4])
				ev.bb2 = parseInt(f[5])
				ss.midi = append(ss.midi, ev)

			case "Control_c":
				unless(len(f) >= 6, "Bad midi count [4]")
				ev.typ = Control_c
				ev.channel = parseInt(f[3])
				ev.bb1 = parseInt(f[4])
				ev.bb2 = parseInt(f[5])
				ss.midi = append(ss.midi, ev)
			case "Pitch_bend_c":
				unless(len(f) >= 5, "Bad midi count [5]")
				ev.typ = Pitch_bend_c
				ev.channel = parseInt(f[3])
				value := parseInt(f[4])
				ev.bb1 = ((value >> 7) & 0x7F) // MSB
				ev.bb2 = value & 0x7F          // LSB
				ss.midi = append(ss.midi, ev)
			case "Header":
				// This is always the first row
				unless(len(f) >= 6, "Bad midi count [6]")
				TPQN = parseInt(f[5])
			}
		}
	}

	// compute sequence length
	if len(ss.midi) > 0 {
		lastTime := ss.midi[len(ss.midi)-1].time
		if ss.meta.ln_Length == 0 {
			ss.length_ticks = lastTime
		} else {
			specifiedLength := 960 * ss.meta.ln_Length
			unless(specifiedLength >= lastTime,
				"The ln (Length) specifier MUST be longer than the last time of the sequence: [%d < %d]",
				specifiedLength, lastTime)

			ss.length_ticks = specifiedLength
		}
	}

	// Mark endgroup and cycle
	if ss.meta.me_MarkEndGroup {
		ss.markEndgroup()
	}
	if ss.meta.um_UseMasterClock {
		ss.markCycle()
	}

	// Compute note-length, and partner-index
	ss.markNoteLengthAndPartnerIndex()

	// A little bit of time sanity
	currT := 0
	for i, m := range ss.midi {
		unless(currT <= m.time, "MIDI-order time sanity FAILED! at event %d", i)
		currT = m.time
	}

	return ss
}

type SlotCoordinates struct {
	channel    int
	note       int
	seq_number int
}

type SlotIndex struct {
	filenames []string
}

func (si *SlotIndex) index(slot string) {
	_, _, index := parseForNoteChannel(slot)
	unless(si.filenames[index] == "", "SlotIndex collision between %q and %q", slot, si.filenames[index])
	si.filenames[index] = slot
}

func (si *SlotIndex) findNConescutiveFreeSlots(n int) []SlotCoordinates {
	index := -1
	for i := 0; i < len(si.filenames); i++ {
		count := 0
		for j := 0; j < n && i+j < len(si.filenames); j++ {
			if si.filenames[i+j] != "" {
				break
			}
			count++
		}
		if count == n {
			index = i
			break
		}
	}
	unless(index >= 0, "Failed to find %d open slots", n)
	ret := []SlotCoordinates{}
	for j := 0; j < n; j++ {
		channel := (index + j) / 128
		note := (index + j) % 128
		ret = append(ret, SlotCoordinates{channel: channel, note: note, seq_number: index + j})
	}

	return ret
}

func newSlotIndex(existingSlots []string) *SlotIndex {
	si := &SlotIndex{filenames: make([]string, 128*16)}
	for _, slot := range existingSlots {
		si.index(slot)
	}
	return si
}

var reAllDigits = regexp.MustCompile(`^\d+$`)
var reLooksLikeNote = regexp.MustCompile(`([a-gA-G][\#sb]?)([-+]?\d)`)
var reChangeSToSharp = regexp.MustCompile(`S`)
var NoteSymbols = []string{"C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"}
var NoteSymbolsFlat = []string{"B#", "Db", "", "Eb", "Fb", "E#", "Gb", "", "Ab", "", "Bb", "Cb"}

func noteFromText(text string) int {
	if reAllDigits.MatchString(text) {
		i64, err := strconv.ParseInt(text, 10, 32)
		unless(err != nil, "Failed to turn slice note into integer %q: %v", text, err)
		return int(i64)
	}
	match := reLooksLikeNote.FindStringSubmatch(text)
	unless(match != nil, "Bad argument to sn %q", text)

	noteString := match[1]
	noteString = strings.ToUpper(noteString[0:1]) + noteString[1:]
	if len(noteString) > 1 && noteString[1] == 's' {
		noteString = noteString[0:1] + "#" + noteString[2:]
	}

	offset := -1
	for index, symb := range NoteSymbols {
		if symb == noteString {
			offset = index
			break
		}
	}
	if offset < 0 {
		for index, symb := range NoteSymbolsFlat {
			if symb == noteString {
				offset = index
				break
			}
		}
	}
	unless(offset >= 0, "Bad note letter %s in %q", noteString, text)

	octaveString := match[2]
	if octaveString[0] == '+' {
		octaveString = octaveString[1:]
	}
	n, err := strconv.ParseInt(octaveString, 10, 32)
	unless(err == nil, "Failed to parse octave integer %s of %q: %v", octaveString, text, err)
	note := (int(n)+2)*12 + offset
	unless(note >= 0 && note < 128, "Note %q (%d) is out of bounds", text, note)

	return note
}

func nameMidiFile(channel, note int, opts string) string {
	// Note I'm solving a division algorithm of sorts for index = 26^2*i + 26*j + k where 0 <= i,j,k < 26. This is
	// just expressing the index in base 26.
	index := channel*128 + note
	i := index / (26 * 26)
	j := (index - (26*26)*i) / 26
	k := (index - (26*26)*i - 26*j) % 26

	dok := func(x int) bool {
		return x >= 0 && x < 26
	}
	unless(dok(i) && dok(j) && dok(k), "INTERNAL ERROR: failed nameMidiFile sanity check")

	lex := fmt.Sprintf("%s%s%s", Letters[i], Letters[j], Letters[k])
	transpose := fmt.Sprintf("%+d", (note - 60))
	nfile := fmt.Sprintf("%s.%d.%d%s%s.mid", lex, channel, note, transpose, opts)
	return nfile
}

func rotateSequence(inputSlot SequenceSlot, sliceTime int) SequenceSlot {
	unless(sliceTime <= inputSlot.length_ticks, "INTERNAL ERROR: sliceTime too large")

	// double the midi sequence to find the sliceTime
	midi := append([]MidiEvent{}, inputSlot.midi...)
	for _, m := range inputSlot.midi {
		m.time += inputSlot.length_ticks
		midi = append(midi, m)
	}

	for _, m := range midi {
		if m.time >= sliceTime && m.typ == Note_on_c {
			sliceTime = m.time
			break
		}
	}
	for sliceTime > inputSlot.length_ticks {
		sliceTime -= inputSlot.length_ticks
	}
	unless(sliceTime >= 0 && sliceTime <= inputSlot.length_ticks, "INTERNAL ERROR")

	midi = []MidiEvent{}
	for i := 0; i < len(inputSlot.midi); i++ {
		m := inputSlot.midi[i]
		if m.time < sliceTime {
			continue
		}
		if m.typ == Note_off_c && inputSlot.midi[m.partnerIndex].time < sliceTime {
			// This is a Note_off who is straddling the sliceTime
			continue
		}

		m.time -= sliceTime
		midi = append(midi, m)
	}

	neededNoteOffs := []int{}
	for _, m := range inputSlot.midi {
		if m.time >= sliceTime {
			break
		}
		if m.typ == Note_on_c && inputSlot.midi[m.partnerIndex].time >= sliceTime {
			// This Note_on_c and his partnerIndex Note_off_c are straddling the sliceTime
			neededNoteOffs = append(neededNoteOffs, m.partnerIndex)
		}

		m.time += inputSlot.length_ticks - sliceTime
		midi = append(midi, m)
	}
	sort.Ints(neededNoteOffs)
	for _, index := range neededNoteOffs {
		m := inputSlot.midi[index]
		m.time += inputSlot.length_ticks - sliceTime
		midi = append(midi, m)
	}

	outputSlot := inputSlot
	outputSlot.midi = midi
	outputSlot.meta.sl_Slice = NullableInteger{valid: true, value: sliceTime}
	outputSlot.markNoteLengthAndPartnerIndex()
	return outputSlot
}

func main_compile() {
	slotFiles, _ := readSequenceSlotsFromCwd()
	var sequenceSlots []SequenceSlot
	for _, slotFile := range slotFiles {
		sequenceSlots = append(sequenceSlots, makeSequenceSlot(slotFile))
	}

	// Fill in global sequence_start and sequence_end
	start := 0
	end := 0
	for index := range sequenceSlots {
		if sequenceSlots[index].meta.em_Empty || sequenceSlots[index].meta.nd_NonDynamic {
			continue
		}
		start = end
		end += len(sequenceSlots[index].midi)
		sequenceSlots[index].sequence_start = start
		sequenceSlots[index].sequence_end = end
	}

	ex := newDefaultExpand(sequenceSlots)
	ex.stanzaExpand("EMIT_SCRIPT", "")
}

var reOptionsInFilename = regexp.MustCompile(`(_[^\.]*)\.mid$`)
var reRemoveSL = regexp.MustCompile(`_sl[^_]+`)
var reRemoveSN = regexp.MustCompile(`_sn[^_]+`)
var reRemoveIC = regexp.MustCompile(`_ic[^_]+`)

func main_import() {
	slotFiles, nonSlotFiles := readSequenceSlotsFromCwd()
	abortList := []string{}
	workingFile := ""
	tryErr := try(func() {
		slotIndex := newSlotIndex(slotFiles)
		for _, file := range nonSlotFiles {
			workingFile = file
			newSlot := fetchSequenceSlot(file)
			slices := []int{0}
			if newSlot.meta.sn_SliceNote.valid {
				slices = newSlot.findSliceTimes(newSlot.meta.sn_SliceNote.value)
			}

			coords := slotIndex.findNConescutiveFreeSlots(len(slices))

			opts := ""
			match := reOptionsInFilename.FindStringSubmatch(file)
			if match != nil {
				opts = match[1]
			}
			opts = reRemoveSL.ReplaceAllString(opts, "")
			opts = reRemoveSN.ReplaceAllString(opts, "")
			opts = reRemoveIC.ReplaceAllString(opts, "")

			for i := range slices {
				zeroSlice := slices[i] == 0
				coord := coords[i]
				var _ = coord.seq_number
				options := opts
				if !zeroSlice {
					options += fmt.Sprintf("_sl%d", slices[i])
				}
				nfile := nameMidiFile(coord.channel, coord.note, options)
				abortList = append(abortList, nfile)
				slotIndex.index(nfile)
				cpy := newSlot
				if cpy.meta.sn_SliceNote.valid {
					cpy.filterSliceNote(cpy.meta.sn_SliceNote.value)
				}
				cpy = rotateSequence(cpy, slices[i])
				fmt.Printf("Writting %s\n", nfile)
				cpy.writeToFile(nfile)
			}
		}
	})
	if tryErr != nil {
		for _, f := range abortList {
			err := os.Remove(f)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Failed to remove file %q: %v\n", f, err)
			}
		}
		fmt.Fprintf(os.Stderr, "While working on %s: %+v\n", workingFile, tryErr)
	}

	// All of the nonSlotFiles have been copied over, so it's safe to delete them
	for _, f := range nonSlotFiles {
		err := os.Remove(f)
		unless(err == nil, "Failed to remove file %q: %v", f, err)
	}
}

func main() {
	// NOTE: all command line flags MUST come after the command. This way flags can be parsed in the main_* functions.
	args := os.Args[1:]
	unless(len(args) > 0, "Requires a command")
	switch args[0] {
	case "compile":
		main_compile()
	case "import":
		main_import()
	default:
		die("Unknown command %q", args[0])
	}
}
