package main

import (
	"bytes"
	"fmt"
	"os"
	"regexp"
	"strings"
	"testing"
)

var _ = fmt.Printf

func test_try(t *testing.T, unit func()) {
	err := try(unit)
	if err != nil {
		t.Fatalf("test_try: %v", err)
	}
}

func assert(t *testing.T, condition bool, format string, args ...interface{}) {
	if !condition {
		t.Errorf(format, args...)
	}
}

func assertFatal(t *testing.T, condition bool, format string, args ...interface{}) {
	if !condition {
		t.Fatalf(format, args...)
	}
}

var reWhiteSpace = regexp.MustCompile(`\s+`)

func whiteSpaceNormalForm(input string) []string {
	lines := strings.Split(input, "\n")
	for len(lines) > 0 && lines[0] == "" {
		lines = lines[1:]
	}
	for len(lines) > 0 && lines[len(lines)-1] == "" {
		lines = lines[:len(lines)-1]
	}
	for i := range lines {
		lines[i] = reWhiteSpace.ReplaceAllString(lines[i], "")
	}
	return lines
}

func whiteFreeCompare(t *testing.T, got, exp string) {
	gotLines := whiteSpaceNormalForm(got)
	expLines := whiteSpaceNormalForm(exp)
	assert(t, len(gotLines) == len(expLines), "whiteFreeCompare length mismatch: got %d, exp %d",
		len(gotLines), len(expLines))
	for i := 0; i < len(gotLines) && i < len(expLines); i++ {
		assert(t, gotLines[i] == expLines[i], "whiteFreeCompare[line %d] mismatch: got %q, exp %q", i,
			gotLines[i], expLines[i])
	}
	return
}

func TestBacktick(t *testing.T) {
	lines := backtick("echo pete is the bomb")
	assertFatal(t, len(lines) == 1, "Length mismatch got %d, exp %d", len(lines), 1)
	assert(t, lines[0] == "pete is the bomb", "echo content")

	exp := []string{"1.txt", "2.txt", "3.txt"}
	lines = backtick("cd testdata/backtick && ls *.txt")
	assertFatal(t, len(lines) == 3, "Number of files mismatch, got %d, exp %d", len(lines), 3)
	for i := range lines {
		assert(t, lines[i] == exp[i], "File mismatch: got %s, exp %s", lines[i], exp[i])
	}
}

func TestScanFilenameMeta(t *testing.T) {
	filename := "foo_em_vr41_cg2_ch5_ln16_sl19800_tgALongAssTag.mid"
	var meta Meta
	test_try(t, func() {
		meta = scan_filename_meta(filename)
	})

	chk := func(condition bool, variable string) {
		if !condition {
			t.Errorf("Variable %s mismatch", variable)
		}
	}
	chk(meta.em_Empty, "em_Empty")
	chk(meta.vr_VelocityRadius == 41, "vr_VelocityRadius")
	chk(meta.cg_ChokeGroup == 2, "cg_ChokeGroup")
	chk(meta.ch_OutputChannel == 5, "ch_OutputChannel")
	chk(meta.ln_Length == 16, "ln_Length")
	chk(meta.sl_Slice.valid && meta.sl_Slice.value == 19800, "sl_Slice")
	chk(meta.tg_Tag == "ALongAssTag", "tg_Tag")
}

func TestReadSequenceSlotsFromCwd(t *testing.T) {
	cd := func(dir string) {
		err := os.Chdir(dir)
		assertFatal(t, err == nil, "Failed to chdir to %s", dir)
	}
	cd("testdata/readSequenceSlotsFromCwd")
	defer cd("../..")
	slots, notSlots := readSequenceSlotsFromCwd()
	expSlots := []string{"AAA.0.10-5.mid", "AAB.15.105+80.mid", "AAC.3.15-40.mid"}
	expNotSlots := []string{"x.mid", "y.mid", "z.mid"}
	assertFatal(t, len(expSlots) == len(slots), "expSlots wrong size; got %d, exp %d", len(slots), len(expSlots))
	assertFatal(t, len(expNotSlots) == len(notSlots), "expNotSlots wrong size; got %d, exp %d",
		len(notSlots), len(expNotSlots))
	for i := range expSlots {
		assert(t, expSlots[i] == slots[i], "Slots mismatch: got %s, exp %s", slots[i], expSlots[i])
	}
	for i := range expNotSlots {
		assert(t, expNotSlots[i] == notSlots[i], "NotSlots mismatch: got %s, exp %s", notSlots[i], expNotSlots[i])
	}

}

func expandIt(stab map[string]string) *Expand {
	return &Expand{stab: stab}
}

func TestExpandQuote(t *testing.T) {
	text := `
        text1
        ## More text
        text2`
	expect := `
        text1
        {# More text }
        text2`
	got := expandIt(nil).commentExpand(text)
	if expect != got {
		t.Fatalf("Mismatch: got %q, expected %q", got, expect)
	}
}

func TestExpandSymbols(t *testing.T) {
	text := `
        text1
        [[CG]]
        text2`
	expect := `
        text1
        foobar
        text2`
	got := expandIt(map[string]string{"CG": "foobar"}).symbolExpand(text)
	if expect != got {
		t.Fatalf("Mismatch: got %q, expected %q", got, expect)
	}
}

func TestExpandSingleQuoteVars(t *testing.T) {
	text := `
        text1
        'variable
        'seq_delta
        text2`
	expect := `
        text1
        %variable[[[CG]]]
        ?seq_delta[[[CG]]]
        text2`
	got := expandIt(nil).tickExpand(text)
	if expect != got {
		t.Fatalf("Mismatch: got %q, expected %q", got, expect)
	}
}

func TestExpandProper(t *testing.T) {
	stanzas := StanzaMap{
		"foo": Stanza{
			name: "foo",
			body: `
            a := 1
            b := 2
            c := 3`,
		},

		"bar": Stanza{
			name: "foo",
			body: `
            d := a*b [[PP]]
            [[baz]]
            e := d-c*b*a`,
		},

		"baz": Stanza{
			name: "foo",
			body: `
            [[MM]]
            z := 5`,
		},
	}

	stab := SymbolTable{
		"MM": "<an interesting expansion>",
		"PP": "<ha ha pee pee>",
	}

	input := `
        [[MM]] [[PP]]
               [[foo]]
        [[MM]]
              [[bar]]
        [[PP]]`

	var buffer bytes.Buffer
	ex := &Expand{stab: stab, stanzas: stanzas, output: &buffer}
	ex.expand(input)

	exp := `
        <an interesting expansion> <ha ha pee pee>
            a := 1
            b := 2
            c := 3
        <an interesting expansion>
            d := a*b <ha ha pee pee>
            <an interesting expansion>
            z := 5            
            e := d-c*b*a
        <ha ha pee pee>`
	whiteFreeCompare(t, buffer.String(), exp)
}

func TestExpandSpacing(t *testing.T) {
	// NOTE: this test DEPENDS ON WHITESPACE .... BEWARE
	stanzas := StanzaMap{
		"ONE": Stanza{
			name: "ONE",
			body: `                                     one := 1`,
		},

		"TWO": Stanza{
			name: "TWO",
			body: `
            TWO[1] := 1
`,
		},

		"THREE": Stanza{
			name: "THREE",
			body: `
                THREE[2] := 2`,
		},
	}

	stab := SymbolTable{}

	input := "    [[ONE]]\n" +
		"        [[TWO]]\n" +
		"    [[THREE]]\n" +
		"            [[ONE]]"

	exp := "    one := 1\n" +
		"        TWO[1] := 1\n" +
		"    THREE[2] := 2\n" +
		"            one := 1\n"

	var buffer bytes.Buffer
	ex := &Expand{stab: stab, stanzas: stanzas, output: &buffer}
	ex.expand(input)

	assert(t, buffer.String() == exp, "Mismatch\n***got***\n%s\n***exp***\n%s\n", buffer.String(), exp)
}

func createOn(t, n, v int) MidiEvent {
	return MidiEvent{
		time: t,
		typ:  Note_on_c,
		bb1:  n,
		bb2:  v,
	}
}

func createOff(t, n int) MidiEvent {
	return MidiEvent{
		time: t,
		typ:  Note_off_c,
		bb1:  n,
		bb2:  0,
	}
}

func createCC(t, cc, value int) MidiEvent {
	return MidiEvent{
		time: t,
		typ:  Control_c,
		bb1:  cc,
		bb2:  value,
	}
}

func midiEqual(t *testing.T, exp []MidiEvent, got []MidiEvent) {
	if len(exp) != len(got) {
		t.Fatalf("Midi length mismatch: exp %d, got %d", len(exp), len(got))
	}
	for i := range exp {
		if exp[i].typ != got[i].typ {
			t.Errorf("Midi type mismatch [%d]: exp %s, got %s", i, exp[i].typ.String(), got[i].typ.String())
			continue
		} else if exp[i].time != got[i].time {
			t.Errorf("Midi time mismatch [%d]: exp %v, got %v", i, exp[i].time, got[i].time)
		} else if exp[i].bb1 != got[i].bb1 {
			t.Errorf("Midi bb1 mismatch [%d]: exp %v, got %v", i, exp[i].bb1, got[i].bb1)
		} else if exp[i].bb2 != got[i].bb2 {
			t.Errorf("Midi bb2 mismatch [%d]: exp %v, got %v", i, exp[i].bb2, got[i].bb2)
		}
	}
}

func TestMakeSequenceSlot(t *testing.T) {
	got := makeSequenceSlot("testdata/make_sequence_slot/AAA.0.0-60.mid")
	exp := []MidiEvent{
		createOn(0, 91, 81),
		createOff(1920, 91),
		createOn(1920, 93, 82),
		createOff(3840, 93),
		createOn(3840, 89, 83),
		createOff(5760, 89),
		createOn(5760, 77, 84),
		createOff(7680, 77),
		createOn(7680, 84, 85),
		createOff(9600, 84),
	}
	midiEqual(t, exp, got.midi)
}

func TestMakeSequenceSlotEndgroup(t *testing.T) {
	got := makeSequenceSlot("testdata/make_sequence_slot/AAB.0.1-59_me.mid")
	exp := []MidiEvent{
		createOn(0, 91, 81),
		createOff(1920, 91),
		createOn(1920, 93, 82),
		createOff(3840, 93),
		createOn(3840, 89, 83),
		createOff(5760, 89),
		createOn(5760, 77, 84),
		createOff(7680, 77),
		createOn(7680, 84, 85),
		MidiEvent{
			time: 7680,
			typ:  Endgroup_c,
		},
		createOff(9600, 84),
	}
	midiEqual(t, exp, got.midi)
}

func TestMakeSequenceSlotCycle(t *testing.T) {
	got := makeSequenceSlot("testdata/make_sequence_slot/AAC.0.2-58_um_ln12.mid")
	exp := []MidiEvent{
		createOn(0, 91, 81),
		createOff(1920, 91),
		createOn(1920, 93, 82),
		createOff(3840, 93),
		createOn(3840, 89, 83),
		createOff(5760, 89),
		createOn(5760, 77, 84),
		createOff(7680, 77),
		createOn(7680, 84, 85),
		createOff(9600, 84),
		MidiEvent{
			time: 11520, // 12*960 == (number of quarter notes)*(960 ticks per quarter note)
			typ:  Cycle_c,
		},
	}
	midiEqual(t, exp, got.midi)
}

func TestMakeSequenceSlotHeaderTPQN(t *testing.T) {
	// This file has a TPQN of 96, this test verifies that it's adjusted to 960
	got := makeSequenceSlot("testdata/make_sequence_slot/AAD.0.3-57.mid")
	exp := []MidiEvent{
		createOn(0, 91, 81),
		createOff(1920, 91),
		createOn(1920, 93, 82),
		createOff(3840, 93),
		createOn(3840, 89, 83),
		createOff(5760, 89),
		createOn(5760, 77, 84),
		createOff(7680, 77),
		createOn(7680, 84, 85),
		createOff(9600, 84),
	}
	midiEqual(t, exp, got.midi)
}

func TestMakeSequenceSlotLength(t *testing.T) {
	// This file has several notes of various lengths, verify that the lengths are correctly computed.
	got := makeSequenceSlot("testdata/make_sequence_slot/AAE.0.4-56.mid")
	expLength := []int{
		960 / 2,
		960,
		2 * 960,
	}
	gotLength := []int{}
	for _, m := range got.midi {
		if m.typ == Note_on_c {
			gotLength = append(gotLength, m.length)
		}
	}

	assertFatal(t, len(expLength) == len(gotLength), "Length OF LENGTH mismatch: got %d, exp %d", len(gotLength), len(expLength))
	for i := range gotLength {
		assert(t, expLength[i] == gotLength[i], "Length mismatch: got %d, exp %d", gotLength[i], expLength[i])
	}
}

func TestNoteFromText(t *testing.T) {
	assert(t, noteFromText("C-2") == 0, "Failed %d == 0", noteFromText("C-2"))
	assert(t, noteFromText("A4") == 81, "Failed %d == 81", noteFromText("A4"))
	assert(t, noteFromText("A+4") == 81, "Failed %d == 81", noteFromText("A4"))
	// remember Cb-1 is a half a step down from C0, NOTE a half a step down from C-1
	assert(t, noteFromText("Cb-1") == 23, "Failed %d == 23", noteFromText("Cb-1"))
}

func TestSetIndexFromFilename(t *testing.T) {
	got := makeSequenceSlot("testdata/make_sequence_slot/XXX.1.14.mid")
	assert(t, got.input_note == 14, "Bad input note")
	assert(t, got.input_channel == 1, "Bad input channel")
	assert(t, got.seq_number == got.input_channel*128+got.input_note, "Bad seq_number")
}

func TestNameMidiFile(t *testing.T) {
	note := 105
	channel := 1
	opts := "_ic4_cg2"
	exp := "AIZ.1.105+45_ic4_cg2.mid"
	name := nameMidiFile(channel, note, opts)
	assert(t, name == exp, "Name mismatch: got %q, exp %q", name, exp)
}

func TestRotateSequence(t *testing.T) {
	seqSlot := makeSequenceSlot("testdata/make_sequence_slot/AAA.0.0-60.mid")
	seqLength := 9600
	input := []MidiEvent{
		createOn(0, 91, 81),
		createOff(1920, 91),
		createOn(1920, 93, 82),
		// seek 3500
		createCC(3700, 60, 60),
		createOff(3840, 93),
		// This is the selected note for 3500
		createOn(3840, 89, 83),
		createCC(4000, 61, 61),
		createOff(5760, 89),
		createOn(5760, 77, 84),
		createOff(7680, 77),
		createOn(7680, 84, 85),
		createOff(seqLength, 84),
	}
	seqSlot.midi = make([]MidiEvent, len(input))
	copy(seqSlot.midi, input)
	seqSlot.markNoteLengthAndPartnerIndex()
	sliceTime := 3500
	foundSliceTime := 3840
	seqSlot = rotateSequence(seqSlot, sliceTime)

	exp := []MidiEvent{}
	for i := 5; i < len(input); i++ {
		m := input[i]
		m.time -= foundSliceTime
		exp = append(exp, m)
	}
	for i := 0; i < 5; i++ {
		m := input[i]
		m.time += seqLength - foundSliceTime
		exp = append(exp, m)
	}

	midiEqual(t, exp, seqSlot.midi)
}

func TestRotateSequenceStraddle(t *testing.T) {
	seqSlot := makeSequenceSlot("testdata/make_sequence_slot/AAA.0.0-60.mid")
	seqLength := 9600
	input := []MidiEvent{
		createOn(0, 91, 81),
		createOff(1920, 91),
		createOn(1920, 93, 82), // This note off is after the slice at 3840
		// seek 3500
		// This is the selected note for 3500
		createOn(3840, 89, 83),
		createOff(4000, 93), // This is the note off that straddles the sliceTime of 3500
		createCC(4000, 61, 61),
		createOff(5760, 89),
		createOn(5760, 77, 84),
		createOff(7680, 77),
		createOn(7680, 84, 85),
		createOff(seqLength, 84),
	}
	seqSlot.length_ticks = seqLength
	seqSlot.midi = make([]MidiEvent, len(input))
	copy(seqSlot.midi, input)
	seqSlot.markNoteLengthAndPartnerIndex()
	sliceTime := 3500
	foundSliceTime := 3840
	seqSlot = rotateSequence(seqSlot, sliceTime)

	exp := []MidiEvent{}
	for i := 3; i < len(input); i++ {
		if i == 4 {
			// We skip the noteOff at 4000 for note 93
			continue
		}
		m := input[i]
		m.time -= foundSliceTime
		exp = append(exp, m)
	}
	for i := 0; i < 3; i++ {
		m := input[i]
		m.time += seqLength - foundSliceTime
		exp = append(exp, m)
	}
	m := input[4]
	m.time += seqLength - foundSliceTime
	exp = append(exp, m)

	midiEqual(t, exp, seqSlot.midi)
}

func TestRotateSequenceAllCC(t *testing.T) {
	seqSlot := makeSequenceSlot("testdata/make_sequence_slot/AAA.0.0-60.mid")
	seqLength := 1100
	input := []MidiEvent{
		createCC(100, 61, 61),
		createCC(200, 62, 62),
		createCC(300, 63, 63),
		createCC(400, 64, 64),
		createCC(500, 65, 65),
		// Slice at 550
		createCC(600, 66, 66),
		createCC(700, 67, 67),
		createCC(800, 68, 68),
		createCC(900, 69, 69),
		createCC(1000, 70, 70),
		createCC(1100, 71, 71),
	}
	seqSlot.length_ticks = seqLength

	seqSlot.midi = make([]MidiEvent, len(input))
	copy(seqSlot.midi, input)
	seqSlot.markNoteLengthAndPartnerIndex()
	sliceTime := 550
	seqSlot = rotateSequence(seqSlot, sliceTime)

	exp := []MidiEvent{}
	for i := 5; i < len(input); i++ {
		m := input[i]
		m.time -= sliceTime
		exp = append(exp, m)
	}
	for i := 0; i < 5; i++ {
		m := input[i]
		m.time += seqLength - sliceTime
		exp = append(exp, m)
	}

	midiEqual(t, exp, seqSlot.midi)
}
