use strict;
use YAML;
use Scalar::Util qw(looks_like_number);
use Getopt::Long;
use Carp;
use Data::Dumper;
use File::Temp qw/ tempfile /;
my $LINE_LENGTH = 80;

my $CsvMidiPath = "$ENV{HOME}/CeeGuitar/midicsv-1.1/csvmidi";

## See manufacture_all_notes_and_names
my %noteName2Note;
my @note2NoteName;
my %legitNoteSymbols;

my %legitCellObjectFields = map {$_ => 1} qw/ ops /;

my @Letters = ("A" .. "Z");

##                         low E,  A,  D,  G,  B,  high E
my @guitarNotesByString = (   40, 45, 50, 55, 59,      64);
my %opFunctions;

my %defaultFields = (
    channel => {
        valid => sub {
            my ($c) = @_;
            return 0 unless isint($c) && $c >= 0 && $c < 16;
            return 1;
        },
        def => 0,
    },
    velocity_radius => {
        valid => sub {
            my ($r) = @_;
            return 0 unless (isint($r) && $r >= 0 && $r <= 63);
            return 1;
        }, 
        def => 20,
    },
);

## The following several variables are set at the bottom of the file.
my $KsbScript; ## KsbScript holds the KSB Script template.
my $baseLabelConfigTemplate; ## baseLabelConfigTemplate holds the base template for label-config

## $inKeyOffset[i-1] Holds the offset that you have to add to the root note of the key to get the I'th scale degree.
## Notice that inKeyOffset[0] == 0 by construction. MaxNumDegreeInKeyOffset defines how many scale degrees will
## be added to inKeyOffset. See build_in_key_offset and in_key.
my @inKeyOffset;
my @inPentatonicOffset;

our $WordsPerCommand = 3;
our $CommandsPerNote = 8;
our $NumNotes        = 128;


sub dd {
    print Dumper(@_), "\n";
}

my $db_print_commands = 0;

sub backtick {
    my ($command) = @_;
    print "$command\n" if $db_print_commands;
    my @lines = `$command`;
    if ($?) {
        confess "Failed '$command': $!";
    }
    chomp(@lines);
    return @lines;
}

sub run {
    my ($cmd) = @_;
    print "$cmd\n" if $db_print_commands;
    if (system $cmd){
        confess "Failed: '$cmd': $!";
    }
}

sub isint {
    my ($v) = @_;
    return $v =~ /^\d+$/;
}

## For each note in [0, 127], all the note names for that note will be added to a hash, and that hash returned. This
## functions purpose for living is to create NOTE_NAMES
sub manufacture_all_notes_and_names {
    my @symbols = qw/ C   C#  D  D#  E   F   F#  G  G#  A A# B  /;
    my @equivs  = qw/ B#  Db  _  Eb  Fb  E#  Gb  _  Ab  _ Bb Cb /;

    my %f;
    my @r;
    for (my $note = 0; $note < $NumNotes; $note++){
        my $octave = int($note/12)-2;
        my $letter = $symbols[$note % 12];
        my $s = sprintf "%s%d", $letter, $octave;
        $f{$s} = $note;
        push @r, $s;

        my $equiv  = $equivs[$note % 12];
        if ($equiv ne '_'){
            $f{sprintf "%s%d", $equiv, $octave} = $note;
        }
    }

    my %l;
    for my $e (@symbols) {
        $l{$e} = 1;
    }

    for my $e (@equivs) {
        next if $e eq '_';
        $l{$e} = 1;
    }

    %noteName2Note    = %f;
    @note2NoteName    = @r;
    %legitNoteSymbols = %l;
}

## note_from_text returns a note number by analyzing the text.
sub note_from_text {
    my ($text) = @_;
    if ($text =~ /^\d+$/){
        return $text;
    }

    confess "note_from_text got a bunk note: '$text'" unless $text =~ /([^\d]+)([-+]?\d)/;

    my $l = uc($1);
    (my $n = $2) =~ s/^\+//;
    my $sym = $l . $n;
    my $note = $noteName2Note{$sym};

    confess "note_from_text got bunk note: '$text'" unless defined($note);
    return $note;
}

sub _note2Coord {
    my ($note) = @_;
    my $C   = 2*(int($note/24)-1);
    my $row = int(($note % 24) / 4) + 1;
    my $col = int($note % 4) + 1;
    return ($C, $row, $col);
}

sub _note2Label {
    my ($C, $row, $col) = _note2Coord(@_);
    return sprintf "C%+d_r%d_c%d", $C, $row, $col;
}

## A LabelConfig is a list with NumNotes cells. This function validates a labelConfig, and possibly MODIFIES the
## input.
sub _validateAndNormalizeLabelConfig {
    my ($labels, $labelConfig) = @_;

    confess "LabelConfig is not the right size" unless @$labelConfig == $NumNotes && @$labels == $NumNotes;

    for (my $i = 0; $i < $NumNotes; $i++){
        eval {
            $labelConfig->[$i] = _validateAndNormalizeCell($labelConfig->[$i]);
        };
        if ($@){
            confess sprintf("LabelConfig validation error encountered while looking at %s\n$@", $labels->[$i]);
        }
    }

}

## A LabelConfig is a list with NumNotes cells. Each cell is logically a hash with two fields: notes and cc. Each
## of these fields, in term is logically a list whose first element is a function name, and whose ancillary elements
## are arguments to that function. With this in mind, there are two shortcuts we accept. First, if the cell is just
## a scalar string, we assume that scalar string is a function name for a notes list. I.e. if foo is a function name
## and our cell == foo, than we transform cell -> {notes=>[foo]}. Similarly, if a cell is JUST a list, we assume that
## is the notes list. So if foo is a list, than cell 0> {notes=>foo}.
sub _validateAndNormalizeCell {
    my ($cell) = @_;
    my $funcExists = sub {
        my ($fname, $key) = @_;
        confess "Bad cell: unknown function $fname in $key" unless defined($opFunctions{$fname});
    };

    my $arrayCool = sub {
        my ($arr, $key) = @_;
        confess "Bad cell: $key array length" unless @{$arr} > 0;
        $funcExists->($arr->[0], $key);
        for my $e (@{$arr}){
            confess "Bad hash cell: found non scalar in $key" unless ref($e) eq 'SCALAR';
        }
    };

    my $ref  = ref($cell);
    if (!$ref){
        $cell = {ops=>[$cell]};
    } elsif ($ref eq 'ARRAY'){
        $cell = {ops=>$cell};
    } elsif ($ref ne 'HASH'){
        confess "Bad cell: unknown ref $ref";
    } else {
        confess "Object cell does not have ops array" unless defined($cell->{ops}) && ref($cell->{ops}) eq 'ARRAY';
    }

    for my $k (keys %$cell){
        confess "Bad cell: unknown hash fields $k" unless $legitCellObjectFields{$k}
    }

    for (my $i = 0; $i < @{$cell->{ops}}; $i++){
        my $subcell = $cell->{ops}[$i];
        if (!ref($subcell)){
            $subcell = [$subcell];
            $cell->{ops}[$i] = $subcell;
        } elsif (ref($subcell) ne 'ARRAY') {
            confess "Unknown ref type for subcell " . ref($subcell);
        }

        my $key = "i$i";
        confess "Bad cell: $key array length" unless @{$subcell} > 0;
        $funcExists->($subcell->[0], $key);
        for my $e (@{$subcell}){
            my $ref = ref($e);
            confess "Bad cell: found non scalar in $key: $ref" unless !$ref || $ref eq 'SCALAR';
        }
    }

    return $cell;
}

## execute_label_config expects a normalized labelConfig. It produces a list of Raw Cells
sub execute_label_config {
    my ($labels, $labelConfig, $defaults) = @_;

    my $kitChannel = $defaults->{channel};

    confess "Inconsistent labelConfig/labels" unless @$labelConfig == $NumNotes && @$labels == $NumNotes;


    for (my $i = 0; $i < $NumNotes; $i++) {
        eval {            
            my $ops = $labelConfig->[$i]{ops};
            my @directives;
            my $velocity_radius;
            for my $op (@$ops) {
                my ($func, @args) = @$op;
                my %kvs;                
                my @r = $opFunctions{$func}(\@args, \%kvs);
                if (defined($kvs{radius})) {
                    confess "It is illegal to call radius twice for the same slot" 
                        if defined($velocity_radius);    
                    $velocity_radius = $kvs{radius};    
                }
                push @directives, @r;
            }
            $velocity_radius = $defaultFields{velocity_radius}{def} unless defined($velocity_radius);
            $labelConfig->[$i]{directives} = \@directives;
            $labelConfig->[$i]{velocity_radius} = $velocity_radius;
        };
        confess "Failed while working on $labels->[$i]: $@" if $@;
    }

    eval {
        for (my $i = 0; $i < $NumNotes; $i++) {
            next unless @{$labelConfig->[$i]{directives}};
            my $name = name_midi_file($kitChannel, $i, "_nd_vr" . $labelConfig->[$i]{velocity_radius});
            directives_to_midi($labelConfig->[$i]{directives}, $kitChannel, $name);
        }
    };
    confess "Failed to export files: $@" if $@;
}



sub _renderListByBlock {
    my ($list, $tab) = @_;
    $tab ||= "";
    my $l = @$list;
    my $b = "";
    for (my $i = 0; $i < $l; $i++){
        if (length($b) >= $LINE_LENGTH) {
            print "$tab$b...\n";
            $b = "";
        }
        $b .= $list->[$i];
        if ($i != $l-1){
            $b .= ', ';
        }
    }
    if (length($b) > 0) {
        print "$tab$b...\n";
    }
}

sub expand_list_as_blocks {
    my ($lst) = @_;
    my $ref = ref($lst);
    if (!$ref) {
        if (looks_like_number($lst)) {
            return $lst;
        } else {
            return "'$lst'"
        }
    }
    confess "Bad argument to expand_list_as_blocks: must be lists of (list or scalar)" unless $ref eq 'ARRAY';
    my $b = "[";
    for (my $i = 0; $i < @$lst; $i++) {
        $b .= expand_list_as_blocks($lst->[$i]);
        $b .= ', ' unless $i == $#$lst;
    }
    $b .= "]";
    return $b;
}
sub pretty_print_config {
    my ($cfg) = @_;
    print "---\n";
    print "label-config:\n";
    for (my $note = $NumNotes-1; $note >= 0; $note--){
        my $label = _note2Label($note);
        my $cell  = $cfg->{$label};
        $cell = [["pass"]] unless defined($cell);
        print "   $label: ", expand_list_as_blocks($cell), "\n";
    }
}

sub sanity_defaults_and_populate {
    my ($defaults) = @_;
    confess "defaults is not a hash" unless ref($defaults) eq 'HASH';
    for my $k (keys(%$defaults)){
        confess "Unknown default key $k" unless $defaultFields{$k};    
        confess "Bad argument to key $k: $defaults->{$k}" unless $defaultFields{$k}{valid}($defaults->{$k});
    }

    for my $k (keys(%defaultFields)){
        $defaults->{$k} = $defaultFields{$k}{def} unless defined($defaults->{$k});
    }
}

sub slurp_config_file {
    my ($file) = @_;
    my $o = YAML::LoadFile($file);
    my $defaults         = $o->{defaults};
    $defaults = {} unless defined($defaults);
    sanity_defaults_and_populate($defaults);

    my $rawLabeledConfig = $o->{'label-config'};
    confess "Config file didn't have a label-config element" unless defined($rawLabeledConfig);
    
    my @labels;
    my @labelConfig;
    my $count = 0;
    for (my $note = 0; $note < $NumNotes; $note++){
        my $label = _note2Label($note);
        push @labels, $label;
        my $cell  = $rawLabeledConfig->{$label};
        $count++ if defined($cell);
        push @labelConfig, defined($cell) ? $cell : "pass";
    }
    _validateAndNormalizeLabelConfig(\@labels, \@labelConfig);
    return (\@labels, \@labelConfig, $defaults)
}


## These few methods define all the allowed directives
sub raw_note {
    my ($n) = @_;
    return ['note', $n, 64];
}

sub raw_note_and_velocity {
    my ($n, $v) = @_;
    return ['note', $n, $v];
}

sub raw_cc {
    my ($cc_num, $cc_value) = @_;
    return ['cc', $cc_num, $cc_value];
}

sub directives_to_midi {
    my ($directives, $channel, $midi_output_file) = @_;
    my $track      = 1;
    my $start_time = 0;
    my $end_time   = 960;
    my @midi_start = ([0, 0, 'Header', 1, 1, 960],
                      [$track, 0, 'Start_track']);
    my @midi_end   = ();
    for my $direct (@$directives){
        my $com = $direct->[0];
        if ($com eq 'note'){   
            push @midi_start, [$track, 0,   'Note_on_c',  $channel, $direct->[1], $direct->[2]];
            push @midi_end,   [$track, 960, 'Note_off_c', $channel, $direct->[1], 0];
        } elsif ($com eq 'cc'){
            push @midi_start, [$track, 0,   'Control_c',  $channel, $direct->[1], $direct->[2]];
        } else {
            confess "Unknown command found: $com";
        }
    }
    push @midi_end, [$track, 960, 'End_track'],
                    [0,        0, 'End_of_file']; 
    my ($fd, $filename) = tempfile(CLEANUP=>0);
    for my $row (@midi_start, @midi_end){
        print {$fd} join(", ", @$row),"\n";
    }
    close($fd);
    run "$CsvMidiPath $filename > $midi_output_file";
    unlink $filename;
}

## name_midi_file MUST conform to the specification in CeeMidiPlayer in terms of file names
sub name_midi_file {
    my ($channel, $note, $opts) = @_;
    $opts = "" unless defined($opts);

    ## Note I'm solving a division algorithm of sorts for index = 26^2*i + 26*j + k where 0 <= i,j,k < 26. This is
    ## just expressing the index in base 26.
    my $index = int($channel*128 + $note);
    my $i     = int($index / (26*26));
    my $j     = int(($index - (26*26)*$i)/26);
    my $k     = ($index - (26*26)*$i - 26*$j) % 26;

    my $dok = sub {
        my ($x) = @_;
        return $x >= 0 && $x < 26;
    };
    confess "Failed Sanity in name_midi_file" 
        unless $dok->($i) && $dok->($j) && $dok->($k) && ($index == 26*26*$i + 26*$j + $k);

    my $lex = "$Letters[$i]$Letters[$j]$Letters[$k]";
    my $transpose = sprintf "%+d", ($note-60);
    my $nfile = "$lex.$channel.$note$transpose$opts.mid";
    return $nfile;
}

sub build_all_keyswitch_funcs {
    my @noteTable = (
        ["ks_poly",          "F#0"],
        ["ks_solo",          "G#0"],

        ["mod_tremolo",      "A#-1"], ## A mod key needs to be held down
        ["mod_trill_1",      "C#0"],
        ["mod_trill_2",      "D#0"],
        ["mod_slide_1_down", "G#6"],
        ["mod_slide_1_up",   "A#6"],
        ["mod_fx",           "F0"],
        ["mod_slides",       "A0"],
        ["mod_harmonics",    "D#5"],
        
        ["tr_chucka_up",     "D#6"],
        ["tr_chucka_down",   "D6"],

        ["tr_deadmute_up",   "C#6"],
        ["tr_deadmute_down", "C6"],

        ["tr_mute_up",       "A#5"],
        ["tr_mute_down",     "A5"],

        ["tr_halfmute_up",       "G#5"],
        ["tr_halfmute_down",     "G5"],

        ["tr_up",                "F#5"],
        ["tr_down",              "F5"],

        ["tr_string_6", "C7"],
        ["tr_string_5", "B6"],
        ["tr_string_4", "A6"],
        ["tr_string_3", "G6"],
        ["tr_string_2", "F6"],
        ["tr_string_1", "E6"],

        ["tr_palm_mute", "E5"],
        ["tr_hand_mute", "F#6"],

        ["rel_finger",          "A-1", "A#-1"],
        ["rel_finger_short",    "A-1", "B-1"],
        ["rel_mixed_1",         "A-1", "C0"],
        ["rel_mixed_2",         "A-1", "C#0"],

        ["rel_hand_mute",       "A-1",  "D0"],
        ["rel_palm_mute",       "A-1",  "D#0"],

        ["rel_pick_noise",       "A-1", "E0"],
        ["rel_slide_down_short", "A-1", "F0"],
        ["rel_slide_down_med",   "A-1", "F#0"],
        ["rel_slide_down_long",  "A-1", "G0"],

        ["rel_slide_down_1",  "A-1", "G#0"],
        ["rel_slide_up_1",    "A-1", "A0"],

        ["rel_slide_noise_down",  "A-1", "A#0"],
        ["rel_slide_noise_up",    "A-1", "B0"],
    );
 
    my $soft = 1;
    my $hard = 127;
    my @noteWithFixedVelocityTable = (
        ["ks_legato_mute", "A#0", $soft],
        ["ks_legato",      "A#0", $hard],

        ["ks_hm",           "B0", $soft],
        ["ks_sustain",      "B0", $hard],

        ["morph_mute_sustain",       "C0", $hard],
        ["morph_mute_hm",            "C0", $soft],
        ["morph_sustain_harm_oct",   "D0", $hard],
        ["morph_sustain_harm_fifth", "D0", $soft],

        ["morph_muteho_ho", "E0", $hard],        
        ["morph_mutepo_po", "E0", $soft],        

        ["ks_ghosts_clean", "G0", $hard],
        ["ks_ghosts_dirty", "G0", $soft],
    );

    for my $entry (@noteTable) {
        my $name = $entry->[0];
        my $note = note_from_text($entry->[1]);
        if (defined($entry->[2])) {
            my $note2 = note_from_text($entry->[2]);
            $opFunctions{$name} = sub {
                return (raw_note($note), raw_note($note2));
            };   
        } else {
            $opFunctions{$name} = sub {
                return raw_note($note);
            };   
        }
    }

    for my $entry (@noteWithFixedVelocityTable) {
        my $name = $entry->[0];
        my $note = note_from_text($entry->[1]);
        my $vel  = $entry->[2];
        $opFunctions{$name} = sub {
            return raw_note_and_velocity($note, $vel);
        };
    }
}

sub flag_bad_arg_count {
    my ($name, $exp, $got) = @_;
    confess "Argument mismatch for function $name: got $got, expected $exp" unless $exp == $got;
}

## Mark a cell pass if you just want the midi note to pass through unchanged
sub pass {
    flag_bad_arg_count("pass", 0, size_first_ref(@_));
    return ();
}
$opFunctions{pass} = \&pass;

sub radius {
    my ($arg_list, $kvs) = @_;
    flag_bad_arg_count("radius", 1, size_first_ref(@_));
    $kvs->{radius} = $arg_list->[0];
    sanity_radius($kvs->{radius});
    return ();
}
$opFunctions{radius} = \&radius;

sub notes {
    confess "notes requires at least on argument" if size_first_ref(@_) < 1;
    my @args = @{$_[0]};
    @args = map {note_from_text($_)} @args;
    my @descriptors;
    for my $n (@args){
        push @descriptors, raw_note($n);
    }
    return @descriptors;
}
$opFunctions{notes} = \&notes;

sub sanity_velocity {
    my ($v) = @_;
    confess "Bad velocity $v" unless $v >= 0 && $v < 128;

}
sub sanity_radius {
    my ($r) = @_;
    confess "Bad radius $r" unless isint($r) && $r >= 0 && $r <= 63;    
}

sub guitar_template {
    my ($shift, @shape) = @_;
    confess "Bad value of shift '$shift': must be integer" unless $shift =~ /^\d+$/;
    my @descriptors;
    for (my $i = 0; $i < 6; $i++){
        next if $shape[$i] eq 'x';
        push @descriptors, raw_note($guitarNotesByString[$i] + $shift + $shape[$i]);
    }
    return @descriptors;
}

sub build_in_key_offset {
    @inKeyOffset = ();
    my @steps = (2, 2, 1, 2, 2, 2, 1);
    my $off = 0;
    my $i   = 0;
    while ($off < 128){
        push @inKeyOffset, $off;
        $off += $steps[$i % @steps];
        $i++;
    }
}

sub build_penatonic_offset {
    @inPentatonicOffset = ();
    my @steps = (2, 2, 3, 2, 3);
    my $off = 0;
    my $i   = 0;
    while ($off < 128){
        push @inPentatonicOffset, $off;
        $off += $steps[$i % @steps];
        $i++;
    }   
}

sub size_first_ref {
    return 0 if @_ < 1;
    return scalar(@{$_[0]});
}

sub Em_shape {
    flag_bad_arg_count("Em_shape", 1, size_first_ref(@_));
    return guitar_template($_[0][0], 0, 2, 2, 0, 0, 0);
}
$opFunctions{Em_shape} = \&Em_shape;

sub E_shape {
    flag_bad_arg_count("E_shape", 1, size_first_ref(@_));
    return guitar_template($_[0][0], 0, 2, 2, 1, 0, 0);
}
$opFunctions{E_shape} = \&E_shape;

sub Am_shape {
    flag_bad_arg_count("Am_shape", 1, size_first_ref(@_));
    return guitar_template($_[0][0], 'x', 0, 2, 2, 1, 0);
}
$opFunctions{Am_shape} = \&Am_shape;

sub A_shape {
    flag_bad_arg_count("A_shape", 1, size_first_ref(@_));
    return guitar_template($_[0][0], 'x', 0, 2, 2, 2, 0);
}
$opFunctions{A_shape} = \&A_shape;

sub Dm_shape {
    flag_bad_arg_count("Dm_shape", 1, size_first_ref(@_));
    return guitar_template($_[0][0], 'x', 'x', 0, 2, 3, 1);
}
$opFunctions{Dm_shape} = \&Dm_shape;

sub D_shape {
    flag_bad_arg_count("D_shape", 1, size_first_ref(@_));
    return guitar_template($_[0][0], 'x', 'x', 0, 2, 3, 2);
}
$opFunctions{D_shape} = \&D_shape;

sub G_shape {
    flag_bad_arg_count("G_shape", 1, size_first_ref(@_));
    return guitar_template($_[0][0], 3, 2, 0, 0, 0, 3);   
}
$opFunctions{G_shape} = \&G_shape;

sub Gp_shape {
    flag_bad_arg_count("Gp_shape", 1, size_first_ref(@_));
    return guitar_template($_[0][0], 3, 2, 0, 0, 3, 3);   
}
$opFunctions{Gp_shape} = \&Gp_shape;

sub C_shape {
    flag_bad_arg_count("C_shape", 1, size_first_ref(@_));
    return guitar_template($_[0][0], 'x', 3, 2, 0, 1, 0);      
}
$opFunctions{C_shape} = \&C_shape;

sub Cp_shape {
    flag_bad_arg_count("Cp_shape", 1, size_first_ref(@_));
    return guitar_template($_[0][0], 'x', 3, 2, 0, 3, 3);      
}
$opFunctions{Cp_shape} = \&Cp_shape;

sub power2 {
    flag_bad_arg_count("power2", 1, size_first_ref(@_));
    my ($root) = @{$_[0]};
    $root = note_from_text($root);
    my @descriptors = (raw_note($root), raw_note($root+7));
}
$opFunctions{power2} = \&power2;

sub power3 {
    flag_bad_arg_count("power3", 1, size_first_ref(@_));
    my ($root) = @{$_[0]};
    $root = note_from_text($root);
    my @descriptors = (raw_note($root), raw_note($root+7), raw_note($root+12));
}
$opFunctions{power3} = \&power3;

sub forths {
    flag_bad_arg_count("forths", 1, size_first_ref(@_));
    my ($root) = @{$_[0]};
    $root = note_from_text($root);
    my @descriptors = (raw_note($root), raw_note($root+5));
}
$opFunctions{forths} = \&forths;

sub in_key {
    flag_bad_arg_count("in_key", 2, size_first_ref(@_));
    my ($key, $degree) = @{$_[0]};
    $key = note_from_text($key);
    confess "Degree must be > 0" unless $degree > 0;
    confess "Degree too large" unless $degree < @inKeyOffset;
    my $note = $key + $inKeyOffset[$degree-1];
    confess "in_key degree overflow: $note" unless $note < 128;
    return raw_note($note);
}
$opFunctions{in_key} = \&in_key;

sub in_penta {
    flag_bad_arg_count("in_penta", 2, size_first_ref(@_));
    my ($key, $degree) = @{$_[0]};
    $key = note_from_text($key);
    confess "Degree must be > 0" unless $degree > 0;
    confess "Degree too large" unless $degree < @inPentatonicOffset;
    my $note = $key + $inPentatonicOffset[$degree-1];
    confess "in_penta degree overflow: $note" unless $note < 128;
    return raw_note($note);    
}
$opFunctions{in_penta} = \&in_penta;




sub cc {
    flag_bad_arg_count("cc", 2, size_first_ref(@_));
    my ($cc, $value) = @{$_[0]};
    return raw_cc($cc, $value);
}
$opFunctions{cc} = \&cc;

sub main {    
    ## Make sure everything that might need to be predefined, is predefined.
    manufacture_all_notes_and_names();
    build_all_keyswitch_funcs();
    build_in_key_offset();

    my ($printConfig, $printTemplate);
    Getopt::Long::Configure("bundling");
    Getopt::Long::GetOptions(
        "c" => \$printConfig,
        "t" => \$printTemplate,
    );
    

    if ($printTemplate) {
        print $baseLabelConfigTemplate;
        return;
    }
    if ($printConfig) {
        pretty_print_config({});
        return;
    }
    
    confess "Must provide config file" unless @ARGV == 1;

    my $configFile = $ARGV[0];
    my ($labels, $labelConfig, $defaults) = slurp_config_file($configFile);

    execute_label_config($labels, $labelConfig, $defaults);
}

$baseLabelConfigTemplate = <<'BASE_CONFIG';
---
label-config:
   C+8_r2_c4: [['pass']]
   C+8_r2_c3: [['pass']]
   C+8_r2_c2: [['pass']]
   C+8_r2_c1: [['pass']]
   C+8_r1_c4: [['pass']]
   C+8_r1_c3: [['pass']]
   C+8_r1_c2: [['pass']]
   C+8_r1_c1: [['pass']]
   C+6_r6_c4: [['pass']]
   C+6_r6_c3: [['pass']]
   C+6_r6_c2: [['pass']]
   C+6_r6_c1: [['pass']]
   C+6_r5_c4: [['pass']]
   C+6_r5_c3: [['pass']]
   C+6_r5_c2: [['pass']]
   C+6_r5_c1: [['pass']]
   C+6_r4_c4: [['pass']]
   C+6_r4_c3: [['pass']]
   C+6_r4_c2: [['pass']]
   C+6_r4_c1: [['pass']]
   C+6_r3_c4: [['pass']]
   C+6_r3_c3: [['pass']]
   C+6_r3_c2: [['pass']]
   C+6_r3_c1: [['pass']]
   C+6_r2_c4: [['pass']]
   C+6_r2_c3: [['pass']]
   C+6_r2_c2: [['pass']]
   C+6_r2_c1: [['pass']]
   C+6_r1_c4: [['pass']]
   C+6_r1_c3: [['pass']]
   C+6_r1_c2: [['pass']]
   C+6_r1_c1: [['pass']]
   C+4_r6_c4: [['pass']]
   C+4_r6_c3: [['pass']]
   C+4_r6_c2: [['pass']]
   C+4_r6_c1: [['pass']]
   C+4_r5_c4: [['pass']]
   C+4_r5_c3: [['pass']]
   C+4_r5_c2: [['pass']]
   C+4_r5_c1: [['pass']]
   C+4_r4_c4: [['pass']]
   C+4_r4_c3: [['pass']]
   C+4_r4_c2: [['pass']]
   C+4_r4_c1: [['pass']]
   C+4_r3_c4: [['pass']]
   C+4_r3_c3: [['pass']]
   C+4_r3_c2: [['pass']]
   C+4_r3_c1: [['pass']]
   C+4_r2_c4: [['pass']]
   C+4_r2_c3: [['pass']]
   C+4_r2_c2: [['pass']]
   C+4_r2_c1: [['pass']]
   C+4_r1_c4: [['pass']]
   C+4_r1_c3: [['pass']]
   C+4_r1_c2: [['pass']]
   C+4_r1_c1: [['pass']]
   ##
   ## Third frame
   ##
   C+2_r6_c4: [['tr_chucka_up']]
   C+2_r6_c3: [['tr_chucka_down']]
   C+2_r6_c2: [['mod_tremolo']]
   C+2_r6_c1: [['tr_palm_mute']]
   C+2_r5_c4: [['mod_slide_1_up']]
   C+2_r5_c3: [['mod_slide_1_up']]
   C+2_r5_c2: [['mod_harmonics']]
   C+2_r5_c1: [['mod_slides']]
   C+2_r4_c4: [['power2', 'C4']]
   C+2_r4_c3: [['power2', 'B3']]
   C+2_r4_c2: [['power2', 'A3']]
   C+2_r4_c1: [['power2', 'G3']]
   C+2_r3_c4: [['power2', 'F3']]
   C+2_r3_c3: [['power2', 'E3']]
   C+2_r3_c2: [['power2', 'D3']]
   C+2_r3_c1: [['power2', 'C3']]
   C+2_r2_c4: tr_deadmute_up
   C+2_r2_c3: tr_deadmute_down
   C+2_r2_c2: tr_mute_up
   C+2_r2_c1: tr_mute_down
   C+2_r1_c4: tr_halfmute_up
   C+2_r1_c3: tr_halfmute_down
   C+2_r1_c2: tr_up
   C+2_r1_c1: tr_down
   ##
   ## Second Frame
   ##
   C+0_r6_c4: [['morph_sustain_harm_fifth']]
   C+0_r6_c3: [['morph_sustain_harm_oct']]
   C+0_r6_c2: [['ks_ghosts_dirty']]
   C+0_r6_c1: [['ks_ghosts_clean']]
   C+0_r5_c4: [['ks_sustain']]
   C+0_r5_c3: [['ks_hm']]
   C+0_r5_c2: [['morph_mute_hm']]
   C+0_r5_c1: [['morph_mute_sustain']]
   C+0_r4_c4: [['in_key', 'C3', 8]]
   C+0_r4_c3: [['in_key', 'C3', 7]]
   C+0_r4_c2: [['in_key', 'C3', 6]]
   C+0_r4_c1: [['in_key', 'C3', 5]]
   C+0_r3_c4: [['in_key', 'C3', 4]]
   C+0_r3_c3: [['in_key', 'C3', 3]]
   C+0_r3_c2: [['in_key', 'C3', 2]]
   C+0_r3_c1: [['in_key', 'C3', 1]]
   C+0_r2_c4: [['mod_trill_1']]
   C+0_r2_c3: [['mod_slides']]
   C+0_r2_c2: [['morph_muteho_ho']]
   C+0_r2_c1: [['morph_mutepo_po']]
   C+0_r1_c4: [['ks_legato']]
   C+0_r1_c3: [['ks_legato_mute']]
   C+0_r1_c2: [['ks_solo']]
   C+0_r1_c1: [['ks_poly']]
   ##
   ## First frame
   ##
   C-2_r6_c4: [['tr_string_6']]
   C-2_r6_c3: [['tr_string_5']]
   C-2_r6_c2: [['tr_string_4']]
   C-2_r6_c1: [['tr_string_3']]
   C-2_r5_c4: [['tr_string_2']]
   C-2_r5_c3: [['tr_string_1']]
   C-2_r5_c2: [['ks_solo']]
   C-2_r5_c1: [['ks_poly']]
   C-2_r4_c4: [['tr_chucka_up']]
   C-2_r4_c3: [['tr_chucka_down']]
   C-2_r4_c2: [['mod_slide_1_up']]
   C-2_r4_c1: [['tr_palm_mute']]
   C-2_r3_c4: [['Gp_shape', 0]]
   C-2_r3_c3: [['Cp_shape', 0]]
   C-2_r3_c2: [['G_shape',  0]]
   C-2_r3_c1: [['C_shape',  0]]
   C-2_r2_c4: [['D_shape',  0]]
   C-2_r2_c3: [['Am_shape', 0]]
   C-2_r2_c2: [['Em_shape', 0]]
   C-2_r2_c1: [['E_shape',  0]]
   C-2_r1_c4: [['tr_halfmute_up']]   
   C-2_r1_c3: [['tr_halfmute_down']]
   C-2_r1_c2: [['tr_up']]
   C-2_r1_c1: [['tr_down']]
BASE_CONFIG

my $midiTemplate = <<'END';
## Header x, y, z --> where x is the format (always 1), y is nTracks which will be 1 for me, and z is
##                    the TPQN which for me is always 960
0, 0, Header, 1, 2, 480
$CHANNEL, 0, Start_track

Track, Time, Note_on_c, Channel, Note, Velocity
Send a command to play the specified Note (Middle C is defined as Note number 60; all other notes are relative in the MIDI specification, but most instruments conform to the well-tempered scale) on the given Channel with Velocity (0 to 127). A Note_on_c event with Velocity zero is equivalent to a Note_off_c.

Track, Time, Note_off_c, Channel, Note, Velocity
Stop playing the specified Note on the given Channel. The Velocity should be zero, but you never know what you'll find in a MIDI file.

Track, Time, Pitch_bend_c, Channel, Value
Send a pitch bend command of the specified Value to the given Channel. The pitch bend Value is a 14 bit unsigned integer and hence must be in the inclusive range from 0 to 16383. The value 8192 indicates no pitch bend; 0 the lowest pitch bend, and 16383 the highest. The actual change in pitch these values produce is unspecified.

Track, Time, Control_c, Channel, Control_num, Value
Set the controller Control_num on the given Channel to the specified Value. Control_num and Value must be in the inclusive range 0 to 127. The assignment of Control_num values to effects differs from instrument to instrument. The General MIDI specification defines the meaning of controllers 1 (modulation), 7 (volume), 10 (pan), 11 (expression), and 64 (sustain), but not all instruments and patches respond to these controllers. Instruments which support those capabilities usually assign reverberation to controller 91 and chorus to controller 93.

Track, Time, Program_c, Channel, Program_num
Switch the specified Channel to program (patch) Program_num, which must be between 0 and 127. The program or patch selects which instrument and associated settings that channel will emulate. The General MIDI specification provides a standard set of instruments, but synthesisers are free to implement other sets of instruments and many permit the user to create custom patches and assign them to program numbers. 


Track, Time, Channel_aftertouch_c, Channel, Value
When a key is held down after being pressed, some synthesisers send the pressure, repeatedly if it varies, until the key is released, but do not distinguish pressure on different keys played simultaneously and held down. This is referred to as “monophonic” or “channel” aftertouch (the latter indicating it applies to the Channel as a whole, not individual note numbers on that channel). The pressure Value (0 to 127) is typically taken to apply to the last note played, but instruments are not guaranteed to behave in this manner.

Track, Time, Poly_aftertouch_c, Channel, Note, Value
Polyphonic synthesisers (those capable of playing multiple notes simultaneously on a single channel), often provide independent aftertouch for each note. This event specifies the aftertouch pressure Value (0 to 127) for the specified Note on the given Channel.

$CHANNEL, 4800, End_track
0, 0, End_of_file
END


main();
