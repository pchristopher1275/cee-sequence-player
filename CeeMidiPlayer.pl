use strict;
use Data::Dumper;
use Carp;
use File::Temp;

my $LineLength = 80;

## Script bodies
my $TheNucleus; ## See below
my $MpPlayBody;

my $LogFile = "log.txt";

my $MidiCsvPath = "$ENV{HOME}/CeeGuitar/midicsv-1.1/midicsv";
my $CsvMidiPath = "$ENV{HOME}/CeeGuitar/midicsv-1.1/csvmidi";
## Maschines transpose knob only supports -48 semitones -> +48 semitones. So when
## we import we only consider this note range. Compile supports all notes.
my $MinImportNote = 12;
my $MaxImportNote = 108;

my $MinChokeGroup     = 0;
my $MaxChokeGroup     = 15;
my $DefaultChokeGroup = 0;
my $DefaultChannel    = 0;

my $CUSTOM_COMMAND_ENDGROUP = 256;
my $CUSTOM_COMMAND_CYCLE    = 257;
## Tool that takes midi file, and slice_note, and produces a new file
## $LEX.$CHANNEL.$NOTE$SIGNED_TRANSPOSE.<user-text-with-no_><options>_sl960.mid 
## Here $SIGNED_TRANSPOSE and $NOTE are redundant. $SIGNED_TRANSPOSE is 
## a positive or negative offset from C3. It's used in maschine. $SIGNED_TRANSPOSE
## always starts with a + or - (+0). 

my @Letters = ("A" .. "Z");
my @NoteSymbols = qw/ C   C#  D  D#  E   F   F#  G  G#  A A# B  /;

my %command_map = (
    Note_on_c    => '$OO',
    Note_off_c   => '$XX',
    Pitch_bend_c => '$PP',
    Control_c    => '$CC',
    Endgroup     => $CUSTOM_COMMAND_ENDGROUP,
    Cycle        => $CUSTOM_COMMAND_CYCLE,
);


my %KnownConfigFields = (
    ## import options
    sn => {name=>"SliceNote", arg=>"Integer specifies which note to consider the split note"},
    ic => {name=>"InputChannel", arg=>"Integer specifies which input channel the midi file MUST be imported into"},
    
    ## compile options
    cg => {name=>"ChokeGroup", arg=>"Integer specifies choke group (0-15)"},
    ch => {name=>"OutputChannel", arg=>"Integer specifies output channel (0-15)"},
    ln => {name=>"Length", arg=>"Integer describes length (in quarter notes) of the sequence"},
    sl => {name=>"Slice", arg=>"Integer specifies where to slice a loop (given in ticks).",
            desc=>"Note that this is just a tag, compile doesn't do anything with it"},
    es => {name=>"ExclusiveSlice", arg=>"NONE", 
        desc=>"If present means that, rather than playing entire sequence, only plays events between slice markers"},
    
    em => {name=>"Empty", arg=>"NONE", desc=>"Specifies that (reguardless of what is in the midi notes) this sequence is empty"},
    me => {name=>"MarkEndgroup", arg=>"NONE", desc=>"Mark the sequence to use the endgroup feature"},
    nd => {name=>"NonDynamic", arg=>"NONE", desc=>"Means that time dependence of midi sequence is ignored"},
    um => {name=>"UseMasterClock", arg=>"NONE", desc=>"Means that the sequence should use the master clock"},
    sm => {name=>"StopMasterClock", arg=>"NONE", 
            desc=>"Means that before you start the sequence stop the master clock. NOTE: sm DOES NOT imply um"},
    as => {name=>"AllStop", arg=>"NONE", desc=>"Stop all sequences."},
    vr => {name=>'VelocityRadius', arg=>"An integer radius with value between 1 and 127", 
                                   desc=>"Set the velocity sensitivity of loop"},
    vd => {name=>'VelocityDecay', arg=>"And integer specifiying the number of quarter notes to decay the velocity delta",
                                  desc=>"Set the rate that the velocity sensitivity peeters out"},

    tg => {name=>'Tag', arg=>"Text that helps identify the sequence", 
           desc=>"Just allows a hunk of text to id sequence"}
);

sub dd {
    print Dumper(@_), "\n";
}

sub backtick {
    my ($command) = @_;
    my @lines = `$command`;
    if ($?) {
        confess "Failed '$command': $!";
    }
    chomp(@lines);
    return @lines;
}

sub run {
    my ($cmd) = @_;
    if (system $cmd){
        confess "Failed: '$cmd': $!";
    }
}

sub isint {
    my ($v) = @_;
    return $v =~ /^\d+$/;
}

sub now {
    my ($d) = backtick 'date +"%F %T"';
    return $d;
}

sub note_from_text {
    my ($text) = @_;
    if ($text =~ /^\d+$/){
        confess "INTERNAL ERROR [1]: bad note" unless $text >= 0 && $text < 128;    
        return $text;
    }

    die "note_from_text got a bunk note: '$text'" unless $text =~ /([^\d]+)([-+]?\d)/;

    my $l = uc($1);
    $l =~ s/s/#/; ## We accept s for # -- so it's nicer to work with on command line
    (my $n = $2) =~ s/^\+//;
    
    my $offset = -1;
    for (my $i = 0; $i < @NoteSymbols; $i++){
        if ($NoteSymbols[$i] eq $l){
            $offset = $i;
            last;
        }
    }
    confess "Failed to find offset for note-letter $l" unless $offset >= 0;
    my $note = ($n+2)*12 + $offset;
    confess "INTERNAL ERROR [2]: bad note" unless $note >= 0 && $note < 128;

    return $note;
}

sub scan_filename_meta {
    my ($filename) = @_;
    my $original = $filename;
    $filename =~ s/\.mid$//;
    my @fields = split "_", $filename;

    ## The first segment does not contain config variables
    shift @fields;

    my %meta;
    for my $f (@fields){
        next unless $f;
        my ($name, $value) = ($f =~ /([^\d][^\d])(.*)/);
        $value = "" unless defined($value);
        confess "Bad field found in $original" unless defined($name) && $name;
        my $cfg = $KnownConfigFields{$name};
        confess "Unknown field $name found in $original" if (!defined($cfg));
        confess "Found argument in boolean config variable $name in $original" if ($value && $cfg->{arg} eq 'NONE');
        $value = 1 if $cfg->{arg} eq 'NONE';
        $meta{$name} = $value;
    }

    confess "Configuration can't have me and um" if $meta{me} && $meta{um};
    confess "Configuration that uses sn MUST use ln" if defined($meta{sn}) && !defined($meta{ln});
    return \%meta;
}

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


## NOTE: transpose in this context is the transpose setting in maschine that gets you back to the note. 
## So maschine always transposes from middle-C (note 60)
sub transpose_from_note {
    my ($note) = @_;
    return sprintf "%+d", ($note-60);
}

## list all midi files in directory that conform to the above format. Return them as $files->{$CHANNEL_$NOTE}
sub list_compile_files {
    my @ls = backtick("ls *.mid");
    @ls = grep {/^[A-Z][A-Z][A-Z]\.\d{1,2}\.\d{1,3}[-+]\d{1,2}/} @ls;
    my %files;
    for my $file (@ls){
        my ($ch, $note) = parse_chan_note($file);
        $files{"${ch}.${note}"} = $file;
    }
    @ls = sort {$a <=> $b} @ls;
    return (\%files, \@ls);
}   

sub list_noncompile_files {
    my ($files) = list_compile_files();
    my %mp;
    for my $k (keys(%$files)){
        $mp{$files->{$k}} = 1;
    }
    my @ls = backtick("ls *.mid");
    return [grep {!$mp{$_}} @ls];
}

sub parse_chan_note {
    my ($arg) = @_;
    my ($chan, $note) = ($arg =~ /(\d{1,2})\.(\d{1,3})/);
    confess "parse_chan_note passed bad argument $arg" unless defined($note);
    confess "parse_chan_note bad channel parsed out of $arg" unless $chan >= 0 && $chan < 16;
    confess "parse_chan_note bad note parsed out of $arg" unless $note >= 0 && $note < 128;
    return $chan, $note;
}

sub index_from_chan_note {
    my ($chan, $note) = @_;
    return $chan * 128 + $note;
}

sub find_slots_for_seqs {
    my ($comp_hash, $nslots, $input_channel) = @_;
    my @slots;
    my $count = 0;
    LOOP: for (my $i = 0; $i < 16; $i++){
        next if defined($input_channel) && $i != $input_channel;
        for (my $j = $MinImportNote; $j <= $MaxImportNote; $j++){
            my $s = "$i.$j";
            if (!defined($comp_hash->{$s})){
                push @slots, $s;
                $count++;
                last LOOP if $count >= $nslots;
            }            
        }
    }
    confess "Failed to find $nslots slots" unless $count == $nslots;
    return @slots;
}


## fetch midi using midicsv. Note, we throw non channel level messages away. We also ignore the channel argument.
sub fetch_midi {
    my ($filename) = @_;
    my @midi;
    my $TPQN = 960; 
    my %want = (map {$_ => 1} qw/ Header Note_on_c Note_off_c Pitch_bend_c Control_c /);
    for my $line (backtick("$MidiCsvPath $filename")){
        my ($track, $time, $command, @rest) = split ', ', $line;
        
        next unless $want{$command};
        if ($TPQN != 960) {
            $time = int( ($time/$TPQN) * 960 );
        }
        if ($command eq 'Note_on_c' || $command eq 'Note_off_c'){
            my ($channel, $note, $vel) = @rest;
            push @midi, [$time, $command, $note, $vel];
        } elsif ($command eq 'Pitch_bend_c') {
            my ($channel, $value) = @rest;
            my $msb = (($value >> 7) & 0x7F);
            my $lsb = ($value & 0x7F);
            push @midi, [$time, $command, $msb, $lsb];
        } elsif ($command eq 'Command_c'){
            my ($channel, $cc, $value) = @rest;
            push @midi, [$time, $command, $cc, $value];
        } elsif ($command eq 'Header'){
            ## NOTE: the Header command is ALWAYS the first command, so if it's present this should all work.
            my ($format, $nTracks, $division) = @rest;
            $TPQN = $division;
        }
    }
    return {midi=>\@midi, meta=>scan_filename_meta($filename), filename=>$filename};
}

sub write_midi_file {
    my ($midi, $channel, $midi_output_file) = @_;
    my $track      = 1;
    my ($fd, $filename) = File::Temp::tempfile(CLEANUP=>0, UNLINK=>0);
    print {$fd} join(', ', 0, 0, 'Header', 1, 1, 960),"\n";
    print {$fd} join(', ', $track, 0, 'Start_track'),"\n";
    my $latest_time = -1;
    for my $row (@$midi) {
        confess "INTERNAL ERROR: row_count=" . scalar(@$row) unless @$row == 4;
        $latest_time = $row->[0] unless $latest_time > $row->[0];
        print {$fd} join(', ', $track, $row->[0], $row->[1], $channel, $row->[2], $row->[3]), "\n";
    }
    print {$fd} join(', ', $track, $latest_time, 'End_track'), "\n";
    print {$fd} join(', ', 0, 0, 'End_of_file'), "\n";
    close($fd);
    run "$CsvMidiPath $filename > $midi_output_file";
    unlink $filename;
}


my %midi_event_type_weight_table = (
    Note_off_c   => 0,
    Note_on_c    => 1,
    Pitch_bend_c => 2,
    Control_c    => 3,
);

sub midi_event_type_weight {
    my ($x) = @_;
    my $y = $midi_event_type_weight_table{$x};
    confess "INTERNAL ERROR" unless defined($y);
    return $y;
}

sub sort_note_offs_first {
    ## First sort by time if possible
    return $a->[0] <=> $b->[0] unless $a->[0] == $b->[0];
    ## Now sort Note_off before Note_on
    return midi_event_type_weight($a->[1]) <=> midi_event_type_weight($b->[1]);
}

sub rotate_sequence_one_shot {
    my ($input_file, $output_file, $slice_note, $slice_point, $midi) = @_;
    
    ## The main difference b/w this and rotate_sequence_master_clock, is that (a) this version WILL NOT
    ## seperate Note_on_c from Note_off_c. This will mean if you let a sequence run to completion it
    ## will be impossible to leave a stranded note. Because of the way we do (a), the second property of
    ## this function is that we make sure that the sequence is NOT any longer than it's specified length
    ## (via the _ln option).

    ## REMEMEBER: for input midi the following is true
    ##    $m[$i][0] -- Is the time the event happend
    ##    $m[$i][1] -- Is the type of event that happened
    ##    $m[$i][2] -- Is the first byte, which is the note number for any Note_on_c or Note_off_c
    ##    $m[$i][3] -- Is the velocity of the note
    ## We also have, for Note_on_c, sometimes we have
    ##    $m[$i][4] -- Length of note

    ## First group Note_on and Note_offs. Do this by putting a length at the end of the Note_on_c row
    ## and remove any Note_off_c
    
    my $length = sequence_length($midi);
    my @m = sort sort_note_offs_first @{$midi->{midi}};

    my %pending;
    my @n;
    for (my $i = 0; $i < @m; $i++){
        my $row = $m[$i];
        if ($row->[1] eq 'Note_on_c'){
            confess "rotate_sequence_one_shot (on): unbalanced midi file $row->[2] ($i)" if $pending{$row->[2]};
            push @n, [@$row];
            $pending{$row->[2]} = $#n;
        } elsif ($row->[1] eq 'Note_off_c'){
            confess "rotate_sequence_one_shot (off): unbalanced midi file $row->[2] ($i)" if !defined($pending{$row->[2]});
            my $source = $pending{$row->[2]};
            delete($pending{$row->[2]});
            ## Push the length onto the source Note_on 
            push @{$n[$source]}, ($row->[0] - $n[$source][0]);
        } else {
            push @n, [@$row];    
        }
    }
    confess "INTERNAL ERROR" unless scalar(%pending) == 0;

    @m = @n;
    @n = ();

    ## Now slice and rotate
    my $slice_index = -1;
    for (my $i=0; $i < @m; $i++){
        if ($m[$i][0] >= $slice_point && $m[$i][2] != $slice_note && $m[$i][1] eq 'Note_on_c'){
            $slice_index = $i;
            last;
        }
    }
    confess "INTERNAL ERROR" unless $slice_index >= 0 && $slice_index < @m;

    my $ignore_slice_note = sub {
        my ($index) = @_;
        my $type = $m[$index][1];
        my $note = $m[$index][2];
        return (($type eq 'Note_on_c' || $type eq 'Note_off_c') && $note == $slice_note);
    };

    for (my $i = $slice_index; $i<@m; $i++){
        next if $ignore_slice_note->($i);
        my @row = @{$m[$i]};
        $row[0] -= $m[$slice_index][0];
        confess "INTERNAL ERROR" unless $row[0] >= 0 && (@n==0 || $row[0] >= $n[$#n][0]);
        push @n, \@row;
    }

    for (my $i = 0; $i < $slice_index; $i++){
        next if $ignore_slice_note->($i);
        my @row = @{$m[$i]};
        $row[0] += $length - $m[$slice_index][0];
        confess "INTERNAL ERROR" unless $row[0] >= 0 && (@n==0 || $row[0] >= $n[$#n][0]);
        push @n, \@row;
    }

    ## Now unpack the Note_offs
    @m = @n;
    @n = ();
    for my $row (@m) {
        if ($row->[1] eq 'Note_on_c') {
            ## Push the Note_on without the length
            push @n, [@{$row}[0,1,2,3]];
            ## We push the Note_off_c onto the table out of order. But we'll sort it to make it correct.
            ## Also, we'll make sure the Note_off doesn't make the sequence longer than the specified length.
            my $future_time = $row->[0] + $row->[4];
            $future_time = $length unless $future_time <= $length;
            push @n, [$future_time, 'Note_off_c', $row->[2], 0];
        } else {
            push @n, [@$row];
        }
    }

    @m = @n;
    @n = sort sort_note_offs_first @m;

    write_midi_file(\@n, 1, $output_file);
}

sub rotate_sequence_master_clock {
    my ($input_file, $output_file, $slice_note, $slice_point, $midi) = @_;

    ## REMEMEBER: for input midi the following is true
    ##    $m[$i][0] -- Is the time the event happend
    ##    $m[$i][1] -- Is the type of event that happened
    ##    $m[$i][2] -- Is the first byte, which is the note number for any Note_on_c or Note_off_c
    ##    $m[$i][3] -- Is the velocity of the note
    my $length = sequence_length($midi);

    ## We sort the note offs first so that need_note_off_at_slice will see the note_offs before
    ## It sets it's
    my @m = sort sort_note_offs_first @{$midi->{midi}};
    my $slice_index = -1;
    for (my $i=0; $i < @m; $i++){
        if ($m[$i][0] >= $slice_point && $m[$i][2] != $slice_note && $m[$i][1] eq 'Note_on_c'){
            $slice_index = $i;
            last;
        }
    }
    confess "INTERNAL ERROR" unless $slice_index >= 0 && $slice_index < @m;

    my $ignore_slice_note = sub {
        my ($index) = @_;
        my $type = $m[$index][1];
        my $note = $m[$index][2];
        return (($type eq 'Note_on_c' || $type eq 'Note_off_c') && $note == $slice_note);
    };

    my @n;
    for (my $i = $slice_index; $i<@m; $i++){
        next if $ignore_slice_note->($i);
        my @row = @{$m[$i]};
        $row[0] -= $m[$slice_index][0];
        confess "INTERNAL ERROR" unless $row[0] >= 0 && (@n==0 || $row[0] >= $n[$#n][0]);
        push @n, \@row;
    }

    for (my $i = 0; $i < $slice_index; $i++){
        next if $ignore_slice_note->($i);
        my @row = @{$m[$i]};
        $row[0] += $length - $m[$slice_index][0];
        confess "INTERNAL ERROR" unless $row[0] >= 0 && (@n==0 || $row[0] >= $n[$#n][0]);
        push @n, \@row;
    }

    write_midi_file(\@n, 1, $output_file);
}

sub rotate_sequence {
    my ($input_file, $output_file, $slice_note, $slice_point) = @_;
    my $midi = fetch_midi($input_file);
    if ($midi->{meta}{um}){
        rotate_sequence_master_clock($input_file, $output_file, $slice_note, $slice_point, $midi);
    } else {
        rotate_sequence_one_shot($input_file, $output_file, $slice_note, $slice_point, $midi);
    }
}


sub mark_endgroup {
    my ($midi) = @_;

    my $last_index = -1;
    my @m = @{$midi->{midi}};
    for (my $i = 0; $i < @m; $i++){
        if ($m[$i][1] eq 'Note_on_c'){
            $last_index = $i;
            last;
        }
    }
    $last_index = $#m unless $last_index >= 0 && $last_index < @m;
    my $nrow = [$m[$last_index][0], 'Endgroup', 0, 0];
    splice @m, $last_index+1, 0, $nrow;
    $midi->{midi} = \@m;
}

sub mark_cycle_time {
    my ($midi) = @_;
    my $len = $midi->{length_ticks};
    my @m   = @{$midi->{midi}};
    confess "INTERNAL ERROR" unless $m[$#m][0] <= $len;
    push @m, [$len, 'Cycle', 0, 0];
    $midi->{midi} = \@m;
}

sub find_slice_notes {
    my ($filename, $slice_note) = @_;
    my $TPQN = 960;
    my @slices;
    for my $line (backtick("$MidiCsvPath $filename")){
        my ($track, $time, $command, @rest) = split ', ', $line;
        if ($TPQN != 960) {
            $time = int( ($time/$TPQN) * 960 );
        }

        if ($command eq 'Note_on_c'){
            my ($channel, $note) = @rest;
            push @slices, $time if $note == $slice_note;
        } elsif ($command eq 'Header') {
            ## NOTE: the Header command is ALWAYS the first command, so if it's present this should all work.
            my ($format, $nTracks, $division) = @rest;
            $TPQN = $division;
        }    
    }
    confess "Failed to find any slices for $slice_note" unless @slices;
    return @slices;
}


sub sequence_length {
    my ($midi) = @_;
    my $last_time = 0;
    for my $row (@{$midi->{midi}}){
        $last_time = $row->[0] if $last_time < $row->[0];
    }
    my $specified_length = $midi->{meta}{ln};
    return $last_time+1 unless defined($specified_length);
   
    my $length = 960*$specified_length;
    
    confess "The ln (Length) specifier MUST be longer than the last time of the sequence " .
            "(specified=$specified_length, end=$last_time)" unless $last_time <= $length;

    return $length;
}

sub print_integer_array_fixed_width {
    my ($list, $tab) = @_;
    $tab ||= "";
    my $l = @$list;
    my $b = "";
    for (my $i = 0; $i < $l; $i++){
        if (length($b) >= $LineLength) {
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

sub expand_comment {
    my ($s) = @_;
    $s =~ s/^(\s*)\#\#(.*)$/$1\{# $2 \}/mg;  
    return $s;
}

sub expand_symbols {
    my ($s, $h) = @_;
    $s =~ s/\[\[([^\[\]]*?)\]\]/$h->{$1}/g;
    return $s;
}

sub expand_tab {
    my ($s, $tab) = @_;
    $s =~ s/^(.*)$/$tab$1/mg;
    return $s;
}

sub expand_backtick {
    my ($s, $h) = @_;
    my $f = sub {
        my ($word) = @_;
        if ($word eq 'seq_delta'){
            return "?seq_delta[[[CG]]]"
        } else {
            return '%' . $word . '[[[CG]]]';
        }
    };
    $s =~ s/\`([_a-zA-Z0-9]+)/$f->($1)/eg;
    return $s;
}

## Velocity varies from 0 127, BUT velocity 0 is always interpreted as Note-off, so you really only have 127 levels
## With this the CENTER of the range 1 .. 127 is 64, with radius 63. This is because (64-63, 64+63) = (1, 127)
## So  $velocity + $radius*($velocity-$center)/$total_radius
## w               $radius*(127-64)/63 = $radius
##                 $radius*(1-64)/63   = -$radius
## a*(b/c) = d*a*b/(d*c) = [(d*a*b/c)/d]
## ($v-$c)/$tr = (10000*($v-$c)/$tr)/10000
##
## perl -e 'for my $v (1 .. 127){$c=($v-64)/63; printf "%10d%10d\n", $v, int(10*$c);}' 
## perl -e 'for my $v (1 .. 127){$d=($v-64); $c=$d/63; printf "%10d%10d\n", $v, int(30*$c + ($d < 0 ? -0.5 : 0.5));}' | perl -lane '$v=$F[1]; $c{$v}++; END{for $k (sort {$a <=> $b} keys(%c)){print "$k -> $c{$k}";}}' | less

## Function 
## delta = int( $r*($v-64)/63 + ($d == 0 ? 0 : $d < 0 ? -0.5 : 0.5))

## d = v-64
## res = 10000
## rnd = res*($d == 0 ? 0 : $d < 0 ? -0.5 : 0.5) = ($d == 0 ? 0 $d < 0 ? -res/2 : +res/2)
## delta = (res*r*d/63 + rnd)/res
## delta = (res*r*d/63 + ($d == 0 ? 0 $d < 0 ? -res/2 : +res/2))/res
##
## $d[[CG]] = $input_velocity[[CG]] - 64
## $rnd[[CG]] = 0
## if ($d[[CG]]<0)
##    $rnd[[CG]] = -[[RES]]/2
## end if
## if ($d[[CG]]>0)
##    $rnd[[CG]] = [[RES]]/2
## end if
## 
## $delta[[CG]] = ([[R]]*[[RES]]*d[[CG]]/63 + $rnd[[CG]])/[[RES]]
## Radius of 10 with a sequence-velocity of 100 means we want the velocity to vary evenly from 90 - 110

sub stab {
    my ($stab, %sets) = @_;
    my %nstab = (%$stab, %sets);
    return \%nstab;
} 

sub expand {
    my ($stab, $string) = @_;
    return expand_symbols(expand_backtick(expand_comment($string)), $stab);
}

sub print_expand {
    my ($stab, $string) = @_;
    print expand($stab, $string);
}

sub emit_clear_state_body {
    my ($stab) = @_;
    my $ts = <<'END';
                    ## Clear ChokeGroup if needed    
                    if (`previous_channel >= 0)
                        $stJ := 0
                        while ($stJ < 128)
                            if (%offs[`previous_channel*128 + $stJ] = [[CG]])
                                %offs[`previous_channel*128 + $stJ] := $NO_CHOKE_GROUP
                                set_midi(`previous_channel, $MIDI_COMMAND_NOTE_OFF, $stJ, 0)
                            end if
                            if (%ccs[`previous_channel*128 + $stJ] = [[CG]])
                                %ccs[`previous_channel*128 + $stJ] := $NO_CHOKE_GROUP
                                set_midi(`previous_channel, $MIDI_COMMAND_CC, $stJ, ...
                                         %background_cc[`previous_channel*128 + $stJ])
                            end if
                            inc($stJ)                    
                        end while
                        if (%pitch_bend[`previous_channel] = [[CG]])
                            %pitch_bend[`previous_channel] := $NO_CHOKE_GROUP
                            set_midi(`previous_channel, $MIDI_COMMAND_PITCH_BEND, $bend_center_msb, $bend_center_lsb)
                        end if
                    end if

                    if (`state = $PLAYER_CLEAR)
                        `state := $PLAYER_DONE
                    end if
END
    print_expand($stab, $ts);
}

sub emit_queue_state_body {
    my ($stab) = @_;
    my $ts = <<'END';
                    `previous_channel := `channel
                    `state          := $PLAYER_RUNNING
                    if (`use_master_clock = 1)
                        if ($master_clock_start = 0)
                            $master_clock_start := $ENGINE_UPTIME
                        end if
                        `start_time := $master_clock_start
                        `ticks := real_to_int( 960.0*1000.0 ...
                                                   * int_to_real($ENGINE_UPTIME-`start_time) ...
                                                   / int_to_real($DURATION_QUARTER))
                        `loop  := `ticks/`seq_length_ticks
                        ## Fast forward
                        while (`i < `end and %n[4*`i] + `loop*`seq_length_ticks < `ticks)
                            inc(`i)
                        end while
                        if (`i >= `end)
                            ## I don't think that this should ever happen, b/c there should always be a CYCLE instruction
                            ## that is at time $seq_length_ticks. But this is purely defensive.
                            inc(`loop)
                            `i := `start
                        end if
                    else
                        `start_time := $ENGINE_UPTIME
                        `loop     := 0
                    end if

                    if (`i >= `end)
                        ## The sequence is empty
                        `state := $PLAYER_DONE
                    end if
END
    print_expand($stab, $ts);
}

sub emit_run_wait_body {
    my ($stab) = @_;
    my $ts = <<'END';
                    ## Player wait if needed
                    `ticks := real_to_int( 960.0*1000.0 ...
                                               * int_to_real($ENGINE_UPTIME-`start_time) ...
                                               / int_to_real($DURATION_QUARTER))
                    `mark  := %n[4*`i] + `loop*`seq_length_ticks
                    if (`mark > `ticks)
                        wait_ticks(`mark - `ticks)
                    end if
END
    print_expand($stab, $ts);
}
sub emit_play_event_body {
    my ($stab) = @_;
    my $increment_playhead = <<'END';
                            ## Increment the playhead
                            inc(`i)
                            if (`i >= `end)
                                `state := $PLAYER_DONE
                            end if
END

    my $ts = <<'END';
                    ## Player play notes if needed        
                    select (%n[4*`i+1])
                        case $MIDI_COMMAND_NOTE_ON
                            if (%offs[`channel*128 + %n[4*`i+2]] >= 0)
                                set_midi(`channel, $MIDI_COMMAND_NOTE_OFF, %n[4*`i+2], 0)
                            end if
                            $stVelocity := %n[4*`i+3]
                            ## NOTE: $mark doesn't need to be adjusted by $loop*$seq_length_ticks b/c currently we only
                            ## support velocity adjustment for single shots (i.e. not using master clock), so $loop is 
                            ## always 0
                            if (`mark < `seq_decay_time)
                                $stVelocity := $stVelocity + real_to_int(`seq_delta ...
                                                                        * int_to_real(`seq_decay_time - `mark) ...
                                                                        / int_to_real(`seq_decay_time))
                                if ($stVelocity < 1)
                                    $stVelocity := 1
                                end if
                                if ($stVelocity  > 127)
                                    $stVelocity := 127 
                                end if                           
                            end if  
                            set_midi(`channel, %n[4*`i+1], %n[4*`i+2], $stVelocity)
                            %offs[`channel*128 + %n[4*`i+2]] := [[CG]]
                            [[INCREMENT_PLAYHEAD]]

                        case $MIDI_COMMAND_NOTE_OFF
                            if (`is_endgroup = 0)
                                set_midi(`channel, %n[4*`i+1], %n[4*`i+2], 0)
                                %offs[`channel*128 + %n[4*`i+2]] := $NO_CHOKE_GROUP
                            end if
                            [[INCREMENT_PLAYHEAD]]

                        case $MIDI_COMMAND_CC
                            set_midi(`channel, %n[4*`i+1], %n[4*`i+2], %n[4*`i+3])
                            %ccs[`channel*128 + %n[4*`i+2]] := [[CG]]
                            [[INCREMENT_PLAYHEAD]]

                        case $MIDI_COMMAND_PITCH_BEND
                            set_midi(`channel, %n[4*`i+1], %n[4*`i+2], %n[4*`i+3])
                            %pitch_bend[`channel] := [[CG]]
                            [[INCREMENT_PLAYHEAD]]

                        case $CUSTOM_COMMAND_CYCLE
                            inc(`loop)
                            `i := `start                 
                            if (`i >= `end)
                                `state := $PLAYER_DONE
                            end if

                        case $CUSTOM_COMMAND_ENDGROUP
                            if (`trigger_held >= 0)
                                `is_endgroup := 1
                            end if
                    end select
END

    open my $fd, '<', \$ts or confess "INTERNAL ERROR: can't open string for reading";
    while (<$fd>){
        if (/\[\[INCREMENT_PLAYHEAD\]\]/){
            print_expand($stab, $increment_playhead);  
        } else {
            print_expand($stab, $_);
        }
    }   
}

sub emit_midi_player_case {
    my ($stab, $choke_group) = @_;
    my $ts = <<'END';
        case [[CG]]
            `callback := $NI_CALLBACK_ID
            while (1 = 1)
                if (`state = $PLAYER_QUEUE or `state = $PLAYER_CLEAR)  
                    [[CLEAR_STATE_BODY]]
                end if

                if (`state = $PLAYER_QUEUE)  
                    [[QUEUE_STATE_BODY]]
                end if

                if (`state = $PLAYER_RUNNING)  
                    [[RUN_WAIT_BODY]]
                end if

                if (`state = $PLAYER_RUNNING)  
                    [[PLAY_EVENT_BODY]]
                end if

                while (`state = $PLAYER_DONE)
                    ## Player waits until new sequence is selected
                    wait($long_time)
                end while
            end while
END

    my %stab = (
        CG => $choke_group, 
    );

    open my $fd, '<', \$ts or confess "INTERNAL ERROR: can't open string for reading";
    while (<$fd>){
        if (/\[\[CLEAR_STATE_BODY\]\]/){
            emit_clear_state_body(\%stab);
        } elsif (/\[\[QUEUE_STATE_BODY\]\]/){
            emit_queue_state_body(\%stab);
        } elsif (/\[\[RUN_WAIT_BODY\]\]/){
            emit_run_wait_body(\%stab);
        } elsif (/\[\[PLAY_EVENT_BODY\]\]/){
            emit_play_event_body(\%stab);
        } else {
            print_expand(\%stab, $_);
        }
    }    
}

sub emit_midi_player_initialize_lattice {
    my ($stab, $midi_slots) = @_;
    
    my %need_cg;
    for my $midi (@$midi_slots){
        $need_cg{$midi->{choke_group}} = 1;
    }

    for my $cg (sort {$a <=> $b} keys(%need_cg)) {
        emit_midi_player_case($stab, $cg);
    }
}

sub emit_midi_array {
    my ($stab, $midi_slots) = @_;
    my @all_midi;
    for my $midi (@$midi_slots){
        if (!$midi->{meta}{em}){
            for my $row (@{$midi->{midi}}){
                my ($t, $command, $bb1, $bb2) = @$row;
                my $ncommand = $command_map{$command};
                confess "Failed to find command in command_map for command $command" unless defined($ncommand);
                push @all_midi, $t, $ncommand, $bb1, $bb2;
            }
        }
    }       
    print_integer_array_fixed_width(\@all_midi, "        ");
}

sub emit_note_on_for_nd {
    my ($stab, $midi) = @_;

    my $note_on_const = <<'END';
            ## NoteOn [[BB1]] [[BB2]]
            if (%offs[[[CHANNEL]]*128 + [[BB1]]] >= 0)
                set_midi([[CHANNEL]], $MIDI_COMMAND_NOTE_OFF, [[BB1]], 0)
            end if
            set_midi([[CHANNEL]], $MIDI_COMMAND_NOTE_ON, [[BB1]], [[BB2]])
            %offs[[[CHANNEL]]*128 + [[BB1]]] := $NON_DYNAMIC_CHOKE_GROUP
END

    my $note_on_vadj = <<'END';
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
            add_text_line($cons, "PETE " & $stVelocity & " " & [[BB1]] & "    " & $stVelocityDelta)
            set_midi([[CHANNEL]], $MIDI_COMMAND_NOTE_ON, [[BB1]], $stVelocity)
            %offs[[[CHANNEL]]*128 + [[BB1]]] := $NON_DYNAMIC_CHOKE_GROUP
END

    my $cc = <<'END';
            ## CC [[BB1]] [[BB2]]
            %ccs[[[CHANNEL]]*128 + [[BB1]]] := $NON_DYNAMIC_CHOKE_GROUP
            set_midi([[CHANNEL]], $MIDI_COMMAND_NOTE_CC, [[BB1]], [[BB2]])      
END

    my $ts = <<'END';
        case [[SEQ]]
            [[ALL_STOP]]
            ## Non dynamic NoteOn
            $stVelocityDelta := real_to_int([[VELOCITY_RADIUS]]*int_to_real($MIDI_BYTE_2-64)/63.0)
            add_text_line($cons, "velocity " & $MIDI_BYTE_1 & " " & [[VELOCITY_RADIUS]])
            [[NOTE_ON_ND]]
            [[CC_ND]]
END
    my %stab = (
        CHANNEL         => $midi->{channel},
        SEQ             => $midi->{seq},
        VELOCITY_RADIUS => $midi->{velocity_radius},
    );
    my @m = @{$midi->{midi}};
    open my $fd, '<', \$ts or confess "INTERNAL ERROR: can't open string for reading";
    while (<$fd>){
        if (/\[\[ALL_STOP\]\]/){
            if ($midi->{meta}{as}){
                emit_all_stop(\%stab);
            }
        } elsif (/\[\[NOTE_ON_ND\]\]/){
            for my $row (@m){
                next unless $row->[1] eq 'Note_on_c';
                $stab{BB1} = $row->[2];
                $stab{BB2} = $row->[3];
                if (($midi->{meta}{vr} || 0) == 0) {
                    print_expand(\%stab, $note_on_const);
                } else {
                    print_expand(\%stab, $note_on_vadj);
                }
                
            }
        } elsif (/\[\[CC_ND\]\]/){ 
            for my $row (@m){
                next unless $row->[1] eq 'Control_c';
                $stab{BB1} = $row->[2];
                $stab{BB2} = $row->[3];
                print_expand(\%stab, $cc);
            }
        } else {
            print_expand(\%stab, $_);
        }
    }
}

sub emit_all_stop {
    my ($stab) = @_;
    my $ts = <<'END';
            ## ALL STOP
            $stJ := 0
            while ($stJ < 16)
                if (`callback # 0)
                    %state[$stJ] := $PLAYER_CLEAR
                    stop_wait(`callback, 0)       
                end if
                inc($stJ)
            end while
END
    print_expand($stab, $ts);
}

sub emit_note_on_for_seq {
    my ($stab, $midi) = @_;

    my %stab = (
        SEQ              => $midi->{seq},
        CG               => $midi->{choke_group},
        SEQ_LENGTH_TICKS => $midi->{length_ticks},
        SEQUENCE_START   => $midi->{sequence_start},
        SEQUENCE_END     => $midi->{sequence_end},
        CHANNEL          => $midi->{channel},
        VELOCITY_RADIUS  => $midi->{velocity_radius},
        VELOCITY_DECAY   => $midi->{velocity_decay},
    );    

    my $stop_master_clock = <<'END';
            $stJ := 0
            while ($stJ < 16)
                if ($stJ # [[CG]] and %use_master_clock[$stJ] = 1 and `callback # 0)
                    %state[$stJ] := $PLAYER_CLEAR
                    stop_wait(`callback, 0)       
                end if
                inc($stJ)
            end while
END
    my $reset_master_clock = <<'END';
            $master_clock_start := 0
END


    my $use_master_clock = <<'END';
            `seq_decay_time   := 0
            `use_master_clock := 1
END

    my $dont_use_master_clock = <<'END';
            `use_master_clock := 0
            `seq_delta        := [[VELOCITY_RADIUS]]*int_to_real($MIDI_BYTE_1-64)/63.0
            `seq_decay_time   := [[VELOCITY_DECAY]]
END

    my $ts = <<'END';
        case [[SEQ]]
            [[ALL_STOP]]
            [[STOP_MASTER_CLOCK]]
            ## This is a sequence launch
            `i                := [[SEQUENCE_START]]
            `start            := [[SEQUENCE_START]]
            `end              := [[SEQUENCE_END]]
            `state            := $PLAYER_QUEUE
            `channel          := [[CHANNEL]]
            
            `seq_length_ticks := [[SEQ_LENGTH_TICKS]]
            `trigger_held     := [[SEQ]]
            `is_endgroup      := 0

            [[MASTER_CLOCK_CONFIG]]

            $stDoinit := -1
            if (`needsinit = 1)
                $stDoinit  := [[CG]]
                `needsinit := 0
            else 
                stop_wait(`callback, 0)
            end if

END

    open my $fd, '<', \$ts or confess "INTERNAL ERROR: can't open string for reading";
    while (<$fd>){
        if (/\[\[ALL_STOP\]\]/) {
            if ($midi->{meta}{as}) {
                emit_all_stop(\%stab);
            }
        } elsif (/\[\[STOP_MASTER_CLOCK\]\]/) {
            if ($midi->{meta}{sm}) {
                ## We only need to issue the stop, if we didn't just emit an all stop
                if (!$midi->{meta}{as}) {
                    print_expand(\%stab, $stop_master_clock);
                }
                print_expand(\%stab, $reset_master_clock);
            }        
        } elsif (/\[\[MASTER_CLOCK_CONFIG\]\]/) {
            if ($midi->{meta}{um}) {
                print_expand(\%stab, $use_master_clock);
            } else {
                print_expand(\%stab, $dont_use_master_clock);
            }
        } else {
            print_expand(\%stab, $_);
        }
    }
}

sub emit_note_off_for_seq {
    my ($stab, $midi) = @_;
    return unless $midi->{meta}{me};
    my $ts = <<'END';
    case [[SEQ]]
        if (`is_endgroup = 1 and `trigger_held = [[SEQ]])
            `is_endgroup := 0
            ## Setting to the empty sequence here means we'll clear existing notes, but immediatly shift into
            ## $PLAYER_DONE
            `state := $PLAYER_QUEUE
            `i   := 0
            `end := 0
            stop_wait(`callback, 0)
        end if
        `trigger_held := -1
END
    my %stab = (
        SEQ => $midi->{seq},
        CG  => $midi->{choke_group},
    );
    print_expand(\%stab, $ts);
}

sub emit_note_off_for_nd {
    my ($stab, $midi) = @_;
    my $note_off = <<'END';
            if (%offs[[[CHANNEL]]*128 + [[BB1]]] >= 0)
                set_midi([[CHANNEL]], $MIDI_COMMAND_NOTE_OFF, [[BB1]], 0)
                %offs[[[CHANNEL]]*128 + [[BB1]]] := $NO_CHOKE_GROUP
            end if
END
    my $cc = <<'END';
            if (%ccs[[[CHANNEL]]*128 + [[BB1]]] = $NON_DYNAMIC_CHOKE_GROUP)
                %ccs[[[CHANNEL]]*128 + [[BB1]]] := $NO_CHOKE_GROUP
                set_midi([[CHANNEL]], $MIDI_COMMAND_NOTE_CC, [[BB1]], %background_cc[[[CHANNEL]]*128 + [[BB1]]])
            end if  
END
    my $case = <<'END';
        case [[SEQ]]
            ## ND NoteOffs and CC reset
END

    my %stab = (
        SEQ     => $midi->{seq},
        CHANNEL => $midi->{channel},
    );

    print_expand(\%stab, $case);
    my @m = @{$midi->{midi}};
    my %ccs;
    for my $row (@m){
        if ($row->[1] eq 'Note_off_c'){
            $stab{BB1} = $row->[2];
            print_expand(\%stab, $note_off);
        } elsif ($row->[1] eq 'Control_c'){
            $ccs{$row->[2]} = 1;
        }
    }
    for my $cc (sort {$a <=> $b} keys(%ccs)){
        $stab{BB1} = $cc;
        print_expand(\%stab, $cc);
    }
}

sub emit_note_on_lattice {
    my ($stab, $midi_slots) = @_;
    for my $midi (@$midi_slots){
        if ($midi->{meta}{nd}){
            emit_note_on_for_nd($stab, $midi);
        } else {
            emit_note_on_for_seq($stab, $midi);
        }
    }
}

sub emit_note_off_lattice {
    my ($stab, $midi_slots) = @_;
    for my $midi (@$midi_slots){
        if ($midi->{meta}{nd}){
            emit_note_off_for_nd($stab, $midi);
        } else {
            emit_note_off_for_seq($stab, $midi);
        }
    }
}

sub emit_script {
    my ($stab, $midi_slots) = @_;
    my $ts = <<'END';
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

    [[CONSOLE]]

    message("")
end on

on midi_in
    ignore_midi
    $stIndex := $MIDI_CHANNEL*128 + $MIDI_BYTE_1
    ## add_text_line($cons, "" & $stIndex & "   " & $MIDI_BYTE_1)
    if ($MIDI_COMMAND = $MIDI_COMMAND_NOTE_ON and $MIDI_BYTE_2 > 0)
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

    if ($MIDI_COMMAND = $MIDI_COMMAND_NOTE_OFF or ($MIDI_COMMAND = $MIDI_COMMAND_NOTE_ON and $MIDI_BYTE_2 = 0))
        select ($stIndex)
        case -1
            ## Empty
        [[NOTE_OFF_LATTICE]]
        end select
    end if
end on

## *** META *** 
{
[[META]]
}
END
    my $n_midi = 0;
    for my $midi (@$midi_slots){
        my $mcommands = $midi->{midi};
        $n_midi += 4*scalar(@$mcommands);
    }

    $stab = stab($stab, 
            N_MIDI                  => $n_midi, 
            CUSTOM_COMMAND_CYCLE    => $CUSTOM_COMMAND_CYCLE,
            CUSTOM_COMMAND_ENDGROUP => $CUSTOM_COMMAND_ENDGROUP,
    );

    my $install_console = 1;
    open my $fd, '<', \$ts or confess "INTERNAL ERROR: can't open string for reading";
    while (<$fd>){
        if (/\[\[MIDI_ARRAY\]\]/){
            emit_midi_array({}, $midi_slots);
        } elsif (/\[\[NOTE_ON_LATTICE\]\]/){
            emit_note_on_lattice({}, $midi_slots);
        } elsif (/\[\[MIDI_PLAYER_INITIALIZE_LATTICE\]\]/){
            emit_midi_player_initialize_lattice({}, $midi_slots);
        } elsif (/\[\[NOTE_OFF_LATTICE\]\]/){
            emit_note_off_lattice({}, $midi_slots);
        } elsif (/\[\[CONSOLE\]\]/){
            if ($install_console){
                print <<'END';
    declare ui_label $cons (6,6)
    { # Add text to the console with add_text_line($cons, <TEXT>) }  
END
            }
        } elsif (/\[\[META\]\]/){
            print "pwd  = ",backtick("pwd"), "\n";
            print "time = ", now(),"\n";
        } else {
            print_expand($stab, $_);
        }
    }
}

sub main_compile {
    my ($comp_files_map, $comp_files_list) = list_compile_files();
    confess "Didn't find any midi files" if (!@$comp_files_list);

    my $include_console = 1;
    my $needed_slots = 0;
    my @midi_slots;
    my $start_ptr = 0;
    my $end_ptr   = 0;
    for my $file (@$comp_files_list){
        my $midi = fetch_midi($file);

        $start_ptr = $end_ptr;
        my $len = 1;
        if (!$midi->{meta}{em}){
            $len = sequence_length($midi);
        }
        $midi->{length_ticks} = $len;

        my $cg               = $midi->{meta}{cg};
        $cg                  = $DefaultChokeGroup unless defined($cg);
        $midi->{choke_group} = $cg;

        $midi->{velocity_radius} = 0;
        if ($midi->{meta}{vr}){
            my $vr = $midi->{meta}{vr};
            confess "Bad vr argument $vr"  unless isint($vr) && $vr > 0 && $vr < 128; 
            $midi->{velocity_radius} = $midi->{meta}{vr};
        }
        $midi->{velocity_radius} .= ".0" unless $midi->{velocity_radius} =~ /\.0$/;

        $midi->{velocity_decay} = 0;
        if ($midi->{meta}{vd}){
            my $vd = $midi->{midi}{vd};
            confess "Bad vd argument $vd" unless isint($vd);
            $midi->{velocity_decay} = 960*$vd;
        }

        my $ch = $midi->{meta}{ch};
        $ch = $DefaultChannel unless defined($ch);
        $midi->{channel} = $ch;

        my ($chan, $note) = parse_chan_note($file);
        my $need = index_from_chan_note($chan, $note)+1;
        $needed_slots = $need if ($need > $needed_slots);
        $midi->{input_channel} = $chan;
        $midi->{note}          = $note;
        $midi->{slot}          = "$chan.$note";
        $midi->{seq}           = index_from_chan_note($chan, $note);

        if ($midi->{meta}{um}){
            mark_cycle_time($midi);
        } elsif ($midi->{meta}{me}){
            mark_endgroup($midi);
        }

        $start_ptr = $end_ptr;
        if (!$midi->{meta}{em}){
            $end_ptr += @{$midi->{midi}};
        }
        $midi->{sequence_start} = $start_ptr;
        $midi->{sequence_end}   = $end_ptr;

        push @midi_slots, $midi;
    }
    emit_script({}, \@midi_slots)
}

sub main_import {
    my @abort_list;
    my @rm_list;
    my $log_block = 'Time: ' . now() . "\n";
    eval {
        my ($comp_files_map, $comp_files_list) = list_compile_files();
        my $noncomp_files = list_noncompile_files();
        @rm_list = @$noncomp_files;
        confess "No importable midi files found" unless @rm_list;
        for my $nc_file (@$noncomp_files){
            my $midi = fetch_midi($nc_file);
            my $slice_note;
            my @slice = (0);
            if (defined($midi->{meta}{sn})){
                $slice_note = note_from_text($midi->{meta}{sn});
                @slice = find_slice_notes($nc_file, $slice_note);
            }

            ## figure out which slice to use
            my @slots = find_slots_for_seqs($comp_files_map, scalar(@slice), $midi->{meta}{ic});
            
            my ($opts) = ($nc_file =~ /(_.*)\.mid$/);
            if ($opts){
                ## Remove import options
                $opts =~ s/_sn[^_]*//;
                $opts =~ s/_ic[^_]*//;
            }

            for (my $i = 0; $i < @slice; $i++) {
                my $do_slice = ($slice[$i] != 0);
                my ($chan, $note) = parse_chan_note($slots[$i]);
                my $options = $opts;
                $options .= ("_sl" . $slice[$i]) if $do_slice;
                my $nfile = name_midi_file($chan, $note, $options);
                push @abort_list, $nfile;
                $comp_files_map->{$slots[$i]} = $nfile;
                rotate_sequence($nc_file, $nfile, $slice_note, $slice[$i]);
            }
        }
    };
    if ($@){
        run "rm -f @abort_list";
        confess $@;
    } else {
        run "rm @rm_list";
    }
}


sub main {
    my ($command) = @ARGV;
    shift @ARGV if @ARGV;
    $command = "compile" unless defined($command);
    
    if ($command eq 'compile'){
        main_compile();
        return;
    } elsif ($command eq 'import'){
        main_import();
        return;
    } 

    confess "Unknown command $command";
}

main();
