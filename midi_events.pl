use strict;
use Getopt::Std;
use Carp;
my $MidiCsvPath = "$ENV{HOME}/CeeGuitar/midicsv-1.1/midicsv";

my @NoteSymbols = qw/ C   C#  D  D#  E   F   F#  G  G#  A A# B  /;

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

sub backtick {
    my ($command) = @_;
    my @lines = `$command`;
    if ($?) {
        confess "Failed '$command': $!";
    }
    chomp(@lines);
    return @lines;
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

## ABOVE THIS LINE ARE BORROWED

sub print_divider {
    my ($time) = @_;
    my $hashs = 15;
    printf "%s %.2f %s\n", ("#" x $hashs), $time, ("#" x $hashs);
}

sub dump_midi {
    my ($midi) = @_;
    my @m = @{$midi->{replicated_midi}};

    ## We want any event that touches from 0 -> length_ticks
    my $start = 0;
    my $end   = $midi->{length_ticks}/960.0;
    my @n;
    for my $row (@m){
        if ($row->[1] eq 'Note_on_c'){
            my $s = $row->[0];
            my $e = $row->[0] + $row->[4];
            push @n, [@$row] if ($start <= $s && $s < $end) || ($start <= $e && $e < $end);
        } else {
            push @n, [@$row] if $start <= $row->[0] && $row->[0] < $end;
        }
    }
    
    my $divider_delta = 1.0;
    my $next = 0;
    for my $row (@n) {
        my $t = $row->[0];
        while ($next <= $t) {
            print_divider($next);
            $next += 1.0;
        }
        my $b = "";
        if (defined($row->[4])){
            $b = sprintf "%.2f", $row->[4];
        }
        my $nname = $NoteSymbols[$row->[2] % 12] . int($row->[2]/12-2);
        printf "%15.2f%15s%15d%15s%15d%15s\n", $t, @{$row}[1,2], $nname, $row->[3], $b;
    }
}

sub normalize_midi {
    my ($midi) = @_;

    my @m = sort sort_note_offs_first @{$midi->{midi}};
    my @n;
    my %pending;
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
    for my $row (@n){
        ## Want all time in quarter-note-floating
        $row->[0] /= 960.0;
        if (defined($row->[4])){
            $row->[4] /= 960.0;
        }
    }

    $midi->{midi} = \@n;
}

sub replicate_midi {
    my ($midi) = @_;
    my @m      = @{$midi->{midi}};
    my @n;
    for my $row (@m){
        my @nrow = @$row;
        $nrow[0] -= $midi->{length_ticks}/960.0;
        push @n, \@nrow;
    }
    for my $row (@m){
        my @nrow = @$row;
        push @n, \@nrow;
    }
    for my $row (@m){
        my @nrow = @$row;
        $nrow[0] += $midi->{length_ticks}/960.0;
        push @n, \@nrow;
    }
    $midi->{replicated_midi} = \@n;
}

sub main {
    my %opts;
    getopts("h", \%opts);
    die "Failed to specify file" unless @ARGV > 0;
    my ($filename) = @ARGV;
    my $midi = fetch_midi($filename);
    $midi->{length_ticks} = sequence_length($midi);
    normalize_midi($midi);
    replicate_midi($midi);
    dump_midi($midi);
}


main();

