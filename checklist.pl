
## Tool that takes midi file, and slice_note, and produces a new file
## $LEX.$CHANNEL.$NOTE$SIGNED_TRANSPOSE.<user-text-with-no_><options>_sl960.mid 
## Here $SIGNED_TRANSPOSE and $NOTE are redundant. $SIGNED_TRANSPOSE is 
## a positive or negative offset from C3. It's used in maschine. $SIGNED_TRANSPOSE
## always starts with a + or - (+0). 

my @Letters = ("A" .. "Z");
my @NoteSymbols = qw/ C   C#  D  D#  E   F   F#  G  G#  A A# B  /;



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
