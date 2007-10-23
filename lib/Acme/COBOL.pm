package Acme::COBOL;
use strict;
use warnings FATAL => 'all';

our $VERSION = '0.01';

use base qw(Class::Accessor);
use File::Slurp qw(read_file);
__PACKAGE__->follow_best_practice;
__PACKAGE__->mk_accessors(qw(state current_row config));

my $row;
my @lines;
my $left_over = '';
my %requirements = (
    identification_division => 0,
    program_id              => '',
    environment_division    => 0,
    data_division           => 0,
    procedure_division      => 0,
    stop_run                => 0,
);


sub new {
    my ($class, %config) = @_;
    my $self = bless {}, $class;
    $self->set_config(\%config);
    return $self;
}

sub parse {
    my ($self, $file) = @_;
    @lines = read_file($file);
    chomp @lines;

    $self->set_state('');
    $self->set_current_row('');
    
    $row = 0;
    while (my $sentence = $self->_next_sentence()) {
        $self->_parse_sentence($sentence);
    }

    foreach my $k (keys %requirements) {
        if (not $requirements{$k}) {
            die "Missing $k\n"; 
        }
    }
}

# TODO: check full line length
# TODO: check if the keywords start in the correct place.
sub _next_sentence {
    my ($self) = @_;
    return if $row >= @lines;
    my $end_of_sentence = 0;

    my $sentence = '';
    while (not $end_of_sentence) {
        my $line = $lines[$row];
        if ($left_over) {
            $line = $left_over;
            $left_over = '';
        } else {
            my $line_number = substr($line, 0,6, "");
            #$self->_check_line_numbers($line_number);

            my $indicator = substr($line, 0,1, "");
            if ($indicator eq '*') {$row++; next;}
            if ($indicator ne ' ') {
                die "ERROR: Incorrect indicator '$indicator' in line $row\n$lines[$row]\n";
            }

            if ($line =~ /^\s*$/)  {$row++; next;}
        }

        my $loc = index($line,  '.');
        if ($loc >= 0) {
            $sentence .= substr($line, 0, $loc, "");
            $line = substr($line, 1);
            if ($line =~ /\S/) {
                $left_over = $line;
            } else {
                $row++;
            }
            last;
        } else {
            $sentence .= $line;
            $row++;
        }
    }
 
    return $sentence;
}


sub _parse_sentence {
    my ($self, $sentence) = @_;

    if ($self->get_config->{parse_debug}) {
        print "'$sentence'\n";
        return;
    }
    # TODO maybe add a debugging option to print sentences?
    # and then we can create .parse files in the examples directory to hold the expected output of this
    # parsing.

    if (not $self->get_state) {
        if ($sentence =~ m/^IDENTIFICATION\s+DIVISION\s*$/x) {
            $self->set_state("identification_division");
            $requirements{identification_division} = 1;
            return;
        } else {
            die "ERROR: missing IDENTIFICATION DIVISION\n";
        }
    }

    if ($self->get_state eq "identification_division") {
        if ($sentence eq "PROGRAM-ID") {
            if ($requirements{program_id}) {
                die "ERROR: PROGRAM-ID was already set\n";
            }
            $self->set_state("program_id");
            return;
        }
        # TODO else error??
    }

    if ($self->get_state eq "program_id") {
        $sentence =~ s/^\s+//;
        if ($sentence =~ m/^[A-Z-]+$/x) {
            # TODO: optionally require to be the same as the name of the file
            $requirements{program_id} = $sentence;
            $self->set_state("identification_division");
        } else {
            die "ERROR: Invalid program_id '$sentence'\n";
        }
    }

    return;
}

#sub _check_line_numbers {
#        if ($line_number =~ /\S/) {
#            if ($self->current_row >= $line_number) {
#                die "ERROR in $. row number is not growing\n";
#            }
#        }
#        if ($line 
#    }

sub run_file {
    my ($self, $file) = @_;

    $self->parse($file);
}

# should compile a COBOL file and create an executable Perl file?
sub compile_file {
    my ($self) = @_;
}

=head1 NAME

Acme::COBOL - A COBOL interpreter

=head1 SYNOPSIS

See L<acme-cobol>

=head1 DESCRIPTION

See the COBOL standards, here are some issues we have to consider

=head1 COBOL

=head2 Standards

See the various COBOL standards. Decide which one to implement or
if to allow switches for the various implementations?

=head2 DIVISIONS

COBOL programs have 4 DIVISIONS:

 IDENTIFICATION DIVISION.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 PROCEDURE DIVISION.

We require all fur divisions to be included.
In a relaxed version we can leave out the ENVIRONMENT and DATA divisions if
they are really not needed.

End of program is indicated by

 STOP RUN.

The IDENTIFICATION DIVISION must contain a declaration of PROGRAM-ID:
Where the actual ID needs to be upper case letters and dash. 
(In relaxed version we can also allow lower case name)
Preferably the name should be the same as the filename.

 PROGRAM-ID. ACMECOBOL. 

=head2 rows 

COBOL programs have 5 sections in each row

=over 4

=item position 1-6 sequence number area

should be either empty or a growing sequence of numbers, 

We should disregard them and optionally give warnings if they are missing,
are not just numbers or if they are out of growing order.

=item position 7 indicator area

should be blank

if it is an asterix  (*) the resto of the row is a comments

Anything else should be disregarded and (optionally?) reported as error.

=item position 8-11 Area A

DIVISIONS and paragraphs should start in this area. Preferable on position 8 
(if not, we should optionally give an error message)
They can extend to Area B.


=item position 12-72 Area B

Sentences must start and end in Area B. Preferably on position 12.

=item position 73-80 identification area

Disregard by the compiler, place for comments by the editors.

=item position 81-

If there is anything in this area that should be reported as error.

=back

=head1 TODO

Examples of code with syntax error and see how the compiler copes with that.

e.g. missing DIVISIONS, sentences starting in Area A and not in Area B,
Lines too long

Use various Cobol versions implemented as dialects in cobc -std=<dialect>

=head1 See Also

L<http://www.perlmonks.org/?node_id=18048>

L<http://www.perl.com/pub/a/2000/05/cobol.html>

=head1 Copyright

Copyright 2007 by Gabor Szabo <gabor@pti.co.il>.

This program is free software; you can redistribute it and/or modify it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut

1;


