package Acme::COBOL;
use strict;
use warnings FATAL => 'all';

our $VERSION = '0.01';

use English qw( -no_match_vars );

use base qw(Class::Accessor);
use File::Slurp qw(read_file);
__PACKAGE__->follow_best_practice;
__PACKAGE__->mk_accessors(qw(state section current_row config));

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

my %paragraph;
my $current_paragraph;
my %vars;

my $VAR_NAME_REGEX = qr/[\da-zA-Z-]+/;
my $EXPRESSION_REGEX = qr/$VAR_NAME_REGEX\s*  \+ \s*$VAR_NAME_REGEX/x;

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
            if ($line =~ /^\s*$/) { $row++; next; } # plain empty line

            my $line_number = substr($line, 0,6, "");
            #$self->_check_line_numbers($line_number);

            my $indicator = substr($line, 0,1, "");
            if ($indicator eq '*') {$row++; next;}
            if ($indicator ne ' ') {
                error("Incorrect indicator '$indicator' in line $row\n$lines[$row]");
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
            error("missing IDENTIFICATION DIVISION");
        }
    }

    if ($self->get_state eq "identification_division") {
        if ($sentence eq "PROGRAM-ID") {
            if ($requirements{program_id}) {
                error("PROGRAM-ID was already set");
            }
            $self->set_state("program_id");
            return;
        }
        if ($sentence =~ /^ENVIRONMENT\s+DIVISION\s*$/) {
            $requirements{environment_division} = 1;
            $self->set_state("environment_division");
            return;
        }
        error("Not processed sentence in IDENTIFICATION DIVISION: '$sentence'")
    }

    if ($self->get_state eq "environment_division") {
        if ($sentence =~ /^DATA\s+DIVISION\s*$/) {
            $requirements{data_division} = 1;
            $self->set_state("data_division");
            return;
        }
        error("Not processed sentence in ENVIRONMENT DIVISION: '$sentence'")
    }

    #  01 Name PIC X(5).
    #  02 X2   PIC 9(3).
    if ($self->get_state eq "data_division") {
        my $section = $self->get_section;
        if ($section and $section eq "working_storage") {
            # if ($sentence =~ m/01  \s+ ($VAR_NAME_REGEX) \s+ PIC(?:TURE)?  \s+   ([X()\d])+   $/x) { 
                my ($name, $pic, $cnt) = split / +/, $sentence;
                if ($name =~ /^\d+$/) {
                    error("Variable must contain at least one letter or hypen: '$sentence'");
                }
                if ($vars{$name}) {
                    error("Variable '$name' already defined");
                }
                $vars{$name}{picture} = $pic x $cnt;
                return;
            # }
            #error("Not processed sentence in DATA DIVISION, WORKING STORAGE section: '$sentence'")
        }

        if ($sentence =~ /^PROCEDURE\s+DIVISION\s*$/) {
            $requirements{procedure_division} = 1;
            $self->set_state("procedure_division");
            return;
        }
        if ($sentence =~ /^WORKING-STORAGE\s+SECTION$/) {
            $self->set_section("working_storage");
            return;
        }
        error("Not processed sentence in DATA DIVISION: '$sentence'")
    }

    # special treatment as PROGRAM-ID. is sorta sentence itself
    if ($self->get_state eq "program_id") {
        $sentence =~ s/^\s+//;
        if ($sentence =~ m/^[A-Z-]+$/ix) {
            # TODO: optionally require to be the same as the name of the file
            $requirements{program_id} = $sentence;
            $self->set_state("identification_division");
            return;
        } else {
            error("Invalid program_id '$sentence'");
        }
    }

    if ($self->get_state eq "procedure_division") {
        # TODO: what about     DISPlAY "hello \"foo\" world"
        # TODO: and about      DISPLAY "hello \\foo"
        if ($sentence =~ m/^\s{4,}DISPLAY\b/x) {
            (my $disp = $sentence) =~ s/^\s{4,}DISPLAY//x;
            my $str = '';
            my $last = 0;
            while ($disp =~ m/(?:\s+|\s*,\s*)   (
                              "([^"]*)"                  # string
                              |  (\d+(?:\.\d+)?)         # number
                              |   ($VAR_NAME_REGEX)      # variable
                              ) /gx) {
                #$str .= $LAST_PAREN_MATCH;
                if (defined $2) {
                    $str .= $2;
                } elsif (defined $3) {
                    $str .= $3;
                } elsif (defined $4) {
                    my $name = $4;
                    # padding of strings
                    if ($vars{$name}{picture} =~ m/^X+$/x) {
                        my $width = length $vars{$name}{picture};
                        $str .= sprintf("%-${width}s", $vars{$name}{value});
                    } elsif ($vars{$name}{picture} =~ m/^9+$/x) {
                        my $width = length $vars{$name}{picture};
                        $str .= sprintf("%-${width}d", $vars{$name}{value});
                        #$str .= $vars{$name}{value};
                    } else {
                        error("Invalid picture '$vars{$name}{picture}'");
                    }
                } else {
                    error("Invalid part in the DISPLAY row: \n'$sentence'")
                }
                $last = $LAST_MATCH_END[1];
            }
            if ($last < length($disp)) {
                error("Invalid DISPLAY: '$sentence'\nLAST_MATCH_END:$last length: " .  length($disp));
            }
            print "$str\n";
            return;
        }
        if ($sentence =~ m/^\s{4,}STOP\s+RUN$/x) {
            $requirements{stop_run}++;
            exit;
        }
        if ($sentence =~ m/^[A-Z-]+$/x) {
            if ($paragraph{$sentence}) {
                error("Paragraph '$sentence' was already defined");
            }
            $paragraph{$sentence} = 1;
            $current_paragraph = $sentence;
            return;
        }
        if ($sentence =~ m/^\s{4,}  ACCEPT \s+ ($VAR_NAME_REGEX)$/x) {
            my $name = $1;
            die "Variable '$name' was not declared. '$sentence'" if not exists $vars{$name};
            chomp($vars{$name}{value} = <STDIN>);

            # TODO: check if the value fits the picture?
            if ($vars{$name}{picture} =~ /X/ and length($vars{$name}{value}) > length($vars{$name}{picture})) {
                $vars{$name}{value} = substr($vars{$name}{value}, 0, length $vars{$name}{picture});
            }

            return;
        }
        if ($sentence =~ m/^\s{4,}  COMPUTE \s+ ($VAR_NAME_REGEX) \s+ = \s+ ($EXPRESSION_REGEX)/x) {
            my ($name, $expr) = ($1, $2);
            $expr =~ s/($VAR_NAME_REGEX)/$vars{$1}{value}/gx;
            $vars{$name}{value} = eval $expr;
            return;
        }

        error("Not processed sentence in PROCEDURE DIVISION: '$sentence'")
    }

    error("Sentence not processed: '$sentence'");
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
    return;
}

# should compile a COBOL file and create an executable Perl file?
sub compile_file {
    my ($self) = @_;
    return;
}

sub error {
    my ($msg) = @_;
    die "ERROR: $msg\n";
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

Handle floating point numbers as well as strings with . in them.

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


