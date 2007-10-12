package Acme::COBOL;
use strict;
use warnings FATAL => 'all';

our $VERSION = '0.01';

use base qw(Class::Accessor);
use File::Slurp qw(read_file);
__PACKAGE__->follow_best_practice;
__PACKAGE__->mk_accessors(qw(state current_row));

sub parse {
    my ($self, $file) = @_;
    my @lines = read_file($file);

    $self->set_state('');
    $self->set_current_row('');
    
    foreach my $line (@lines) {
        my $line_number = substr($line, 0,7, "");
        if ($line_number =~ /\S/) {
            if ($self->current_row >= $line_number) {
                die "ERROR in $. row number is not growing\n";
            }
        }
        next if substr($line, 7,1, "") eq "*";
        if ($line 
        
    }
}

sub run_file {
    my ($self, $file) = @_;

    $self->parse($file);

}


1;


