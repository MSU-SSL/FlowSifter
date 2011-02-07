#!/usr/bin/env perl

use warnings;
use strict;

use SOAP::Transport::HTTP;

# don't want to die on 'Broken pipe' or Ctrl-C
$SIG{PIPE} = $SIG{INT} = 'IGNORE';

my $daemon = SOAP::Transport::HTTP::Daemon
    -> new (LocalPort => 8080)
    -> dispatch_to('Demo')
    ;

print "Contact to SOAP server at ", $daemon->url, "\n";
$daemon->handle;


package Demo;

my $hi = 0;
my $bye = 0;

sub hi {      
    $hi++;
    return "hello, world";     
}

sub bye {  
    $bye++;
    return "goodbye, cruel world";
}

sub counts {
    return {hi => $hi, bye=> $bye};
}

