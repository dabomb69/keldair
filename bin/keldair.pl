#!/usr/bin/perl
#
#       core.pl
#       
#       Copyright 2010 Chazz Wolcott <root@spartairc.org>
#
#
#       Redistribution and use in source and binary forms, with or without
#       modification, are permitted provided that the following conditions are
#       met:
#       
#       * Redistributions of source code must retain the above copyright
#         notice, this list of conditions and the following disclaimer.
#       * Redistributions in binary form must reproduce the above
#         copyright notice, this list of conditions and the following disclaimer
#         in the documentation and/or other materials provided with the
#         distribution.
#       * Neither the name of the Sparta Project nor the names of its
#         contributors may be used to endorse or promote products derived from
#         this software without specific prior written permission.
#       
#       THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#       "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#       LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#       A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#       OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#       SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#       LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#       DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#       THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#       (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#       OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

use strict;
use warnings;
use lib 'lib';
use IO::Socket;
use Config::Scoped;

my $parser = Config::Scoped->new(
		file => 'etc/keldair.conf',
		) or die("Cannot open config file!\n");

my $SETTINGS = $parser->parse;

my $sock = IO::Socket::INET->new(	
		Proto           => "tcp",	
		PeerAddr        => $SETTINGS->{'server'}->{'host'},	
		PeerPort        => $SETTINGS->{'server'}->{'port'},	
		) or die("Connection failed to $SETTINGS->{'server'}->{'host'}. \n");

my ($line,$nickname,$command,$mtext,$hostmask,$channel,@spacesplit);

# Ok, I do believe connecting is important, eh? :P
snd('USER '.$SETTINGS->{'keldair'}->{'ident'}.' * * :'.$SETTINGS->{'keldair'}->{'real'});
snd("NICK ".$SETTINGS->{'keldair'}->{'nick'});

while ($line = <$sock>) {

# First, since this is a loop, we undefine all the special and REALLY IMPORTANT variables!
	undef $nickname;
	undef $command;
	undef $mtext;
	undef $hostmask;

# Mkay, now let's kill off those \r\n's at the end of $line.
	chomp($line);
	chomp($line);

# Hey, let's print the line too!
	print($line."\r\n");

# Now, time to /extract/ those there variables!
	$hostmask = substr($line,index($line,":"));
	$mtext = substr($line,index($line,":",index($line,":")+1)+1);
	($hostmask, $command) = split(" ",substr($line,index($line,":")+1));
	($nickname,undef) = split("!",$hostmask);

	@spacesplit = split(" ",$line);
	$channel = $spacesplit[2];

# Now, time for some commandish stuff!

	if ($command eq '001') {
		snd("JOIN ".$SETTINGS->{'channels'}->{'debug'}.",".$SETTINGS->{'channels'}->{'general'});
	}

	if ($command eq 'PRIVMSG') {
	}
}


# Oh look, a nice empty chunk of land for the really important subs!!!

sub snd {
	my ($text) = @_;
	chomp ($text);
	print("SEND: $text\r\n");
	send($sock,$text."\r\n",0);
	return;
}

sub msg {
	my ($target,$text) = @_;
	snd("PRIVMSG ".$target." :".$text);
}

sub notice {
	my ($target,$text) = @_;
	snd("NOTICE ".$target." :".$text);
}

sub ctcp {
	my ($ctcp,$target) = @_;
	snd("PRIVMSG ".$target." :\001".$text."\001");
}


