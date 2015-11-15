package Net::Telnet::ERX;

#-----------------------------------------------------------------
# Net::Telnet::ERX - interact with a ERX router
#
# $Id: ERX.pm,v 1.00 2003/04/16 jstankus Exp $
#
# Todo: only supports password login and enable password
#
# POD documentation at end of file.
#
# This module borrows heavily from Net::Telnet and Net::Telnet::Cisco
#
#-----------------------------------------------------------------

require 5.005;

use strict;
use Net::Telnet 3.02;
use AutoLoader;
use Carp;

use vars qw($AUTOLOAD @ISA $VERSION $DEBUG);

@ISA      = qw(Net::Telnet);
$VERSION  = '1.0';
$^W       = 1;
$DEBUG    = 0;
$|++;

#------------------------------
# Public Methods
#------------------------------

#---------------
# good prompt except when shelling to a slot
# '/(?m:[\w.-]+\s?(?:\(support[^\)]*\))?(?:\(config[^\)]*\))?\s?[\$#>])/')
# adds shelling to slot
# '/(?m:(slot\s*)?[\w.-]+\s?(?:\(support[^\)]*\))?(?:\(config[^\)]*\))?\s?[\$#>])/'
# adds support for ... dropping characters... , but you have to send a '' to the router after to get back in sync
# '/(?m:((slot\s*)?[\w.-]+\s?(?:\(support[^\)]*\))?(?:\(config[^\)]*\))?\s?[\$#>]))|(\.+dropping\s*chars\.+)/')
#---------------


sub new {
    my $class = shift;

    my ($self, $host, %args);

#     '/(?m:[a-zA-Z_0-9]+[#>]$)/')
    # Add default prompt to args if none present.
#    push @_, (-Prompt => '/(?m:((slot\s*)?[\w.-]+\s?(?:\(support[^\)]*\))?(?:\(config[^\)]*\))?\s?[\$#>]))|(\.+dropping\s*chars\.+)/')
#push @_, (-Prompt => '/(?m:((slot\s+\d+\->)|(\->)|(\.+dropping\s*chars\.+)|[\w.-]+\s?(?:\(support\))?(?:\(config[^\)]*\))?\s?[\$#>]))/')
push @_, (-Prompt => '/(?m:((slot\s+\d+\->)|(\->)|(\.+dropping\s*chars\.+)|([\w.-]+\s*(?:\(support\))?(?:\(config[^\)]*\))?\s*[\$#>])))/')
	unless grep /^-?prompt$/i, @_;

    # There's a new cmd_prompt in town.
    $self = $class->SUPER::new(@_) or return;

    *$self->{net_telnet_erx} = {
	last_prompt	       => '',
        last_cmd	       => '',

        always_waitfor_prompt  => 1,
	waitfor_pause	       => 0.1,

	autopage	       => 1,

	more_prompt	       => '/(?m:^\s*--More--)/',

	normalize_cmd	       => 1,

	send_wakeup 	       => 0,

	ignore_warnings	       => 0,
	warnings	       => '/(?mx:^% Unknown VPN
				     |^%IP routing table VRF.* does not exist. Create first$
				     |^%No CEF interface information
				     |^%No matching route to delete$
				     |^%Not all config may be removed and may reappear after reactivating/
				   )/',
    };

    ## Parse the args.
    if (@_ == 2) {  # one positional arg given
        $host = $_[1];
    } elsif (@_ > 2) {  # named args
        ## Get the named args.
        %args = @_;

        ## Parse the errmode named arg first.
        foreach (keys %args) {
            $self->errmode($args{$_})
                if /^-?errmode$/i;
        }

        ## Parse all other named args.
        foreach (keys %args) {
            if (/^-?always_waitfor_prompt$/i) {
                $self->always_waitfor_prompt($args{$_});
            }
            elsif (/^-?waitfor_pause$/i) {
                $self->waitfor_pause($args{$_});
	    }
            elsif (/^-?more_prompt$/i) {
                $self->more_prompt($args{$_});
	    }
            elsif (/^-?autopage$/i) {
                $self->autopage($args{$_});
	    }
            elsif (/^-?normalize_cmd$/i) {
                $self->normalize_cmd($args{$_});
	    }
	    elsif (/^-?send_wakeup$/i) {
		$self->send_wakeup($args{$_});
	    }
	}
    }

    $self;
} # end sub new


# The new prompt() stores the last matched prompt for later
# fun 'n amusement. You can access this string via $self->last_prompt.
#
# It also parses out any router errors and stores them in the
# correct place, where they can be acccessed/handled by the
# Net::Telnet error methods.
#
# No POD docs for prompt(); these changes should be transparent to
# the end-user.
sub prompt {
    my( $self, $prompt ) = @_;
    my( $prev, $stream );

    $stream  = $ {*$self}{net_telnet_erx};
    $prev    = $self->SUPER::prompt;




    ## Parse args.
    if ( @_ == 2 ) {
        defined $prompt or $prompt = '';
	$self->_match_check($prompt);
	$self->SUPER::prompt($prompt);
    } elsif (@_ > 2) {
        return $self->error('usage: $obj->prompt($match_op)');
    }

    return $prev;
} # end sub prompt

# cmd() now parses errors and sticks 'em where they belong.
#
# This is a routerish error:
#   routereast#show asdf
#                     ^
#   % Invalid input detected at '^' marker.
#
# "show async" is valid, so the "d" of "asdf" raised an error.
#
# If an error message is found, the following error message
# is sent to Net::Telnet's error()-handler:
#
#   Last command and router error:
#   <last command prompt> <last command>
#   <error message fills remaining lines>
sub cmd {
    my $self             = shift;
    my $ok               = 1;

    my $normalize	 = $self->normalize_cmd;

    # Parse args
    if (@_ == 1) {
	$ {*$self}{net_telnet_erx}{last_cmd} = $_[0];
    } elsif ( @_ >= 2 ) {
	my @args = @_;
	while (my ($k, $v) = splice @args, 0, 2) {
	    $ {*$self}{net_telnet_erx}{last_cmd} = $v if $k =~ /^-?[Ss]tring$/;
	    $normalize = $v if $k =~ /^-?[Nn]ormalize_cmd$/;
	}
    }

    my $cmd		 = $ {*$self}{net_telnet_erx}{last_cmd};
    my $old_ors		 = $self->output_record_separator;
    my $need_more	 = 0;
    my @out;

    while(1) {
	# Send a space (with no newline) whenever we see a "More" prompt.
	if ($need_more) {
	    $self->output_record_separator('');

	    # We saw a more prompt, so put it in the command output.
	    my @tmp = $self->last_prompt;

	    # Send the <space>, taking care not to
	    # discard the top line.
	    push @tmp, $self->SUPER::cmd(String => " ", Cmd_remove_mode => 0);

	    if ($self->normalize_cmd) {
		push @out, _normalize(@tmp);
	    } else {
		push @out, @tmp;
	    }
	} else {
	    $self->output_record_separator($old_ors);
	    push @out, $self->SUPER::cmd(@_);
	}

	# Look for errors in output
	for ( my ($i, $lastline) = (0, '');
	      $i <= $#out;
	      $lastline = $out[$i++] ) {

	    # This may have to be a pattern match instead.
	    if ( ( substr $out[$i], 0, 1 ) eq '%' ) {
		if ( $out[$i] =~ /'\^' marker/ ) { # Typo & bad arg errors
		    chomp $lastline;
		    $self->error( join "\n",
				  "Last command and router error: ",
				  ( $self->last_prompt . $cmd ),
				  $lastline,
				  $out[$i],
				);
		    splice @out, $i - 1, 3;
		} else { # All other errors.
		    chomp $out[$i];
		    $self->error( join "\n",
				  "Last command and router error: ",
				  ( $self->last_prompt . $cmd ),
				  $out[$i],
				);
		    splice @out, $i, 2;
		}
		$ok = 0;
		last;
	    }
	}

	# Restore old settings
	$self->output_record_separator($old_ors);
  
	my $dropping_re = $self->re_sans_delims('/(?m:\.+dropping\s*chars\.+)/');
        if ( $self->last_prompt =~ /$dropping_re/) {
      print "Found dropping chars !!\n";
	    $need_more = 1;
	} 


	# redo the while loop if we saw a More prompt.
	my $more_re = $self->re_sans_delims($self->more_prompt);
	if ($self->autopage && $self->last_prompt =~ /$more_re/) {
	    $need_more = 1;
	} 

          else
        {
	    last;
	}
    }

    return wantarray ? @out : $ok;
}


# waitfor now stores prompts to $obj->last_prompt()
sub waitfor {
    my $self = shift;

    return unless @_;

    # $all_prompts will be built into a regex that matches all currently
    # valid prompts.
    #
    # -Match args will be added to this regex. The current prompt will
    # be appended when all -Matches have been exhausted.
    my $all_prompts = '';

    # Literal string matches, passed in with -String.
    my @literals = ();

    # Parse the -Match => '/prompt \$' type options
    # waitfor can accept more than one -Match argument, so we can't just
    # hashify the args.
    if (@_ >= 2) {
	my @args = @_;
	while ( my ($k, $v) = splice @args, 0, 2 ) {
	    if ($k =~ /^-?[Ss]tring$/) {
		push @literals, $v;
	    } elsif ($k =~ /^-?[Mm]atch$/) {
		$all_prompts = $self->prompt_append($all_prompts, $v);
	    }
	}
    } elsif (@_ == 1) {
	# A single argument is always a -match.
	$all_prompts = $self->prompt_append($all_prompts, $_[0]);
    }

    my $all_re	   = $self->re_sans_delims($all_prompts);
    my $prompt_re  = $self->re_sans_delims($self->prompt);
    my $more_re    = $self->re_sans_delims($self->more_prompt);


    # Add the current prompt if it's not already there. You can turn this behavior
    # off by setting always_waitfor_prompt to a false value.
    if ($self->always_waitfor_prompt && index($all_re, $prompt_re) == -1) {
	unshift @_, "-Match" if @_ == 1;
	push @_, (-Match => $self->prompt);

	$all_prompts  = $self->prompt_append($all_prompts, $self->prompt);
	$all_re	      = $self->re_sans_delims($all_prompts);
    }

    # Add the more prompt if it's not present. See the autopage() docs
    # to turn this behaviour off.
    if ($self->autopage && index($all_re, $more_re) == -1) {
	unshift @_, "-Match" if @_ == 1;
	push @_, (-Match => $self->more_prompt);

	$all_prompts  = $self->prompt_append($all_prompts, $self->more_prompt);
	$all_re	      = $self->re_sans_delims($all_prompts);
    }

# print "--> all_prompts $all_prompts \n\n";
# print "--> all_re $all_re \n\n";


    return $self->error("Godot ain't home - waitfor() isn't waiting for anything.")
	unless $all_prompts || @literals;

    # There's a timing issue that I can't quite figure out.
    # Adding a small pause here seems to make it go away.
    select undef, undef, undef, $self->waitfor_pause;
&_sleep(0.01);


    my ($prematch, $match) = $self->SUPER::waitfor(@_);

# print "--> prematch: $prematch \n\n";
# print "--> match: $match \n\n";

    # If waitfor saw a prompt then store it.
    if ($match) {
	for (@literals) {
	    if (index $match, $_) {
		return wantarray ? ($prematch, $match) : 1;
	    }
	}

	if ($match =~ /($all_re)/m ) {
	    $ {*$self}{net_telnet_erx}{last_prompt} = $1;
	    return wantarray ? ($prematch, $match) : 1;
	}
    }
    return wantarray ? ( $prematch, $match ) : 1;
}


sub login {
    my($self) = @_;
    my(
       $cmd_prompt,
       $endtime,
       $error,
       $lastline,
       $match,
       $orig_errmode,
       $orig_timeout,
       $prematch,
       $reset,
       $timeout,
       $usage,
       $sent_wakeup,
       );
    my ($username, $password, $tacpass, $passcode ) = ('','','','');
    my (%args, %seen);

    local $_;

    ## Init vars.
    $timeout = $self->timeout;
    $self->timed_out('');
    return if $self->eof;
    $cmd_prompt = $self->prompt;
    $sent_wakeup = 0;

    print "login:\t[orig: $cmd_prompt]\n" if $DEBUG;

    $usage = 'usage: $obj->login([Name => $name,] [Password => $password,] '
	   . '[Passcode => $passcode,] [Prompt => $matchop,] [Timeout => $secs,])';

    if (@_ == 3) {  # just username and passwd given
	($username, $password) = (@_[1,2]);
    }
    else {  # named args given
	## Get the named args.
	(undef, %args) = @_;

	## Parse the named args.
	foreach (keys %args) {
	    if (/^-?name$/i) {
		$username    = $args{$_};
	    } elsif (/^-?passw/i) {
		$password    = $args{$_};
	    } elsif (/^-?passcode/i) {
		$passcode    = $args{$_};
	    } elsif (/^-?prompt$/i) {
		# login() always looks for a cmd_prompt. This is not
		# controllable via always_waitfor_prompt().
		$cmd_prompt = $self->prompt_append($cmd_prompt, $args{$_});
	    } elsif (/^-?timeout$/i) {
		$timeout = _parse_timeout($args{$_});
	    } else {
		return $self->error($usage);
	    }
	}
    }

    print "login:\t[after args: $cmd_prompt]\n" if $DEBUG;

    ## Override these user set-able values.
    $endtime	  = _endtime($timeout);
    $orig_timeout = $self->timeout($endtime);
    $orig_errmode = $self->errmode;

    ## Create a subroutine to reset to original values.
    $reset
	= sub {
	    $self->errmode($orig_errmode);
	    $self->timeout($orig_timeout);
	    1;
	};

    ## Create a subroutine to generate an error for user.
    $error
	= sub {
	    my($errmsg) = @_;

	    &$reset;
	    if ($self->timed_out) {
		return $self->error($errmsg);
	    } elsif ($self->eof) {
		($lastline = $self->lastline) =~ s/\n+//;
		return $self->error($errmsg, ": ", $lastline);
	    } else {
		return $self->error($self->errmsg);
	    }
	};



    # Send a newline as the wakeup-call
    if ($self->send_wakeup eq 'connect') {

	$sent_wakeup = 1;

	my $old_sep = $self->output_record_separator;
	$self->output_record_separator("\n");
	$self->print('');
	$self->output_record_separator($old_sep);
    }
    while (1) {
	(undef, $_) = $self->waitfor(
		-match => '/(?:[Ll]ogin|[Uu]sername|[Pp]assw(?:or)?d)[:\s]*$/',
		-match => '/(?i:Passcode)[:\s]*$/',
		-match => $cmd_prompt,
                -match => '/(?:[Ll]ogged)/'
	);
	unless ($_) {
	    return &$error("read eof waiting for login or password prompt")
		if $self->eof;

	    # We timed-out. Send a newline as the wakeup-call.
	    if ($sent_wakeup == 0 && $self->send_wakeup eq 'timeout') {

		$sent_wakeup = 1;

		my $old_sep = $self->output_record_separator;

		$self->output_record_separator("\n");
		$self->print('');
		$self->output_record_separator($old_sep);

		next;
	    }

	    return &$error("timed-out during login process");
	}

	my $cmd_prompt_re = $self->re_sans_delims($cmd_prompt);



	if (not defined) {
	    return $self->error("login failed: access denied or bad name, passwd, etc");
	} elsif (/sername|ogin/) {
	    $self->print($username) or return &$error("login disconnected");
	    $seen{login}++ && $self->error("login failed: access denied or bad username");
	} elsif (/[Pp]assw/) {

	    $self->print($password) or return &$error("login disconnected");
	    $seen{passwd}++ && $self->error("login failed: access denied or bad password");
	} elsif (/(?i:Passcode)/) {
	    $self->print($passcode) or return &$error("login disconnected");
	    $seen{passcode}++ && $self->error("login failed: access denied or bad passcode");
	} elsif (/($cmd_prompt_re)/) {

	    &$reset; # Success. Reset obj to default vals before continuing.
	    last;
	} elsif (/Logged/) {

	    &$reset; # Success. Reset obj to default vals before continuing.
	    last;
	}else {
	    $self->error("login received unexpected prompt. Aborting.");
	}
    }

    1;
} # end sub login


# Overridden to support ignore_warnings()
sub error {
    my $self = shift;

    # Ignore warnings
    if ($self->ignore_warnings) {
	my $errmsg = join '', @_;
	my $warnings_re = $self->re_sans_delims($self->warnings);
	return if $errmsg =~ /$warnings_re/;
    }

    return $self->SUPER::error(@_);
}


# Tries to enter enabled mode with the password arg.
sub enable {
    my $self = shift;
    my $usage = 'usage: $obj->enable([Name => $name,] [Password => $password,] '
	      . '[Passcode => $passcode,] [Level => $level] )';
    my ($en_username, $en_password, $en_passcode, $en_level) = ('','','','');
    my ($error, $lastline, $orig_errmode, $reset, %args, %seen);

print " trying to enable \n";
    if (@_ == 1) {  # just passwd given
	($en_password) = shift;
    } else {  # named args given
	%args = @_;

	foreach (keys %args) {
	    if (/^-?name$|^-?login$|^-?user/i) {
		$en_username = $args{$_};
	    } elsif (/^-?passw/i) {
		$en_password = $args{$_};
	    } elsif (/^-?passc/i) {
		$en_passcode = $args{$_};
	    } elsif (/^-?level$/i) {
		$en_level    = $args{$_};
	    } else {
		return $self->error($usage);
	    }
	}
    }

    ## Create a subroutine to generate an error for user.
    $error = sub {
	    my($errmsg) = @_;

	    if ($self->timed_out) {
		return $self->error($errmsg);
	    } elsif ($self->eof) {
		($lastline = $self->lastline) =~ s/\n+//;
		return $self->error($errmsg, ": ", $lastline);
	    } else {
		return $self->error($errmsg);
	    }
	};

    # Store the old prompt without the //s around it.
    my ($old_prompt) = $self->re_sans_delims($self->prompt);



    # We need to expect either a Password prompt or a
    # typical prompt. If the user doesn't have enough
    # access to run the 'enable' command, the device
    # won't even query for a password, it will just
    # ignore the command and display another [boring] prompt.
    $self->print("enable $en_level");


    {
	my ($prematch, $match) = $self->waitfor(
		-match => '/[Ll]ogin[:\s]*$/',
		-match => '/[Uu]sername[:\s]*$/',
		-match => '/[Pp]assw(?:or)?d[:\s]*$/',
		-match => '/(?i:Passcode)[:\s]*$/',
		-match => "/$old_prompt/",
        ) or do {
		return &$error("read eof waiting for enable login or password prompt")
		    if $self->eof;
		return &$error("timed-out waiting for enable login or password prompt");
	};

	if (not defined $match) {
	    return &$error("enable failed: access denied or bad name, passwd, etc");
	} elsif ($match =~ /sername|ogin/) {
 #print "==> ERX prompted for username \n";
	    $self->print($en_username) or return &$error("enable failed");
	    $seen{login}++
 		&& return &$error("enable failed: access denied or bad username");
	    redo;
        } elsif ($match =~ /[Pp]assw/ ) {
 #print "==> ERX prompted for password \n";
	    $self->print($en_password) or return &$error("enable failed");
	    $seen{passwd}++
		 && return &$error("enable failed: access denied or bad password");
	    redo;
	} elsif ($match =~ /(?i:Passcode)/ ) {
 #print "==> ERX prompted for passcode \n";
	    $self->print($en_passcode) or return &$error("enable failed");
	    $seen{passcode}++
		 && return &$error("enable failed: access denied or bad passcode");
	    redo;
	} elsif ($match =~ /$old_prompt/) {
 #print "==> ERX prompted with old prompt \n";
	    ## Success! Exit the block.
	    last;
	} else {
	    return &$error("enable received unexpected prompt. Aborting.");
	}
    }



    if (not defined $en_level or $en_level =~ /^[1-9]/) {
	# Prompts and levels over 1 give a #/(enable) prompt.
	return $self->is_enabled ? 1 : &$error('Failed to enter enable mode');
    } else {
	# Assume success
        return 1;
    }
}



# Tries to enter enabled mode with the password arg.
sub supportmode {
    my $self = shift;
    my $usage = 'usage: $obj->enable([Name => $name,] [Password => $password,] '
	      . '[Passcode => $passcode,] [Level => $level] )';
    my ($en_username, $en_password, $en_passcode, $en_level) = ('','','','');
    my ($error, $lastline, $orig_errmode, $reset, %args, %seen);

print " trying to enter support mode \n";
    if (@_ == 1) {  # just passwd given
	($en_password) = shift;
    } else {  # named args given
	%args = @_;

	foreach (keys %args) {
	    if (/^-?name$|^-?login$|^-?user/i) {
		$en_username = $args{$_};
	    } elsif (/^-?passw/i) {
		$en_password = $args{$_};
	    } elsif (/^-?passc/i) {
		$en_passcode = $args{$_};
	    } elsif (/^-?level$/i) {
		$en_level    = $args{$_};
	    } else {
		return $self->error($usage);
	    }
	}
    }

    ## Create a subroutine to generate an error for user.
    $error = sub {
	    my($errmsg) = @_;

	    if ($self->timed_out) {
		return $self->error($errmsg);
	    } elsif ($self->eof) {
		($lastline = $self->lastline) =~ s/\n+//;
		return $self->error($errmsg, ": ", $lastline);
	    } else {
		return $self->error($errmsg);
	    }
	};

    # Store the old prompt without the //s around it.
    my ($old_prompt) = $self->re_sans_delims($self->prompt);



    # We need to expect either a Password prompt or a
    # typical prompt. If the user doesn't have enough
    # access to run the 'enable' command, the device
    # won't even query for a password, it will just
    # ignore the command and display another [boring] prompt.
    $self->print("support");


    {
	my ($prematch, $match) = $self->waitfor(
		-match => '/[Ll]ogin[:\s]*$/',
		-match => '/[Uu]sername[:\s]*$/',
		-match => '/[Pp]assw(?:or)?d[:\s]*$/',
		-match => '/(?i:Passcode)[:\s]*$/',
		-match => "/$old_prompt/",
        ) or do {
		return &$error("read eof waiting for enable login or password prompt")
		    if $self->eof;
		return &$error("timed-out waiting for enable login or password prompt");
	};

	if (not defined $match) {
	    return &$error("enable failed: access denied or bad name, passwd, etc");
	} elsif ($match =~ /sername|ogin/) {
 print "==> ERX prompted for username \n";
	    $self->print($en_username) or return &$error("enable failed");
	    $seen{login}++
 		&& return &$error("enable failed: access denied or bad username");
	    redo;
        } elsif ($match =~ /[Pp]assw/ ) {
 print "==> ERX prompted for password \n";
	    $self->print($en_password) or return &$error("enable failed");
	    $seen{passwd}++
		 && return &$error("enable failed: access denied or bad password");
	    redo;
	} elsif ($match =~ /(?i:Passcode)/ ) {
 print "==> ERX prompted for passcode \n";
	    $self->print($en_passcode) or return &$error("enable failed");
	    $seen{passcode}++
		 && return &$error("enable failed: access denied or bad passcode");
	    redo;
	} elsif ($match =~ /$old_prompt/) {
 print "==> ERX prompted with old prompt \n";
	    ## Success! Exit the block.
	    last;
	} else {
	    return &$error("enable received unexpected prompt. Aborting.");
	}
    }

    if (not defined $en_level or $en_level =~ /^[1-9]/) {
	# Prompts and levels over 1 give a #/(enable) prompt.
	return $self->is_enabled ? 1 : &$error('Failed to enter enable mode');
    } else {
	# Assume success
        return 1;
    }
}



# Leave enabled mode.
sub disable {
    my $self = shift;
    $self->cmd('disable');
    return $self->is_enabled ? $self->error('Failed to exit enabled mode') : 1;
}






# Send control-^ (without newline)
sub ios_break {
    my $self = shift;

    my $old_ors = $self->output_record_separator;
    $self->output_record_separator('');
    my $ret = $self->print("\c^");
    $self->output_record_separator($old_ors);

    return $ret;
}

# Displays the last prompt.
sub last_prompt {
    my $self = shift;
    my $stream = $ {*$self}{net_telnet_erx};
    exists $stream->{last_prompt} ? $stream->{last_prompt} : undef;
}

# Displays the last command.
sub last_cmd {
    my $self = shift;
    my $stream = $ {*$self}{net_telnet_erx};
    exists $stream->{last_cmd} ? $stream->{last_cmd} : undef;
}

# Examines the last prompt to determine the current mode.
# Some prompts may be hard set to #, so this won't always return a valid answer.
# Call 'show priv' instead.
# 1     => enabled.
# undef => not enabled.
sub is_enabled { $_[0]->last_prompt =~ /\#|enable|config/ ? 1 : undef }

# Typical get/set method.
sub always_waitfor_prompt {
    my ($self, $arg) = @_;
    my $stream = $ {*$self}{net_telnet_erx};
    $stream->{always_waitfor_prompt} = $arg if defined $arg;
    return $stream->{always_waitfor_prompt};
}

# Typical get/set method.
sub waitfor_pause {
    my ($self, $arg) = @_;
    my $stream = $ {*$self}{net_telnet_erx};
    $stream->{waitfor_pause} = $arg if defined $arg;
    return $stream->{waitfor_pause};
}

# Typical get/set method.
sub autopage {
    my ($self, $arg) = @_;
    my $stream = $ {*$self}{net_telnet_erx};
    $stream->{autopage} = $arg if defined $arg;
    return $stream->{autopage};
}

# Typical get/set method.
sub normalize_cmd {
    my ($self, $arg) = @_;
    my $stream = $ {*$self}{net_telnet_erx};
    $stream->{normalize_cmd} = $arg if defined $arg;
    return $stream->{normalize_cmd};
}

# Typical get/set method.
sub send_wakeup {
    my ($self, $arg) = @_;
    my $stream = $ {*$self}{net_telnet_erx};
    $stream->{send_wakeup} = $arg if defined $arg;
    return $stream->{send_wakeup};
}

# Typical get/set method.
sub ignore_warnings {
    my ($self, $arg) = @_;
    my $stream = $ {*$self}{net_telnet_erx};
    $stream->{ignore_warnings} = $arg if defined $arg;
    return $stream->{ignore_warnings};
}

# Get/set the More prompt
sub more_prompt {
    my ($self, $arg) = @_;
    my $stream = $ {*$self}{net_telnet_erx};
    if (defined $arg) {
	$self->_match_check($arg);
        $stream->{more_prompt} = $arg;
    }
    return $stream->{more_prompt};
}

# Join two or more regexen into one on "|".
sub prompt_append {
    my $self = shift;
    my $orig = shift || '';
    return $self->error("usage: \$obj->prompt_append(orig, new, [new...])")
	unless @_;

    print "prompt_append:\t[original: $orig]\n" if $DEBUG;

    if ($orig) {
	if ($self->_match_check($orig)) {
	    $orig = $self->re_sans_delims($orig);
	    return $self->error("Can't parse prompt: '$orig'") unless $orig;
	}
    }

    for (@_) {
	print "prompt_append:\t[append: $_]\n" if $DEBUG;
	if ($self->_match_check($_)) {
	    my $re = $self->re_sans_delims($_);

	    unless ($re) {
		$self->error("Can't parse prompt: '$_'");
		next;
	    }

	    $orig .= $orig ? "|$re" : $re;
	}
    }

    print "prompt_append:\t[return: /$orig/]\n\n" if $DEBUG;
    return "/$orig/";
}

# Return a Net::Telnet regular expression without the delimiters.
sub re_sans_delims {
    my ($self, $str) = @_;

    return $self->error("usage: \$obj->re_sans_delims(\$matchop)")
	unless $str;

    $self->_match_check($str);
    my ($delim, $re) = $str =~  /^\s*m?\s*(\W)(.*)\1\s*$/;
    return $re;
}


############################################################################
# Tries to show ip bgp
############################################################################
sub showIpBgp2 {
    my $self = shift;
    my $usage = 'usage: $obj->enable([Name => $name,] [Password => $password,] '
	      . '[Passcode => $passcode,] [Level => $level] )';
    my ($en_username, $en_password, $en_passcode, $en_level) = ('','','','');
    my ($error, $lastline, $orig_errmode, $reset, %args, %seen);

print " trying to show ip bgp\n";


    ## Init vars.
    my $prefix = '';

    if (@_ == 2) {  # just prefix given
	$prefix = @_[1];
    }

    else {  # named args given
	## Get the named args.
	 %args = @_;

	## Parse the named args.
	foreach (keys %args) {
	    if (/^-?prefix$/i) {
		$prefix    = $args{$_};
              }
        } # end for each

      } # end else

    ## Create a subroutine to generate an error for user.
    $error = sub {
	    my($errmsg) = @_;

	    if ($self->timed_out) {
		return $self->error($errmsg);
	    } elsif ($self->eof) {
		($lastline = $self->lastline) =~ s/\n+//;
		return $self->error($errmsg, ": ", $lastline);
	    } else {
		return $self->error($errmsg);
	    }
	};

    # Store the old prompt without the //s around it.
    my ($old_prompt) = $self->re_sans_delims($self->prompt);

# print "==> old prompt is $old_prompt \n";

    # We need to expect an enabled prompt 
    $self->print("show ip bgp | inc $prefix");


    {
    # we need to use the waitfor supplied by net::telnet because the waitfor in this
    # module gets confused on "show bgp" commands because these commands use '>' to indicate 
    # best route.  This confuses the waitfor in this module

	my ($prematch, $match) = $self->SUPER::waitfor(
		-match => '/[\w.-]+#$/'
        ) or do {
		return &$error("read eof waiting for prompt")
		    if $self->eof;
		return &$error("timed-out waiting for prompt");
	};

	if (not defined $match) {
	    return &$error("show ip bgp failed - did not get prompt back");
	} elsif ($match =~ /[\w.-]+#/) 
         {

          print " prematch is $prematch \n";

          my $showIpBgp = $prematch;

        #  if ( $showIpBgp =~ /(?m:\>\s+([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)\s+([a-zA-Z]+))/ )
           if ( $showIpBgp =~ /(?m:\>\s+([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)?\s*([a-zA-Z]+))/ )
           {
           # print " parsed prefix as $1\n";
           # print " parsed mask as $2\n";
           # print " parsed peer as $3\n";
           # print " parsed next hop as $4\n";
           # print " parsed med as $5\n";
           # print " parsed weight as $6\n";
           # print " parsed origin as $7\n";
            return ($1, $2, $3, $4, $5, $6, $7);
            
    
           }  
         else {
	    print "show ip bgp  did not find the prefix $prefix.\n";
            return ( '','','','','','','');
	      }
   

        }  # endif elsif ($match =~ /[\w.-]+#/) 
    }



}


############################################################################
# Tries to show ip bgp and find a route that matches the specified arguments
# prefix is the only required argument
############################################################################
sub check_bgp_route {
    my $self = shift;
    my $usage = 'usage: $obj->check_bgp_route([statusCode => $statusCode,] [prefix => $prefix,] '
	      . '[mask => $mask,] [peer => $peer, ] [nextHop => $nextHop,] [ med => $med ,] '
              . '[ weight => $weight,] [ lclPrf => $lclPrf ] [origin => $origin] ';
              
   # my ($en_username, $en_password, $en_passcode, $en_level) = ('','','','');
    my ($error, $lastline, $orig_errmode, $reset, %args, %seen);

print " trying to check ip bgp route\n";


    ## Init vars.
    my $statusCode = '';
    my $prefix = '';
    my $mask = '([0-9]+)';
    my $peer = '([0-9\.]+)';
    my $nextHop = '([0-9\.]+)';
    my $med = '(\d+)?';
    my $weight = '(\d+)?';
    my $lclPrf = '(\d+)?';
    my $origin = '([a-zA-Z\.]+)';
    my $rc = 0;
    my $ gotMask = 0;

    if (@_ == 2) {  # just prefix given
	$prefix = @_[1];
    }

    else {  # named args given
	## Get the named args.
	 %args = @_;

	## Parse the named args.
	foreach (keys %args) {
	    if (/^-?statusCode$/i) {
		$statusCode    = $args{$_};
              }
	    if (/^-?prefix$/i) {
		$prefix    = $args{$_};
              }
	    if (/^-?mask$/i) {
		$mask    = $args{$_};
                $gotMask = 1;
              }
	    if (/^-?peer$/i) {
		$peer    = $args{$_};
              }
	    if (/^-?nextHop$/i) {
		$nextHop    = $args{$_};
              }
	    if (/^-?med$/i) {
		$med    = $args{$_};
              }
	    if (/^-?weight$/i) {
		$weight    = $args{$_};
              }
	    if (/^-?lclPrf$/i) {
		$lclPrf    = $args{$_};
              }
	    if (/^-?origin$/i) {
		$origin    = $args{$_};
              }
        } # end for each

      } # end else

# if ( $showIpBgp =~ /(?m:\>\s+([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)?\s*([a-zA-Z]+))/ )
#my $searchstring = "(?m:($prefix)\\/($mask)\\s*($peer)\\s*($nextHop)\\s*($med)\\s*($weight)\\s*($origin))";

    ## Create a subroutine to generate an error for user.
    $error = sub {
	    my($errmsg) = @_;

	    if ($self->timed_out) {
		return $self->error($errmsg);
	    } elsif ($self->eof) {
		($lastline = $self->lastline) =~ s/\n+//;
		return $self->error($errmsg, ": ", $lastline);
	    } else {
		return $self->error($errmsg);
	    }
	};

    # Store the old prompt without the //s around it.
    my ($old_prompt) = $self->re_sans_delims($self->prompt);

# print "==> old prompt is $old_prompt \n";

    # We need to expect an enabled prompt 
if (1 == $gotMask)
   {
    $self->print("show ip bgp | inc $prefix/$mask");
   }
  else 
     {
    $self->print("show ip bgp | inc $prefix/");
   }

    {
	my ($prematch, $match) = $self->SUPER::waitfor(
		-match => '/[\w.-]+#$/'
        ) or do {
		return &$error("read eof waiting for prompt")
		    if $self->eof;
		return &$error("timed-out waiting for prompt");
	};

	if (not defined $match) {
	    return &$error("show ip bgp failed - did not get prompt back");
	} elsif ($match =~ /[\w.-]+#/) 
         {

          print " check_bgp_route result for $prefix is:\n\n $prematch \n";
          print "end of matching entries\n\n";
 
          my $showIpBgp = $prematch;
print " show ip bgp is:\n$showIpBgp\n\n";

#my $searchstring = "(?m:($prefix)\\/($mask)\\s*($peer)\\s*($nextHop)\\s*($med)\\s*($weight)\\s*($origin))";
 my $searchstring = "(?m:($statusCode)\\s*($prefix)\\/($mask)\\s*($peer)\\s*($nextHop)\\s*($med)\\s*($lclPrf)\\s*($weight)\\s*($origin))";

#  print " searchstring is $searchstring\n\n";

        #  if ( $showIpBgp =~ /(?m:\>\s+([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)\s+([a-zA-Z]+))/ )
        #   if ( $showIpBgp =~ /(?m:\>\s+([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)?\s*([a-zA-Z]+))/ )
           if ( $showIpBgp =~ $searchstring)
           {
         #   print " FOUND\n";
           # print " looking for prefix $prefix and parsed prefix as $2\n";
           # print " parsed mask as $3\n";
           # print " parsed peer as $4\n";
           # print " parsed next hop as $5\n";
           # print " parsed med as $6\n";
           # print " parsed weight as $7\n";
           # print " parsed origin as $8\n";
             $rc = 1;
             return ($rc);
           # return ($1, $2, $3, $4, $5, $6, $7);
           
            
    
           }  
         else {
	    print "show ip bgp  did not find the prefix $prefix with the required fields.\n";
             if ( $showIpBgp =~ /(?m:([\>\*sdra]?)\s*([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)?\s*([a-zA-Z]+))/ )
         # print " looking for prefix $prefix and parsed prefix as $1\n";
             {
              print " looking for prefix $prefix and parsed prefix as $2\n";
              print " parsed mask as $3\n";
              print " parsed peer as $4\n";
              print " parsed next hop as $5\n";
              print " parsed med as $6\n";
             # print " parsed weight as $7\n";
              print " parsed origin as $8\n";
             }
             $rc = 0;
             return ($rc);
	      }
   
 


        }  # endif elsif ($match =~ /[\w.-]+#/) 
    }



}


############################################################################
# Tries to show ip bgp
############################################################################

sub show_ip_bgp {
    my $self = shift;
    my $usage = 'usage: $obj->show_ip_bgp([statusCode => $statusCode,] [prefix => $prefix,] '
	      . '[mask => $mask,] [peer => $peer, ] [nextHop => $nextHop,] [ med => $med ,] '
              . '[ weight => $weight,] [ lclPrf => $lclPrf ] [origin => $origin] ';
    my ($en_username, $en_password, $en_passcode, $en_level) = ('','','','');
    my ($error, $lastline, $orig_errmode, $reset, %args, %seen);




    ## Create a subroutine to generate an error for user.
    $error = sub {
	    my($errmsg) = @_;

	    if ($self->timed_out) {
		return $self->error($errmsg);
	    } elsif ($self->eof) {
		($lastline = $self->lastline) =~ s/\n+//;
		return $self->error($errmsg, ": ", $lastline);
	    } else {
		return $self->error($errmsg);
	    }
	};

    # Store the old prompt without the //s around it.
    my ($old_prompt) = $self->re_sans_delims($self->prompt);

# print "==> old prompt is $old_prompt \n";

    # We need to expect an enabled prompt 
    $self->print("show ip bgp");


    {

	my ($prematch, $match) = 	$self->SUPER::waitfor(
		-match => '/[a-zA-Z0-9]+#$/'
        ) or do {
                if ($self->eof)
                 {
		   print "show ip bgp read eof waiting for prompt";
		 }   
                 else
                 {
		   print "show ip bgp timed-out waiting for prompt";
                 }
               return; 
	};



	if (not defined $match) {
            print "show ip bgp failed - did not get prompt back";
	    return &$error("show ip bgp failed - did not get prompt back");
	} elsif ($match =~ /[a-zA-Z0-9]+#/) 
         
          {
          print " show ip bgp:\n\n $prematch \n";
          }



         
     
    }



}


############################################################################
# Tries to output command to stdout.
# This assumes that the ERx is enabled and waits for the # prompt
############################################################################
sub output_erx_cmd {
    my $self = shift;
   # my $usage = 'usage: $obj->enable([Name => $name,] [Password => $password,] '
   #	      . '[Passcode => $passcode,] [Level => $level] )';
   # my ($en_username, $en_password, $en_passcode, $en_level) = ('','','','');
    my ($error, $lastline, $orig_errmode, $reset, %args, %seen);
    my $command = '!';
    my $gotCmd = 0;

if (@_ == 2) {  # just prefix given
	$command = @_[1];
        $gotCmd = 1;
    }

    else {  # named args given
	## Get the named args.
	 %args = @_;

	## Parse the named args.
	foreach (keys %args) {
	    if (/^-?command$/i) {
		$command    = $args{$_};
                $gotCmd = 1;
              }
	    
        } # end for each

      } # end else

if ( 0 == $gotCmd)
  { 
    print " output_erx_cmd did not get required parameter (command)\n";
    return;
  }

 
    ## Create a subroutine to generate an error for user.
    $error = sub {
	    my($errmsg) = @_;

	    if ($self->timed_out) {
		return $self->error($errmsg);
	    } elsif ($self->eof) {
		($lastline = $self->lastline) =~ s/\n+//;
		return $self->error($errmsg, ": ", $lastline);
	    } else {
		return $self->error($errmsg);
	    }
	};

    # Store the old prompt without the //s around it.
    my ($old_prompt) = $self->re_sans_delims($self->prompt);

# print "==> old prompt is $old_prompt \n";

    # We need to expect an enabled prompt 
    $self->print($command);


    {
	my ($prematch, $match) = $self->SUPER::waitfor(
		-match => '/[a-zA-Z0-9]+#$/'
        ) or do {
		return &$error("read eof waiting for prompt")
		    if $self->eof;
		return &$error("timed-out waiting for prompt");
	};

	if (not defined $match) {
            print "show ip bgp failed - did not get prompt back";
	    return &$error("show ip bgp failed - did not get prompt back");
	} elsif ($match =~ /[a-zA-Z0-9]+#/)   # =~ /[\w.-]+#/) 
         
          {
          print " show ip bgp:\n\n $prematch \n";
           }

         
     
    }



}


############################################################################
# Tries to show ip bgp neighbor x.x.x.x advertised-routes and checks for 
# the route that matches the specified arguments
############################################################################
sub check_bgp_adv_route {
    my $self = shift;
    my $usage = 'usage: $obj->enable([Name => $name,] [Password => $password,] '
	      . '[Passcode => $passcode,] [Level => $level] )';
    my ($en_username, $en_password, $en_passcode, $en_level) = ('','','','');
    my ($error, $lastline, $orig_errmode, $reset, %args, %seen);

print " trying to show ip bgp\n";


    ## Init vars.
    my $prefix = '';
    my $mask = '([0-9]+)';
    my $peer = '([0-9\.]+)';
    my $nextHop = '([0-9\.]+)';
    my $med = '(\d+)';
    my $weight = '(\d+)?';
    my $origin = '([a-zA-Z\.]+)';
    my $neighbor = '';
    
    my $rc = 0;

    if (@_ == 2) {  # just prefix given
	$prefix = @_[1];
    }

    else {  # named args given
	## Get the named args.
	 %args = @_;

	## Parse the named args.
	foreach (keys %args) {
	    if (/^-?prefix$/i) {
		$prefix    = $args{$_};
              }
	    if (/^-?mask$/i) {
		$mask    = $args{$_};
              }
	    if (/^-?peer$/i) {
		$peer    = $args{$_};
              }
	    if (/^-?nextHop$/i) {
		$nextHop    = $args{$_};
              }
	    if (/^-?med$/i) {
		$med    = $args{$_};
              }
	    if (/^-?weight$/i) {
		$weight    = $args{$_};
              }
	    if (/^-?origin$/i) {
		$origin    = $args{$_};
              }
            if (/^-?neighbor$/i) {
		$neighbor    = $args{$_};
              } 
        } # end for each

      } # end else

# if ( $showIpBgp =~ /(?m:\>\s+([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)?\s*([a-zA-Z]+))/ )
my $searchstring = "(?m:$prefix\\/$mask\\s*$peer\\s*$nextHop\\s*$med\\s*$weight\\s*$origin)";

    ## Create a subroutine to generate an error for user.
    $error = sub {
	    my($errmsg) = @_;

	    if ($self->timed_out) {
		return $self->error($errmsg);
	    } elsif ($self->eof) {
		($lastline = $self->lastline) =~ s/\n+//;
		return $self->error($errmsg, ": ", $lastline);
	    } else {
		return $self->error($errmsg);
	    }
	};

    # Store the old prompt without the //s around it.
    my ($old_prompt) = $self->re_sans_delims($self->prompt);

# print "==> old prompt is $old_prompt \n";

    # We need to expect an enabled prompt 
    $self->print("show ip bgp neighbor $neighbor advertised-routes | inc $prefix");


    {
	my ($prematch, $match) = $self->SUPER::waitfor(
		-match => '/[\w.-]+#$/'
        ) or do {
		return &$error("read eof waiting for prompt")
		    if $self->eof;
		return &$error("timed-out waiting for prompt");
	};

	if (not defined $match) {
	    return &$error("show ip bgp failed - did not get prompt back");
	} elsif ($match =~ /[\w.-]+#/) 
         {

          print " prematch is $prematch \n";

          my $showIpBgp = $prematch;

        #  if ( $showIpBgp =~ /(?m:\>\s+([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)\s+([a-zA-Z]+))/ )
        #   if ( $showIpBgp =~ /(?m:\>\s+([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)?\s*([a-zA-Z]+))/ )
           if ( $showIpBgp =~ $searchstring)
           {
           # print " parsed prefix as $1\n";
           # print " parsed mask as $2\n";
           # print " parsed peer as $3\n";
           # print " parsed next hop as $4\n";
           # print " parsed med as $5\n";
           # print " parsed weight as $6\n";
           # print " parsed origin as $7\n";
             $rc = 1;
             return ($rc);
           # return ($1, $2, $3, $4, $5, $6, $7);
           
            
    
           }  
         else {
	    print "show ip bgp  did not find the prefix $prefix with the required fields.\n";
            if ( $showIpBgp =~ /(?m:\>\s+([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)?\s*([a-zA-Z]+))/ )
          
             {
              print " parsed prefix as $1\n";
              print " parsed mask as $2\n";
              print " parsed peer as $3\n";
              print " parsed next hop as $4\n";
              print " parsed med as $5\n";
              # print " parsed weight as $6\n";
              print " parsed origin as $7\n";
             }
             $rc = 0;
             return ($rc);
	      }
   
 


        }  # endif elsif ($match =~ /[\w.-]+#/) 
    }



}

##############################################################################
# Tries to show ip bgp neighbor x.x.x.x received-routes
##############################################################################
sub check_bgp_rcv_route {
    my $self = shift;
    my $usage = 'usage: $obj->enable([Name => $name,] [Password => $password,] '
	      . '[Passcode => $passcode,] [Level => $level] )';
    my ($en_username, $en_password, $en_passcode, $en_level) = ('','','','');
    my ($error, $lastline, $orig_errmode, $reset, %args, %seen);

print " trying to show ip bgp\n";


    ## Init vars.
    my $statusCode ='';
    my $prefix = '';
    my $mask = '([0-9]+)';
    my $peer = '([0-9\.]+)';
    my $nextHop = '([0-9\.]+)';
    my $med = '(\d+)';
    my $weight = '(\d+)?';
    my $origin = '([a-zA-Z\.]+)';
    my $neighbor = '';
    
    my $rc = 0;

    if (@_ == 2) {  # just prefix given
	$prefix = @_[1];
    }

    else {  # named args given
	## Get the named args.
	 %args = @_;

	## Parse the named args.
	foreach (keys %args) {
	    if (/^-?statusCode$/i) {
		$statusCode    = $args{$_};
              }
	    if (/^-?prefix$/i) {
		$prefix    = $args{$_};
              }
	    if (/^-?mask$/i) {
		$mask    = $args{$_};
              }
	    if (/^-?peer$/i) {
		$peer    = $args{$_};
              }
	    if (/^-?nextHop$/i) {
		$nextHop    = $args{$_};
              }
	    if (/^-?med$/i) {
		$med    = $args{$_};
              }
	    if (/^-?weight$/i) {
		$weight    = $args{$_};
              }
	    if (/^-?origin$/i) {
		$origin    = $args{$_};
              }
            if (/^-?neighbor$/i) {
		$neighbor    = $args{$_};
              } 
        } # end for each

      } # end else

# if ( $showIpBgp =~ /(?m:\>\s+([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)?\s*([a-zA-Z]+))/ )
# my $searchstring = "$prefix\\/$mask\\s*$peer\\s*$nextHop\\s*$med\\s*$weight\\s*$origin";
my $searchstring = "(?m:$statusCode\\s*$prefix\\/$mask\\s*$peer\\s*$nextHop\\s*$med\\s*$weight\\s*$origin)";

    ## Create a subroutine to generate an error for user.
    $error = sub {
	    my($errmsg) = @_;

	    if ($self->timed_out) {
		return $self->error($errmsg);
	    } elsif ($self->eof) {
		($lastline = $self->lastline) =~ s/\n+//;
		return $self->error($errmsg, ": ", $lastline);
	    } else {
		return $self->error($errmsg);
	    }
	};

    # Store the old prompt without the //s around it.
    my ($old_prompt) = $self->re_sans_delims($self->prompt);

# print "==> old prompt is $old_prompt \n";

    # We need to expect an enabled prompt 
    $self->print("show ip bgp neighbor $neighbor received-routes | inc $prefix");


    {
	my ($prematch, $match) = $self->SUPER::waitfor(
		-match => '/[\w.-]+#$/'
        ) or do {
		return &$error("read eof waiting for prompt")
		    if $self->eof;
		return &$error("timed-out waiting for prompt");
	};

	if (not defined $match) {
	    return &$error("show ip bgp failed - did not get prompt back");
	} elsif ($match =~ /[\w.-]+#/) 
         {

          print " prematch is $prematch \n";

          my $showIpBgp = $prematch;

        #  if ( $showIpBgp =~ /(?m:\>\s+([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)\s+([a-zA-Z]+))/ )
        #   if ( $showIpBgp =~ /(?m:\>\s+([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)?\s*([a-zA-Z]+))/ )
           if ( $showIpBgp =~ $searchstring)
           {
           # print " parsed prefix as $1\n";
           # print " parsed mask as $2\n";
           # print " parsed peer as $3\n";
           # print " parsed next hop as $4\n";
           # print " parsed med as $5\n";
           # print " parsed weight as $6\n";
           # print " parsed origin as $7\n";
             $rc = 1;
             return ($rc);
           # return ($1, $2, $3, $4, $5, $6, $7);
           
            
    
           }  
         else {
	    print "show ip bgp  did not find the prefix $prefix with the required fields.\n";
            if ( $showIpBgp =~ /(?m:\>\s+([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)?\s*([a-zA-Z]+))/ )
          
             {
              print " parsed prefix as $1\n";
              print " parsed mask as $2\n";
              print " parsed peer as $3\n";
              print " parsed next hop as $4\n";
              print " parsed med as $5\n";
           #   print " parsed weight as $6\n";
              print " parsed origin as $7\n";
             }
             $rc = 0;
             return ($rc);
	      }
   
 


        }  # endif elsif ($match =~ /[\w.-]+#/) 
    }



}

##############################################################################
# Tries to show ip bgp community xxxxx
##############################################################################
sub check_bgp_community {
    my $self = shift;
    my $usage = 'usage: $obj->enable([Name => $name,] [Password => $password,] '
	      . '[Passcode => $passcode,] [Level => $level] )';
    my ($en_username, $en_password, $en_passcode, $en_level) = ('','','','');
    my ($error, $lastline, $orig_errmode, $reset, %args, %seen);

print " trying to show ip bgp\n";


    ## Init vars.
    my $prefix = '';
    my $mask = '([0-9]+)';
    my $peer = '([0-9\.]+)';
    my $nextHop = '([0-9\.]+)';
    my $med = '(\d+)';
    my $weight = '(\d+)?';
    my $origin = '([a-zA-Z\.]+)';
    my $community = '';
    
    my $rc = 0;

    if (@_ == 2) {  # just prefix given
	$prefix = @_[1];
    }

    else {  # named args given
	## Get the named args.
	 %args = @_;

	## Parse the named args.
	foreach (keys %args) {
	    if (/^-?prefix$/i) {
		$prefix    = $args{$_};
              }
	    if (/^-?mask$/i) {
		$mask    = $args{$_};
              }
	    if (/^-?peer$/i) {
		$peer    = $args{$_};
              }
	    if (/^-?nextHop$/i) {
		$nextHop    = $args{$_};
              }
	    if (/^-?med$/i) {
		$med    = $args{$_};
              }
	    if (/^-?weight$/i) {
		$weight    = $args{$_};
              }
	    if (/^-?origin$/i) {
		$origin    = $args{$_};
              }
            if (/^-?community$/i) {
		$community    = $args{$_};
              } 
        } # end for each

      } # end else

# if ( $showIpBgp =~ /(?m:\>\s+([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)?\s*([a-zA-Z]+))/ )

#my $searchstring = "(?m:$prefix\\/$mask\\s*$peer\\s*$nextHop\\s*$med\\s*$weight\\s*$origin)";
my $searchstring = "(?m:$prefix\\/$mask\\s*$peer\\s*$nextHop\\s*$med\\s*$weight\\s*$origin)";
    ## Create a subroutine to generate an error for user.
    $error = sub {
	    my($errmsg) = @_;

	    if ($self->timed_out) {
		return $self->error($errmsg);
	    } elsif ($self->eof) {
		($lastline = $self->lastline) =~ s/\n+//;
		return $self->error($errmsg, ": ", $lastline);
	    } else {
		return $self->error($errmsg);
	    }
	};

    # Store the old prompt without the //s around it.
    my ($old_prompt) = $self->re_sans_delims($self->prompt);

# print "==> old prompt is $old_prompt \n";

    # We need to expect an enabled prompt 
    $self->print("show ip bgp  community $community | inc $prefix");


    {
	my ($prematch, $match) = $self->SUPER::waitfor(
		-match => '/[\w.-]+#$/'
        ) or do {
		return &$error("read eof waiting for prompt")
		    if $self->eof;
		return &$error("timed-out waiting for prompt");
	};

	if (not defined $match) {
	    return &$error("show ip bgp failed - did not get prompt back");
	} elsif ($match =~ /[\w.-]+#/) 
         {

          print " prematch is $prematch \n";

          my $showIpBgp = $prematch;

        #  if ( $showIpBgp =~ /(?m:\>\s+([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)\s+([a-zA-Z]+))/ )
        #   if ( $showIpBgp =~ /(?m:\>\s+([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)?\s*([a-zA-Z]+))/ )
           if ( $showIpBgp =~ $searchstring)
           {
           # print " parsed prefix as $1\n";
           # print " parsed mask as $2\n";
           # print " parsed peer as $3\n";
           # print " parsed next hop as $4\n";
           # print " parsed med as $5\n";
           # print " parsed weight as $6\n";
           # print " parsed origin as $7\n";
             $rc = 1;
             return ($rc);
           # return ($1, $2, $3, $4, $5, $6, $7);
           
            
    
           }  
         else {
	    print "show ip bgp  did not find the prefix $prefix with the required fields.\n";
            if ( $showIpBgp =~ /(?m:\>\s+([0-9\.]+)\/([0-9]+)\s+([0-9\.]+)\s+([0-9\.]+)\s+(\d+)\s+(\d+)?\s*([a-zA-Z]+))/ )
          
             {
              print " parsed prefix as $1\n";
              print " parsed mask as $2\n";
              print " parsed peer as $3\n";
              print " parsed next hop as $4\n";
              print " parsed med as $5\n";
            #  print " parsed weight as $6\n";
              print " parsed origin as $7\n";
             }
             $rc = 0;
             return ($rc);
	      }
   
 


        }  # endif elsif ($match =~ /[\w.-]+#/) 
    }



}



##############################################################################
#
#  show_ip_bgp_sum does the equivalent of 
#     show ip bgg sum | inc x.x.x.x, where x.x.x.x is the IP address
#     of the bgp neighbor
##############################################################################

sub show_ip_bgp_sum
{

my $self = shift;
   
   my (%args, %seen);

    local $_;

    ## Init vars.
    my $prefix = '';

    if (@_ == 2) {  # just prefix given
	$prefix = @_[1];
    }

    else {  # named args given
	## Get the named args.
	 %args = @_;

	## Parse the named args.
	foreach (keys %args) {
	    if (/^-?prefix$/i) {
		$prefix    = $args{$_};
              }
        } # end for each

      } # end else

my @output = $self->cmd("show ip bgp sum | inc $prefix");
my $showIpBgpSum = join($",@output);
print " show ip bgp sum:\n\n$showIpBgpSum";

#   if ( $showIpBgpSum =~ /(?m:([0-9\.]+)\s+(\d+)\s+([a-zA-Z]+)\s+([0-9]+d\s+))/ )
 if ( $showIpBgpSum =~ /(?m:([0-9\.]+)\s+(\d+)\s+([a-zA-Z]+)\s+([0-9]+d\s+)?([0-9:]+)\s+(\d+)\s+(\d+)\s+(\d+))/ )
      {
        print "parsed prefix as $1\n";
        print " parsed AS as $2\n";
        print " parsed state as $3\n";
        print " parsed optional #days as $4\n";
        print " parsed uptime as $5\n";
        print " parsed msg sent as $6\n";
        print " parsed msg rcv as $7\n";
        print " parsed pref rcv as $8\n";
    
       }  

   return ($1, $2, $3, $4, $5, $6, $7, $8);
}



###################################################################
#
#  check_ip_bgp_sum 
###################################################################

sub check_ip_bgp_sum
{

my $self = shift;
   
   my (%args, %seen);

    local $_;

    ## Init vars.
    my $neighbor = '[0-9\.]+';
    my $ as ='\d+';
    my $state = '[a-zA-Z]+';

    if (@_ == 2) {  # just prefix given
	$neighbor = @_[1];
    }

    else {  # named args given
	## Get the named args.
	 %args = @_;

	## Parse the named args.
	foreach (keys %args) {
	    if (/^-?neighbor$/i) {
		$neighbor    = $args{$_};
              }
            if (/^-?as$/i) {
		$as    = $args{$_};
              }
            if (/^-?state$/i) {
		$state    = $args{$_};
              }
        } # end for each

      } # end else

my @output = $self->cmd("show ip bgp sum | inc ^$neighbor");
my $showIpBgpSum = join($",@output);
print " show ip bgp sum:\n\n$showIpBgpSum";
my $rc = 0;

#   if ( $showIpBgpSum =~ /(?m:([0-9\.]+)\s+(\d+)\s+([a-zA-Z]+)\s+([0-9]+d\s+))/ )
# if ( $showIpBgpSum =~ /(?m:([0-9\.]+)\s+(\d+)\s+([a-zA-Z]+)\s+([0-9]+d\s+)?([0-9:]+)\s+(\d+)\s+(\d+)\s+(\d+))/ )
if ( $showIpBgpSum =~ /(?m:($neighbor)\s+($as)\s+($state)(\(\*\))?\s+([0-9]+d\s+)?([0-9:]+)\s+(\d+)\s+(\d+)\s+(\d+))/)
#if ( $showIpBgpSum =~ (?m:($neighbor)\s+($as)\s+($state)\s+([0-9]+d\s+)?([0-9:]+)\s+(\d+)\s+(\d+)\s+(\d+)) )
      {
        print "\n";
        print " check_ip_bgp_sum - parsed neighbor as $1\n";
        print " parsed AS as $2\n";
        print " parsed state as $3\n";
     #   print " parsed optional #days as $4\n";
        print " parsed uptime as $5\n";

# please check this -- uninitialized variables
   #     print " parsed msg sent as $6\n";
   #     print " parsed msg rcv as $7\n";
   #     print " parsed pref rcv as $8\n";
        $rc = 1;
       
       }  
       return ($rc);
  # return ($1, $2, $3, $4, $5, $6, $7, $8);
}

###################################################################
#
#  check_route sees if a route is in the routing table
#
#
###################################################################

sub check_route
{

#Prefix/Length     Type      Next Hop        Dist/Met            Intf

    my $self = shift;
    
    my (%args, %seen);

    local $_;

    ## Init vars.
    my $prefix = '';
    my $length  = '\d+';
    my $routeType  = '[a-zA-Z\-]+';
    my $nexthop    = '[\.0-9]+';
    my $dist_metric = '\d+\/\d+';
    my $intf       = '[\./:a-zA-Z0-9]+';
    my $dist = '\d+';
    my $metric = '\d+';

my $rc = 0;

my $tmpdist = '\d+';
my $tmpmetric = '\d+';
my $tmprouteType  = '[a-zA-Z\-]+';
my $tmpintf = $intf; 
my $tmpnexthop    = '[\.0-9]+';
my $tmplength  = '\d+';

my $usage = 'usage: $obj->login([prefix => $prefix,] [length => $length,] '
	   . '[routeType => $routeType,] [nexthop => $nexthop,] [intf => $intf,])';

    if (@_ == 2) {  # just prefix given
	$prefix = @_[1];
    }

    else {  # named args given
	## Get the named args.
	 %args = @_;

	## Parse the named args.
	foreach (keys %args) {
	    if (/^-?prefix$/i) {
		$prefix    = $args{$_};
	    } elsif (/^-?length/i) {
		$length    = $args{$_};
                $tmplength = quotemeta($length);
	    } elsif (/^-?routeType/i) {
		$routeType    = $args{$_};
                $tmprouteType = quotemeta($routeType);
	    } elsif (/^-?nexthop$/i) {
	        $nexthop    = $args{$_};
                $tmpnexthop = quotemeta($nexthop);  ;
	    } elsif (/^-?intf$/i) {
		$intf = $args{$_};
                $tmpintf = quotemeta($intf);
	    } elsif (/^-?metric$/i) {
		$metric = $args{$_};
                $tmpmetric = quotemeta($metric);
	    } elsif (/^-?dist$/i) {
		$dist = $args{$_};
                $tmpdist = quotemeta($dist);
	    }else {
		# return $self->error($usage);
	    }
	} # end for each

      } # end else


 
my $tmpprefix = quotemeta($prefix);
my $tmpdist_metric = quotemeta($dist_metric);
 


my $searchstring = "$tmpprefix\/$tmplength\\s+$tmprouteType\\s+$tmpnexthop\\s+$tmpdist\/$tmpmetric\\s+$tmpintf";
# "$tmpprefix\/$tmplength\\s+$tmprouteType\\s+$tmpnexthop\\s+$tmpdist\/$tmpmetric\\s+$tmpintf";



my $showIpRoute1 = '';

my @output =();
# this double line looks stupid and unnecessary, but it keeps things in sync for reasons I don't know

my @output = $self->cmd("show ip route $prefix | begin ^[0-9]");
my @output = $self->cmd("show ip route $prefix | begin ^[0-9]");
my $showIpRoute1 = join('',@output);
print " show ip route $prefix:\n\n$showIpRoute1";


# print " search string is $searchstring\n";

 if ( $showIpRoute1 =~ $searchstring)
      {
       print " found route for $prefix in routing table\n\n";
       $rc = 1;
       
       }    
      else
        {

        $rc = 0;
        print " check_route  did not find specified route for $prefix  in routing table\n\n";
       }  


return ($rc);

# if ( $showIpRoute1 =~ /(?m:([0-9\.]+)\/([0-9]+)\s+([a-zA-Z]+)\s+([0-9\.]+)\s+(\d+)\/(\d+)\s+([a-zA-Z]+)(\d+)\/(\d+))/ )
#      {
#        print "parsed prefix as $1\n";
#        print " parsed length as $2\n";
#        print " parsed type as $3\n";
#        print " parsed nexthop as $4\n";
#        print " parsed dist as $5\n";
#        print " parsed metric as $6\n";
#        print " parsed interface as $7\n";
#        print " parsed slot as $8\n";
#        print " parsed port as $9\n";       
#       }  


}

###################################################################
#
#  ping utility
#
#
###################################################################

sub erx_ping
{

 my $self = shift;
 my ($mystring) = '';   # make mystring a local variable and initialize it
 my $successRate = -1; 
 my $gotback = -1;
 my $outof = -1; 
 my $min = -1;
 my $max = -1;
 my $avg = -1;


 my @output = $self->cmd("ping $_[0]");

#print "output is @output\n";
$mystring = join('',@output);

 
# foreach $letter (@output) 
#  {
#   $mystring = $mystring.$letter;
#  }


# print "My string: $mystring\n";
# print "My string2: $mystring2\n";

# Success rate = 100% (5/5), round-trip min/avg/max = 0/0/1 ms
      
   
      if ($mystring =~ /Success\s+rate\s+=\s+(\d+)\%\s+\((\d+)\/(\d+)/) 
       {
       $successRate = $1; 
       $gotback = $2;
       $outof = $3;
       }

 if ($mystring =~ /min\/avg\/max\s+=\s+(\d+)\/(\d+)\/(\d+)/ )
      {
        $min = $1;
        $max = $3;
        $avg = $2;
       }

  # print " min = $min max =$max avg = $avg\n";
  return ($successRate, $gotback, $outof, $min, $max, $avg)

}



sub showLastPrompt
{
my $self;

print "last prompt is $self->last_prompt\n\n";
}

#------------------------------
# Private methods
#------------------------------

# strip backspaces, deletes, kills, and the character they
# pertain to, from an array.
sub _normalize {
    $_ = join "", @_;

    1 while s/[^\cH\c?][\cH\c?]//mg; # ^H ^?
    s/^.*\cU//mg;		     # ^U

    return wantarray ? split /$/mg, $_ : $_; # ORS instead?
}

# Lifted from Net::Telnet en toto
sub _match_check {
    my ($self, $code) = @_;
    return unless $code;

    my $error;
    my @warns = ();

    print "_match_check:\t[Checking: $code]\n" if $DEBUG;
    print "_match_check:\t[Checking self: $$self]\n" if $DEBUG;

    ## Use eval to check for syntax errors or warnings.
    {
	local $SIG{'__DIE__'} = 'DEFAULT';
	local $SIG{'__WARN__'} = sub { push @warns, @_ };
	local $^W = 1;
	local $_ = '';
	eval "\$_ =~ $code;";
    }
    if ($@) {
	## Remove useless lines numbers from message.
	($error = $@) =~ s/ at \(eval \d+\) line \d+.?//;
	chomp $error;
	return $self->error("bad match operator: $error");
    }
    elsif (@warns) {
	## Remove useless lines numbers from message.
	($error = shift @warns) =~ s/ at \(eval \d+\) line \d+.?//;
	$error =~ s/ while "strict subs" in use//;
	chomp $error;
	return $self->error("bad match operator: $error");
    }

    1;
} # end sub _match_check

#------------------------------
# Class methods
#------------------------------

# Look for subroutines in Net::Telnet if we can't find them here.
sub AUTOLOAD {
    my ($self) = @_;
    croak "$self is an [unexpected] object, aborting" if ref $self;
    $AUTOLOAD =~ s/.*::/Net::Telnet::/;
    goto &$AUTOLOAD;
}

1;

__END__

#------------------------------------------------------------
# Docs
#------------------------------------------------------------

=head1 NAME

Net::Telnet::ERX - interact with a ERX router

=head1 SYNOPSIS

  use Net::Telnet::ERX;

  my $session = Net::Telnet::ERX->new(Host => '123.123.123.123');
  $session->login('login', 'password');

  # Execute a command
  my @output = $session->cmd('show version');
  print @output;

  # Enable mode
  if ($session->enable("enable_password") ) {
      @output = $session->cmd('show privilege');
      print "My privileges: @output\n";
  } else {
      warn "Can't enable: " . $session->errmsg;
  }

  $session->close;

=head1 DESCRIPTION

Net::Telnet::ERX provides additional functionality to Net::Telnet
for dealing with ERX routers.

cmd() parses router-generated error messages - the kind that
begin with a '%' - and stows them in $obj-E<gt>errmsg(), so that
errmode can be used to perform automatic error-handling actions.

=head1 CAVEATS

Before you use Net::Telnet::ERX, you should have a good
understanding of Net::Telnet, so read it's documentation first, and
then come back here to see the improvements.



=head1 METHODS

=over 4

=item B<new> - create new Net::Telnet::ERX object

    $session = Net::Telnet::ERX->new(
	[Autopage		  => $boolean,] # 1
	[More_prompt		  => $matchop,] # '/(?m:^\s*--More--)/',
	[Always_waitfor_prompt	  => $boolean,] # 1
	[Waitfor_pause		  => $milliseconds,] # 0.1
	[Normalize_cmd		  => $boolean,] # 1
	[Send_wakeup		  => $when,] # 0
	[Ignore_warnings	  => $boolean,] # 0
	[Warnings		  => $matchop,] # see docs
	
	# Net::Telnet arguments
	[Binmode		  => $mode,]
	[Cmd_remove_mode	  => $mode,]
	[Dump_Log		  => $filename,]
	[Errmode		  => $errmode,]
	[Fhopen			  => $filehandle,]
	[Host			  => $host,]
	[Input_log		  => $file,]
	[Input_record_separator	  => $char,]
	[Option_log		  => $file,]
	[Output_log		  => $file,]
	[Output_record_separator  => $char,]
	[Port			  => $port,]
	[Prompt			  => $matchop,] # see docs
	[Telnetmode		  => $mode,]
	[Timeout		  => $secs,]
    );

Creates a new object. Read `perldoc perlboot` if you don't understand that.

=item B<login> - login to a router

    $ok = $obj->login($username, $password);

    $ok = $obj->login([Name     => $username,]
                      [Password => $password,]
                      [Passcode => $passcode,] # for Secur-ID/XTACACS
                      [Prompt   => $match,]
                      [Timeout  => $secs,]);

All arguments are optional. Some routers don't ask for a
username, they start the login conversation with a password request.

=item B<cmd> - send a command

    $ok = $obj->cmd($string);
    $ok = $obj->cmd(String   => $string,
                    [Output  => $ref,]
                    [Prompt  => $match,]
                    [Timeout => $secs,]
                    [Cmd_remove_mode => $mode,]);

    @output = $obj->cmd($string);
    @output = $obj->cmd(String   => $string,
                        [Output  => $ref,]
                        [Prompt  => $match,]
                        [Timeout => $secs,]
                        [Cmd_remove_mode => $mode,]
                        [Normalize_cmd => $boolean,]);

Normalize_cmd has been added to the default Net::Telnet args. It
lets you temporarily change whether backspace, delete, and kill
characters are parsed in the command output. (This is performed by default)

=item B<prompt> - return control to the program whenever this string occurs in router output

    $matchop = $obj->prompt;

    $prev = $obj->prompt($matchop);

The default cmd_prompt is suitable for
matching prompts like C<router$ >, C<router# >, C<routerE<gt>
(enable) >, and C<router(config-if)# >

Let's take a closer look, shall we?

  (?m:			# Net::Telnet doesn't accept quoted regexen (i.e. qr//)
			# so we need to use an embedded pattern-match modifier
			# to treat the input as a multiline buffer.

    ^			# beginning of line

      [\w.-]+		# router hostname

      \s?		# optional space

      (?:		# Strings like "(config)" and "(config-if)", "(config-line)",
			# and "(config-router)" indicate that we're in privileged
        \(config[^\)]*\) # EXEC mode (i.e. we're enabled).
      )?		# The middle backslash is only there to appear my syntax
			# highlighter.

      \s?		# more optional space

      [\$#>]		# Prompts typically end with "$", "#", or ">". Backslash
			# for syntax-highlighter.

      \s?		# more space padding

      (?:		# Catalyst switches print "(enable)" when in privileged
        \(enable\)	# EXEC mode.
      )?

      \s*		# spaces before the end-of-line aren't important to us.

    $			# end of line

  )			# end of (?m:

If your code  starts timing out when using Net::Telnet::ERX, the prompt regular expression is the first thing
to investigate.

=item B<enable> - enter enabled mode

    $ok = $obj->enable;

    $ok = $obj->enable($password);

    $ok = $obj->enable([Name => $name,] [Password => $password,]
	               [Passcode => $passcode,] [Level => $level,]);

This method changes privilege level to enabled mode, (i.e. root)

If a single argument is provided by the caller, it will be used as
a password. For more control, including the ability to set the
privilege-level, you must use the named-argument scheme.

enable() returns 1 on success and undef on failure.

=item B<is_enabled> - Am I root?

    $bool = $obj->is_enabled;

A trivial check to see whether we have a root-style prompt, with
either the word "(enable)" in it, or a trailing "#".

B<Warning>: this method will return false positives if your prompt has
"#"s in it. You may be better off calling C<$obj-E<gt>cmd("show
privilege")> instead.

=item B<disable> - leave enabled mode

    $ok = $obj->disable;

This method exits the router's privileged mode.

=item B<ios_break> - send a break (control-^)

    $ok = $obj->ios_break;

You may have to use errmode(), fork, or threads to break at the
an appropriate time.

=item B<last_prompt> - displays the last prompt matched by prompt()

    $match = $obj->last_prompt;

last_prompt() will return '' if the program has not yet matched a
prompt.

=item B<always_waitfor_prompt> - waitfor and cmd prompt behaviour

    $boolean = $obj->always_waitfor_prompt;

    $boolean = $obj->always_waitfor_prompt($boolean);

Default value: 1

If you pass a Prompt argument to cmd() or waitfor() a String or Match,
they will return control on a successful match of your argument(s) or
the default prompt. Set always_waitfor_prompt to 0 to return control
only for your arguments.

This method has no effect on login(). login() will always wait for a
prompt.

=item B<waitfor_pause> - insert a small delay before waitfor()

    $boolean = $obj->waitfor_pause;

    $boolean = $obj->waitfor_pause($milliseconds);

Default value: 0.1

In rare circumstances, the last_prompt is set incorrectly. By adding
a very small delay before calling the parent class's waitfor(), this
bug is eliminated. If you ever find reason to modify this from it's
default setting, please let me know.

=item B<autopage> - Turn autopaging on and off

    $boolean = $obj->autopage;

    $boolean = $obj->autopage($boolean);

Default value: 1

IOS pages output by default. It expects human eyes to be reading the
output, not programs. Humans hit the spacebar to scroll page by
page so autopage() mimicks that behaviour. This is the slow way to
handle paging. See the Paging EXAMPLE for a faster way.

=item B<normalize_cmd> - Turn normalization on and off

    $boolean = $obj->normalize_cmd;

    $boolean = $obj->normalize_cmd($boolean);

Default value: 1

JUNOSe clears '--More--' prompts with backspaces (e.g. ^H). If
you're excited by the thought of having raw control characters
like ^H (backspace), ^? (delete), and ^U (kill) in your command
output, turn this feature off.

Logging is unaffected by this setting.

=item B<more_prompt> - Matchop used by autopage()

    $matchop = $obj->prompt;

    $prev = $obj->prompt($matchop);

Default value: '/(?m:\s*--More--)/'.

Please email me if you find others.

=item B<send_wakeup> - send a newline to the router at login time

    $when = $obj->send_wakeup;

    $when = $obj->send_wakeup( 'connect' );
    $when = $obj->send_wakeup( 'timeout' );
    $when = $obj->send_wakeup( 0 );

Default value: 0

Some routers quietly allow you to connect but don't display the
expected login prompts. Sends a newline in the hopes that this
spurs the routers to print something.

'connect' sends a newline immediately upon connection.
'timeout' sends a newline if the connection timeouts.
0 turns this feature off.

I understand this works with Livingston Portmasters.

=item B<ignore_warnings> - Don't call error() for warnings

    $boolean = $obj->ignore_warnings;

    $boolean = $obj->ignore_warnings($boolean);

Default value: 0

Not all strings that begin with a '%' are really errors. Some are just
warnings. By setting this, you are ignoring them. This will show up in
the logs, but that's it.

=item B<warnings> - Matchop used by ignore_warnings().

    $boolean = $obj->warnings;

    $boolean = $obj->warnings($matchop);

Default value:

	/(?mx:^% Unknown VPN
	     |^%IP routing table VRF.* does not exist. Create first$
	     |^%No matching route to delete$

	 )/

Not all strings that begin with a '%' are really errors. Some are just
warnings. 

=back

=head1 EXAMPLES

=head2 Paging

There is internal autopaging support to cmd(). Whenever a '--Page--'
prompt appears on the screen, we send a space right back. It works, but
it's slow. You can also get out of sync if you get returned lines that are longer than
the width specified by the ERX. You'd be better off sending  the following commands
just after login():

  # To a router
  $session->cmd('terminal length 0');
  $session->cmd('terminal width 512');


=head2 Logging

Want to see the session transcript? Just call input_log().

  e.g.
  my $session = Net::Telnet::ERX->new(Host => $router,
					Input_log => "input.log",
					);

See input_log() in L<Net::Telnet> for info.

Input logs are easy-to-read translated transcripts with all of the
control characters and telnet escapes cleaned up. If you want to view
the raw session, see dump_log() in L<Net::Telnet>. If you're getting
tricky and using print() in addition to cmd(), you may also want to use 
output_log().

=head2 Big output

Trying to dump the entire BGP table? (e.g. "show ip bgp") The default buffer size
is 1MB, so you'll have to increase it.

  my $MB = 1024 * 1024;
  $session->max_buffer_length(5 * $MB);

=head2 Sending multiple lines at once

Some commands like "extended ping" and "copy" prompt for several lines
of data. It's not necessary to change the prompt for each
line. Instead, send everything at once, separated by newlines.

For:

  router# ping
  Protocol [ip]:
  Target IP address: 10.0.0.1
  Repeat count [5]: 10
  Datagram size [100]: 1500
  Timeout in seconds [2]:
  Extended commands [n]:
  Sweep range of sizes [n]:

Try this:

  my $protocol  = ''; # default value
  my $ip       = '10.0.0.1';
  my $repeat    = 10;
  my $datagram  = 1500;
  my $timeout   = ''; # default value
  my $extended  = ''; # default value
  my $sweep     = ''; # default value

  $session->cmd(
  "ping
  $protocol
  $ip
  $repeat
  $datagram
  $timeout
  $extended
  $sweep
  ");

If you prefer, you can put the cmd on a single line and replace
every static newline with the "\n" character.

e.g.

  $session->cmd("ping\n$protocol\n$ip\n$repeat\n$datagram\n"
	      . "$timeout\n$extended\n$sweep\n");



=head1 SEE ALSO

L<Net::Telnet>

L<Net::SNMP::Cisco> by Joshua_Keroes@eli.net

L<Net::SNMP>

UCD NetSNMP - http://www.netsnmp.org/

RAT/NCAT - http://ncat.sourceforge.net/

=head1 AUTHOR
 
Net::telnet::ERX by Joe Stankus to support Juniper ERX 2003/04/17 


=head1 COPYRIGHT AND LICENSE

Portions Copyright (c)2003 Joe Stankus
All rights reserved. This program is free software; you
can redistribute it and/or modify it under the same terms
as Perl itself.

Portions Copyright (c) 2000-2002 Joshua Keroes, Electric Lightwave Inc.


