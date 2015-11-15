#!/usr/bin/perl

################
# erx_grab_info.pl
#
# Written by Justin Ryburn (jryburn@juniper.net)
# Last revised: 02/25/09
#
# This script is designed to log in to a list of ERX routers and gather information.
# useful for JTAC to do troubleshooting.
# The hard coded commands
# gathered are generic in nature and may not cover everything JTAC will ask for. Additional
# commands may be specified in commands.txt file.
################

################
# Setup the variables and procedure calls.
################

use Net::FTP;
use Net::Telnet::ERX;

my $user = "user"; # scipt account username
my $pw = "password"; # script account password
my $enpw = "enpassword"; # enable password for the router

my $routerlist="router_list.txt"; # routers to run this against
my $cmds = "commands.txt"; # specific commands to be run

# Generic commands we always run.
my @gen_cmds = (
	"show hard",
 	"show env",
	"show util",
	"show red",
	"show proc",
	"show proc mem",
	"show subscribers sum",
	"show user all",
	"show ver",
	"show reboot-history",
	"show ip int br",
	"show ip bgp summary",
	"show ip ospf neighbor",
	"show ip route summary",
	"show ip traffic",
	"show ip traffic", # second time is intended
	"show log data nv > show_log_nv.txt",
	"show log data severity 7 > show_log_sev_7.txt",
	"show conf > config.scr",
	"copy run config.cnf",
	"copy standby:reboot.hty standby.hty"
);


&UserInteraction(); # Get information we need from the user.
&MkFtpDir(); # Make the directory on the FTP server.

# Open the router list.
open(INFILE,"$routerlist") || die "Cant open $routerlist\n";
@list=<INFILE>;
chomp @list;

# For each router we need to gather information and send it to Juniper.
my $i=0;
while ($i < @list){

    # Establish a telnet session
	my $tnet = Net::Telnet::ERX->new(
				 Host		=> $list[$i],
				 Input_log	=> "$list[$i].log",
				 Timeout	=> 90
		 ) ;

	my $MB = 1024 * 1024;
	$tnet->max_buffer_length(5 * $MB);

	# Log in to the router.
 	$tnet->login(Username => $user);
	$tnet->login(Password => $pw) ;

# Commented out since radius puts the user in enable mode.
#	Enter enable mode
# 	my $erxEnabled = 0;
# 	if ($tnet->enable($enpw) )
# 	{
# 	$erxEnabled = 1;
# 	} else {
# 	  warn "Can't enable: " . $tnet->errmsg;
# 	}

	@line = $tnet->cmd("term len 0");

	&GenCmds($tnet); # Run the generic commands.
	&SpecCmds($tnet); # Run the specific commands.
	&SendRemote($tnet, $list[$i]); # Send the files on the router flash to Juniper.

	#Exit the telnet session.
	$tnet->close;

	# Push local captures out to Juniper.
	$ftp = Net::FTP->new("ftp.juniper.net", Debug => 0)
		or die "Cannot connect to ftp.juniper.net: $@";

	$ftp->login("ftp",'user@')
		or die "Cannot login ", $ftp->message;

	$ftp->cwd("/pub/incoming/$jtac_case")
		or die "Cannot change working directory ", $ftp->message;

	$ftp->put("$list[$i].log")
		or die "Transfer failed ", $ftp->message;

	$ftp->quit;

	# Tell the user each router is done as it completes.
	print " $list[$i] is done\n";

	# Wash, rinse, and repeat!
	sleep(2);
	$i++;
}

exit 0; # Exit cleanly we hope!

sub UserInteraction {
	################
	# Gather information from the user.
	################
	print "Enter Jtac Case Number: ";
	chomp($jtac_case = <>);
	print "Enter .dmp file name or enter to continue: ";
	chomp($dump_file = <>);
}

sub MkFtpDir {
	################
	# Setup the directory on the FTP server.
	################
	$ftp = Net::FTP->new("ftp.juniper.net", Debug => 0)
		or die "Cannot connect to ftp.juniper.net: $@";

	$ftp->login("ftp",'user@')
		or die "Cannot login ", $ftp->message;

	$ftp->mkdir("/pub/incoming/$jtac_case")
		or die "Cannot create directory $jtac_case ", $ftp->message;

	$ftp->quit;
}

sub GenCmds {
	################
	# Run the generic commands.
	################
	local ($n=0, $tnet=$_[0]);
	while ($n < @gen_cmds) {
		@line = $tnet->cmd("sho clock");
		chomp($gen_cmds[$n]);
		# Add the name of the command being run to make the output easier to read.
		@line = $tnet->cmd("! ************ $gen_cmds[$n] ************");
		@line = $tnet->cmd(String => "$gen_cmds[$n]");
		$n++;
	}
}

sub SpecCmds {
	################
	# Run the specific commands.
	################
	local ($tnet = $_[0]);
	open(CMD, "$cmds");
	while (<CMD>) {
		@line = $tnet->cmd("sho clock");
		chomp;
		@line = $tnet->cmd(String => "$_");
	}
	close(CMD);
}

sub SendRemote {
	################
	#Send the files on the router flash to Juniper
	################

	local ($n=0, $tnet=$_[0]);
	#Commands to copy files to Juniper
	my @send_cmds = (
		"copy reboot.hty ftp.juniper.net:/pub/incoming/$jtac_case/$_[1]_reboot.hty",
		"copy standby.hty ftp.juniper.net:/pub/incoming/$jtac_case/$_[1]_standby.hty",
		"copy config.scr ftp.juniper.net:/pub/incoming/$jtac_case/$_[1]_config.scr",
		"copy config.cnf ftp.juniper.net:/pub/incoming/$jtac_case/$_[1]_config.cnf",
		"copy $dump_file ftp.juniper.net:/pub/incoming/$jtac_case/$_[1]_$dump_file",
		"copy show_log_nv.txt ftp.juniper.net:/pub/incoming/$jtac_case/$_[1]_show_log_data_nv.txt",
		"copy show_log_sev_7.txt ftp.juniper.net:/pub/incoming/$jtac_case/$_[1]_show_log_data_sev_7.txt",
		"del standby.hty",
		"del config.scr",
		"del config.cnf",
		"del show_log_nv.txt",
		"del show_log_sev_7.txt"
	);

	while ($n < @send_cmds) {
		print $send_cmds[$n];
 		chomp($send_cmds[$n]);
		@line = $tnet->cmd(String => "$send_cmds[$n]");
		$n++;
	}
}
