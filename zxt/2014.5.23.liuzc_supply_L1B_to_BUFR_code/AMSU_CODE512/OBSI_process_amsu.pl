#!/usr/bin/perl
# !------------------------------------------------------------------
# !
# !  FILENAME: OBSI_process_amsu.pl
# !
# !  This script searchs for AMSU level 1b input files for selected
# !  satellites within a configurable amount of time before and after
# !  the cycle, converts them to BUFR format and concatenates them 
# !  into single files for AMSU-A and AMSU-B.
# !
# !
# !------------- LOCAL VARIABLES AND INPUT PARAMETERS ---------------
# !
# !  fn_array     : list of files to ship
# !  source       : data type: lite post trim_lite trim_post
# !  thtr         : theater i.e. t2
# !  nest         : nest letter  i.e. 'a'
# !  cycle        : two digit cycle  - HH
# !
# !--------------------- SOFTWARE UNIT HISTORY ----------------------
# !
# !  13 JUN 06 : created, SCR20061163; ccf
# !  08 DEC 06 : Changed pager phone number to be read in from cfg SCR20070249, duffm
# !
# !------------------------------------------------------------------

run_path();
use Env;

$scriptpath = $0;
($scriptname = $scriptpath) =~ s{^.*/}{};
chomp $scriptname;
($scriptbase = $scriptname) =~ s{...$}{};

if(scalar(@ARGV) < 3 )
{
   errorhandler("USAGE: $scriptname "
              . "mode 2_digit_cycle INIT/FCST\n");
}

$mode         = $ARGV[0];
$cycle2d      = $ARGV[1];
$type         = $ARGV[2];

#--------------------------------------------------------------------
#-- Redirect outuput of STDOUT and STDERR to a logfile.
#--
#-- Append to files younger than $days_old value
#-- and overwrite older files.
#--------------------------------------------------------------------

$logname     = "$scriptbase.$cycle2d.log";
$logpath     = "$DATA_DIR/global/OBSI/data/log";
$days_old    = 0.125;
$redirection = ( -e "$logpath/$logname" and
               (-M "$logpath/$logname" < $days_old))
             ? ">>" : ">";
open(STDOUT, "$redirection" . "$logpath/$logname") or
             errorhandler("Could not open log file.");
open(STDERR, ">>" . "&STDOUT") or
             errorhandler("Could not dup STDOUT.");
select STDOUT; $| = 1;   # make unbuffered

print "Starting $scriptname\n";
print "==============================\n";

#--------------------------------------------------------------------
#-- Change to working directory.  Create it if necessary
#--------------------------------------------------------------------

if ( not -d "${DATA_DIR}/global/OBSI/data/AMSU/work")
{
   mkdir "${DATA_DIR}/global/OBSI/data/AMSU/work", 0777;
}

chdir "${DATA_DIR}/global/OBSI/data/AMSU/work";

#--------------------------------------------------------------------
#-- Get the date field for the filenames
#--------------------------------------------------------------------

($junk,$junk,$hr,$d,$m,$yr,$junk,$junk,$junk) = gmtime(time);
$day = sprintf "%02d",$d;
$mon = sprintf "%02d",($m + 1);
$yr4d = $yr + 1900;
if ($hr < $cycle)
{
   $cycle10d = $yr4d . $mon . $day . $cycle2d;
   $cycle10d_offset = qx|/h/UTIL/bin/UTIL_offset_hour.pl $cycle10d -24|;
   $cycle_date = substr($cycle10d_offset, 0, 8);
}
else
{
   $cycle10d = $yr4d . $mon . $day . $cycle2d;
   $cycle_date = $yr4d . $mon . $day;
}

#--------------------------------------------------------------------
#-- Create filenames for collected AMSU-A and AMSU-B files
#--------------------------------------------------------------------

$amsua_file = "obs_amsua_" . $cycle10d . ".3dvar";
$amsub_file = "obs_amsub_" . $cycle10d . ".3dvar";
unlink "../${amsua_file}";
unlink "../${amsub_file}";

#--------------------------------------------------------------------
#-- Read the configuration file
#-- Read the max time difference from cycle to get data for
#-- Generate 14 digit start and stop times
#--------------------------------------------------------------------

$cfg_file  = "${DATA_DIR}/global/SGUI/data/cfg/amsu_config.txt";
open (CFG, "< $cfg_file") or
   errorhandler("Could not open $fullFilename\n");
$minutes_diff = <CFG>;
$end_hr_diff = $minutes_diff / 60;
$mod_diff = $minutes_diff % 60;
if ($mod_diff == 0) {
   $start_hr_diff = $end_hr_diff * -1;
   $start_min = 0;
}
else
{
   $start_hr_diff = ($end_hr_diff + 1) * -1;
   $start_min = 60 - $mod_diff;
}

$start_min2d = sprintf "%02d", $start_min;
$end_min2d = sprintf "%02d", $mod_diff;
$window10d_start = qx|${UTIL_HOME}/bin/UTIL_offset_hour.pl $cycle10d $start_hr_diff|;
chomp($window10d_start);
$window_start = $window10d_start . $start_min2d . "00";
$window10d_end = qx|/h/UTIL/bin/UTIL_offset_hour.pl $cycle10d $end_hr_diff|;
chomp($window10d_end);
$window_end = $window10d_end . $end_min2d . "00";

#--------------------------------------------------------------------
#-- Read the instruments (satellite number and a or b) to use and
#-- a coefficients file which may be null
#--------------------------------------------------------------------

while ($cfg_line = <CFG>)
{
   push @instruments, $cfg_line;
}

#--------------------------------------------------------------------
#-- Get the list of AMSU data files
#--------------------------------------------------------------------

$amsu_directory = "${DATA_DIR}/global/OBSI/data/AMSU";
opendir OBSI_AMSU, $amsu_directory;

@amsu_files = readdir OBSI_AMSU;

#--------------------------------------------------------------------
#-- For each instruement (satellite and A or B)
#-- Get the files for that instrument
#--------------------------------------------------------------------

foreach $instrument (@instruments)
{
   ($instrument_name, $coef_file) = split /\s+/, $instrument;
   @data_files = grep /^$instrument_name/, @amsu_files;

#--------------------------------------------------------------------
#-- For each file get the start and stop time
#--------------------------------------------------------------------

   foreach $data_file (@data_files)
   {
      $fdate = substr ($data_file, 11, 8);
      $fstime = substr ($data_file, 21, 6);
      $fetime = substr ($data_file, 29, 6);
      $file_start = $fdate . $fstime;

      if ($fetime < $fstime)
      {
         $end10d = $fdate . substr ($fetime, 0, 2);
         $end10d_new = qx|/h/UTIL/bin/UTIL_offset_hour.pl $end10d +24|;
         chomp($end10d_new);
         $file_end = $end10d_new . substr ($fetime, 2, 4);
      }
      else
      {
         $file_end = $fdate . $fetime;
      }
      
      print "----------\n";
      print "file end - $file_end\n";
      print "window start - $window_start\n";
      print "file start - $file_start\n";
      print "window end - $window_end\n";
      print "----------\n";

#--------------------------------------------------------------------
#--   If the file intersects the time window to retrieve
#--      Link necessary files
#--      Convert the file to BUFR and concatenate to the aggregated file
#--------------------------------------------------------------------

      if (($file_end > $window_start) && ($file_start < $window_end))
      {
         print "$data_file\n";
         make_tranamsu_namelists ($data_file);
         if ( not -e $coef_file)
         {
            symlink "${DATA_DIR}/global/OBSI/data/cfg/${coef_file}", "./$coef_file";
         }
         if ( not -e "./lowtopog.dat")
         {
            symlink "${DATA_DIR}/global/OBSI/data/cfg/lowtopog.dat", "./lowtopog.dat";
         }
         if ( not -e "./fort.12")
         {
            symlink "${DATA_DIR}/global/OBSI/data/cfg/bufrtab.021", "./fort.12";
         }
         if ($data_file =~ /^amsua/)
         {
            $command = "/sphome/rpeck/AMSU_FOR_NCAR/OBSI_bufr_tranamsua.exe < tranamsu_namelist_ta_${$}.txt";
            print $command;
            qx|$command|;
            $command = "/sphome/rpeck/AMSU_FOR_NCAR/OBSI_bufr_tranamsua.exe < tranamsu_namelist_${$}.txt";
            print $command;
            qx|$command|;
            qx|cat fort.52 >> ${DATA_DIR}/global/OBSI/data/AMSU/$amsua_file|;
         }
         else
         {
            $command = "/sphome/rpeck/AMSU_FOR_NCAR/OBSI_bufr_tranamsub.exe < tranamsu_namelist_ta_${$}.txt";
            print $command;
            qx|$command|;
            $command = "/sphome/rpeck/AMSU_FOR_NCAR/OBSI_bufr_tranamsub.exe < tranamsu_namelist_${$}.txt";
            print $command;
            qx|$command|;
            qx|cat fort.52 >> ${DATA_DIR}/global/OBSI/data/AMSU/$amsub_file|;
         }
      }
   }
}

#--------------------------------------------------------------------
#-- Clean up and copy the file to include INIT or FCST suffix
#--------------------------------------------------------------------

qx|rm -f tranamsu_namelist_ta_${$}.txt tranamsu_namelist_${$}.txt|;
qx|rm -f fort.53 fort.52 tranamsu_workfile|;

if ( -e "${DATA_DIR}/global/OBSI/data/AMSU/${amsua_file}" )
{
   qx|cp ../${amsua_file} ../${amsua_file}.$type|;
}
if ( -e "${DATA_DIR}/global/OBSI/data/AMSU/${amsub_file}" )
{
   qx|cp ../${amsub_file} ../${amsub_file}.$type|;
}
   
#--------------------------------------------------------------------
#-- If on GDS, send the file to GTWAPS-S via TGS
#--------------------------------------------------------------------
$host = `hostname -s`; chomp $host;
if (substr($host_switch, 0, 1) eq "c") 
{
   if ( not open(IN, "</h/data/global/DXFER/data/cfg/siprnet_routes.cfg") )
   {
      errhand("Unable to open /h/data/global/DXFER/data/cfg/siprnet_routes.cfg.");
   } 
   else
   {
      $seq_num = 0;
      @siprnet_dests = <IN>;
      close(IN);
      foreach $dest (@siprnet_dests)
      {
         chomp $dest;
         ($dest_sys, $mais_dest_path) = split /:/,$dest;
         if ( -e "${DATA_DIR}/global/OBSI/data/AMSU/${amsua_file}" )
         {
            $ship_cmd = qq|/h/DXFER/bin/DXFER_ship_file.ksh ftp ${DATA_DIR}/global/OBSI/data/AMSU|
                      . qq| $amsua_file |
                      . qq|$dest_sys $mais_dest_path $amsua_file $$ $seq_num 50|;

            system( $ship_cmd );
            $seq_num++;
            $dist_cmd = qq|${DXFER_HOME}/bin/DXFER_data_dist.ksh ${DATA_DIR}/global/OBSI/data/AMSU|
                      . qq| $amsua_file SGUI_amsu_dec_dist.cfg|;
            system( $dist_cmd );
         }
         if ( -e "${DATA_DIR}/global/OBSI/data/AMSU/${amsub_file}" )
         {
            $ship_cmd = qq|/h/DXFER/bin/DXFER_ship_file.ksh ftp ${DATA_DIR}/global/OBSI/data/AMSU|
                      . qq| $amsub_file |
                      . qq|$dest_sys $mais_dest_path $amsub_file $$ $seq_num 50|;

            system( $ship_cmd );
            $seq_num++;
            $dist_cmd = qq|${DXFER_HOME}/bin/DXFER_data_dist.ksh ${DATA_DIR}/global/OBSI/data/AMSU|
                      . qq| $amsub_file SGUI_amsu_dec_dist.cfg|;
            system( $dist_cmd );
         }
      }
   }
}

close STDERR;
close STDOUT;

sub make_tranamsu_namelists
{
   my($input_file) = shift;
   open (NAMELISTA, ">tranamsu_namelist_ta_${$}.txt") or
             errorhandler("Could not open tranamsu_namelist_ta_${$}.txt.");
   print NAMELISTA "&input\n";
   print "&input\n";
   print NAMELISTA " infile = '../${input_file}'\n";
   print " infile = '../${input_file}'\n";
   print NAMELISTA " outfile = './tranamsu_workfile'\n";
   print " outfile = './tranamsu_workfile'\n";
   print NAMELISTA " coefile = '${coef_file}'\n";
   print " coefile = '${coef_file}'\n";
   print NAMELISTA " compress = 'NO'\n";
   print " compress = 'NO'\n";
   print NAMELISTA " process_Tb = 'NO'\n";
   print " process_Tb = 'NO'\n";
   print NAMELISTA " process_Ta = 'YES'\n";
   print " process_Ta = 'YES'\n";
   print NAMELISTA "/\n";
   print "/\n";
   close NAMELISTA;

   open (NAMELIST, ">tranamsu_namelist_${$}.txt") or
             errorhandler("Could not open tranamsu_namelist_${$}.txt.");
   print NAMELIST "&input\n";
   print "&input\n";
   print NAMELIST " infile = '../${input_file}'\n";
   print " infile = '../${input_file}'\n";
   print NAMELIST " outfile = './tranamsu_workfile'\n";
   print " outfile = './tranamsu_workfile'\n";
   print NAMELIST " coefile = '${coef_file}'\n";
   print " coefile = '${coef_file}'\n";
   print NAMELIST " compress = 'NO'\n";
   print " compress = 'NO'\n";
   print NAMELIST " process_Tb = 'YES'\n";
   print " process_Tb = 'YES'\n";
   print NAMELIST " process_Ta = 'NO'\n";
   print " process_Ta = 'NO'\n";
   print NAMELIST "/\n";
   print "/\n";
   close NAMELIST;

}

sub errorhandler
{
   my($errmsg) = shift; chomp($errmsg);
#--------------------------------------------------------------------
#-- if production
#--    define abort file
#--    set calling arguments to local variables
#--------------------------------------------------------------------
   my($prod) = "prod";
   if ($mode eq $prod and $HOST_TYPE eq $prod)
   {
      my($date1)     = `date -u +%d%h%y_%H%M`; chomp($date1);
      my($date2)     = `date`; chomp($date2);
      my($abort)     = "${HOME}/abt.$date1.$0";
      my($err_cycle) = `date -u +%y%j%H00`; chomp($err_cycle);
      my($host)      = `hostname -s`; chomp($host);
      my $phone=`grep PHONE_NUMBER /h/data/global/UTIL/data/cfg/abortContactInfo.txt | cut -d= -f2`;

#--------------------------------------------------------------------
#--   report error to tivoli
#--   open abort file and write message
#--------------------------------------------------------------------

      my($abort_msg) = sprintf <<EOF;

------------------------------------------
--- PROBLEM SUMMARY
---
--- Platform: $host, GTWAPS SP2
--- Process:  $0
---
--- Category 6 -- Output not generated...
--- Error   = $errmsg
--- Theater = ${theater}${nest}.${prod_id}
--- Cycle   = $err_cycle  Hour = ${fcst_hour}
--- Date    = $date2
---
------------------------------------------
--- ASPOC ACTIONS
---
--- Call the MM5 beeper ($phone).
---
------------------------------------------
--- AUTOMATED ACTIONS
---
--- No automated responses defined.
---
------------------------------------------

EOF

      my($tivoli_cmd) =
        qq\/h/PLNR/bin/PLNR_report_error $0 "$abort_msg"\;
      system ($tivoli_cmd);

      open ABORT, ">$abort";
         print ABORT "$abort_msg";
      close ABORT;

#--------------------------------------------------------------------
#--    get the list of users and mail them a message
#-- terminate the process for both production and development
#--------------------------------------------------------------------
      open USER_LIST, "grep @ $SGUI_DATA/cfg/mailerrs|";
      while (<USER_LIST>)
      {
         chomp;
         my($mail_cmd) =
            qq\mail -s "$0 ABORT ON $host" $_ < $abort_msg\;
         system($mail_cmd);
      }
      close USER_LIST;
   }
   print "$errmsg\n";
   die "$0 terminating on error\n";
}

#--------------------------------------------------------------------
#-- Use PLNR_path.run
#--
#-- if PLNR_path.run has not been executed
#--    if valid mode
#--       execute PLNR_path.run
#--    PLNR_path.run did not run if this line is executed
#-- else
#--    remove token argument
#--------------------------------------------------------------------
sub run_path
{
   my($path_has_run) = "path_has_run";
   my($prod)         = "prod";
   my($devel)        = "devel";

   if(scalar(@ARGV) > 0 and $ARGV[0] ne $path_has_run)
   {
      $mode = $ARGV[0];

      if(($mode eq $prod or $mode eq $devel) or
         errorhandler("First argument should be: prod/devel\n"))
      {
         unshift @ARGV, $path_has_run;
         if( -e "/h/PLNR/bin/PLNR_path.run")
         {
            exec ". /h/PLNR/bin/PLNR_path.run $mode > /dev/null 2>&1; \
                  set +x; \
                  $0 @ARGV" ;
         }
      }
      errorhandler("Could not execute PLNR_path.run\n");
   }
   else
   {
      shift @ARGV;
   }
}
