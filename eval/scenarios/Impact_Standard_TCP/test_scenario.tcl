#
# Copyright (c) 2011
#  Swinburne University of Technology, Melbourne, Australia
#  All rights reserved.
#
# Copyright (c) 2013
#  University of Oslo, Norway
#  All rights reserved.
#
#
# Released under the GNU General Public License version 2.
#
# Author
#  - David Hayes (davihay@ifi.uio.no or david.hayes@ieee.org)
#
# Note that inital work on this test suite was done by:
# - Gang Wang (wanggang@research.nec.com.cn)
# - Yong Xia   (xiayong@research.nec.com.cn)
#
#              S_1                                  S_4
#              T_1                                  T_4 
#                 \                                / 
#                  \        central link          / 
#          S_2 --- Router_1 -------------- Router_2 --- S_5 
#          T_2     /                              \     T_5
#                 /                                \ 
#              S_3                                  S_6
#              T_3                                  T_6
#
#               Figure: Dumbbell test topology
# 
# Where the "S" nodes are standard TCP, 
# and the "T" nodes are the TCP variety under test
#
# Note that each S and T node has its own link to the Router with identical
# delay and bandwidth characteristics
#
# environment setting
# [include external source code]
source $env(TCPEVAL)/tcl/create_topology.tcl
source $env(TCPEVAL)/tcl/create_traffic.tcl
source $env(TCPEVAL)/tcl/shuffle_traces.tcl
set if_wireless 0               ;# default to not wireless
#
# defaults for this scenario
#
### Tmix
set num_tmix_flow 9		;# number of tmix flows per TCP variety (9 for S and 9 for T)
set num_tmix_node 3		;# number of tmix nodes per TCP variety (3 for S and 3 for T)
set tmix_pkt_size 1460          ;# default packet size. overriden by m record in cv files
#
#
# debug output files (will be prefixed with an S or T )
set tmix_debug_output [list "flow1" "flow2" "flow3" "flow4" "flow5" "flow6" "flow7" "flow8" "flow9"]
#
# include parameter files
#
source ./tcl_base_setup
source ./defaults.tcl

# finish process
proc finish { fmonF fmonR } {
    global ns topo traffic tracefd_ TIME SaveTraceFile
    global findtarget prevstat scale prefill_t prefill_si targetload TargetDirection 
    global pcntLoad  pcntBdep pcntPLoss measuretime test_time
    global result_filename nsoutfd_ tmp_directory_ error_
    global warmup Incremental_display_interval finishplus longflowthresh
    global fmonFsize fmonRsize fmonFdelay fmonRdelay avBQsize

    if { [$ns get-ns-traceall] != "" && $SaveTraceFile > 0} {
	$ns flush-trace
	close $tracefd_
    }
    if {$findtarget } { 
	#Split thoughput in half and stop when two halves are
	#statistically similar after the warmup time.
	set numsamples [llength $pcntBdep($TargetDirection)]
	if {$longflowthresh > 0} {
	    set minwarmup [expr int(ceil(1.0*$longflowthresh/$Incremental_display_interval))]
	} else {
	    set minwarmup 1
	}
	#search for warmup through first half or up to testtime
	set half [expr $numsamples/2.0]
        set measuretime [expr [$ns now] - $prefill_t - $finishplus]
	for { set wu $minwarmup; set rest [expr $numsamples - $wu ]} { $wu <= $half } { incr wu; incr rest -1 } {
	    set mid [expr $wu + round($rest / 2.0) ]
	    set qs_l  [lrange $avBQsize($TargetDirection) $wu $mid-1 ]
	    set qs_u  [lrange $avBQsize($TargetDirection) $mid end ]
	    set avq_l [expr ([ join $qs_l +]+0.0) / [llength $qs_l]]	  
	    set avq_u [expr ([ join $qs_u +]+0.0) / [llength $qs_u]]
	    set pbd_l [lrange $pcntBdep($TargetDirection) $wu $mid-1 ]
	    set pbd_u [lrange $pcntBdep($TargetDirection) $mid end ]
	    set avd_l [expr ([ join $pbd_l +]+0.0) / [llength $pbd_l]]	  
	    set avd_u [expr ([ join $pbd_u +]+0.0) / [llength $pbd_u]]
	    if {$avq_l <= 1  || $avq_u <= 1} {
	    	set avq_tol  [expr abs($avq_l - $avq_u)]
	    } else {
	    	set avq_tol  [expr abs($avq_l - $avq_u) / $avq_l]
	    }
	    puts stderr "---finding warmup--- wu=$wu, mid=$mid, avd_l=$avd_l, avd_u=$avd_u, avq_l=$avq_l, avq_u=$avq_u"

	    if { [expr abs($avd_l - $avd_u) / $avd_l] <= $error_ } {
	    	# When load is high, the queue lengh is not a stable measure.
	    	if {($targetload > 70.0 && $targetload < 105)|| $avq_tol <= (2.0*$error_)} {
		    set warmup [expr $wu * $Incremental_display_interval]
		    set TIME [expr $warmup + $prefill_t]
		    puts $nsoutfd_ [format "Calculated Times: Warmup = %6.3g, Test time = %6.3g" $warmup [expr $measuretime - $warmup] ]
		    break
	    	}
	    }
	}
	if { $warmup == 0 } {
		puts $nsoutfd_ "WARNING: warmup could not be calculated"
		puts stderr "WARNING: warmup could not be calculated"
	}
    } else {
	set measuretime [expr [$ns now] - $TIME - $finishplus]
    }
    set numFsamples [$fmonFdelay cnt]
    set numRsamples [$fmonRdelay cnt]
    set avlossF [expr 100.0 * $prevstat(total,pkt,drop,F)/$prevstat(total,pkt,arr,F) ]
    set avlossR [expr 100.0 * $prevstat(total,pkt,drop,R)/$prevstat(total,pkt,arr,R) ]
    if { $numFsamples > 0  && $numRsamples > 0} {
    	set avQsizeBF [expr [$fmonFsize set sum_]/ $measuretime]
    	set avQsizeBR [expr [$fmonRsize set sum_]/ $measuretime]
    	set avQwaitF [$fmonFdelay mean]
    	set avQwaitR [$fmonRdelay mean]
    } else {
     	set avQsizeBF inf
     	set avQwaitF  inf
     	set avQsizeBR inf
     	set avQwaitR  inf
    }

    puts $nsoutfd_ [format "Traffic Summary : Total F/R(Mbps) %6.3g / %6.3g, Test flows %s Arr(Mbps)/Drop: Test %6.3g / %5.2f %%, Standard %6.3g / %5.2f %%" \
    			[expr $prevstat(total,byte,arr,F)*8.0/1e6/$measuretime] \
    			[expr $prevstat(total,byte,arr,R)*8.0/1e6/$measuretime] \
    			$TargetDirection \
    			[expr $prevstat(TestTCP,byte,arr,$TargetDirection)*8.0/1e6/$measuretime] \
    			[expr 100.0*$prevstat(TestTCP,pkt,drop,$TargetDirection)/$prevstat(TestTCP,pkt,arr,$TargetDirection)] \
    			[expr $prevstat(StdTCP,byte,arr,$TargetDirection)*8.0/1e6/$measuretime] \
    			[expr 100.0*$prevstat(StdTCP,pkt,drop,$TargetDirection)/$prevstat(StdTCP,pkt,arr,$TargetDirection)] ]
    puts $nsoutfd_ [format "Summary (F/R): Av Q Dly %6.3g s / %6.3g s, Av Q Sz %6.3g B / %6.3g B, Av Loss %6.3f %% / %6.3f %%, scale %8.6f" \
    			$avQwaitF $avQwaitR $avQsizeBF $avQsizeBR $avlossF $avlossR \
    			$scale ]

    ############################### set up result file ####################################
    set resultfd_ [open $tmp_directory_/result_$result_filename w]
    puts $resultfd_ [format "Overall Throughput (bps) Sbps/Tbps\tStd Bytes-SB\tStd Dropped Bytes-SDP\tTest Bytes (B)-TB\tTest Bytes Dropped (B)-TDP\tAverage Queueing Delay-AQD (s)\tAverage Queue Size-AQS (B)\tAverage Packet Loss-APL (%%)\tTarget Data\tTmix scale-Scale"]
    puts $resultfd_ [format "Sbps L->R, Sbps R->L, Tbps L->R, Tbps R->L, SP L->R, SP R->L, SDP L->R, SDP R->L, TP L->R, TP R->L, TDP L->R, TDP R->L, AQD L->R, AQD R->L, AQS L->R, AQS R->L APL L->R, APL R->L, (B), Scale"]
    puts $resultfd_ [format "%8.4f, %8.4f, %8.4f, %8.4f, %d, %d, %d, %d, %d, %d, %d, %d, %6.3g, %6.3g, %6.3g, %6.3g, %6.3f, %6.3f, %8.6f" \
    			 [expr $prevstat(StdTCP,byte,dep,F)*8.0/$measuretime] \
    			 [expr $prevstat(StdTCP,byte,dep,R)*8.0/$measuretime] \
    			 [expr $prevstat(TestTCP,byte,dep,F)*8.0/$measuretime] \
    			 [expr $prevstat(TestTCP,byte,dep,R)*8.0/$measuretime] \
    			 $prevstat(StdTCP,byte,dep,F) $prevstat(StdTCP,byte,dep,R) \
    			 $prevstat(StdTCP,byte,drop,F) $prevstat(StdTCP,byte,drop,R) \
    			 $prevstat(TestTCP,byte,dep,F) $prevstat(TestTCP,byte,dep,R) \
    			 $prevstat(TestTCP,byte,drop,F) $prevstat(TestTCP,byte,drop,R) \
    			 $avQwaitF $avQwaitR $avQsizeBF $avQsizeBR \
    			 $avlossF $avlossR $scale]
    
    close $resultfd_
    flush $nsoutfd_
    $topo finish
    $traffic finish
	
    $ns halt
}

proc monitorcntlnkflows {fmonF fmonR} {
    global ns Incremental_display_interval display_counter TIME
    global pcntBdep pcntLoad pcntPloss avBQsize BQsum
    global prevstat tmix_R tmix_L TNsrc TNsnk
    global cntlnk_bw num_tmix_node loadF loadR
    global scale nsoutfd_ test_time
    global fmonFsize fmonRsize fmonFdelay fmonRdelay

    set measuretime [expr [$ns now] - $TIME]
    
    set fcF [$fmonF classifier]
    set fcR [$fmonR classifier]
    ####################### collect traffic stats ###################
    foreach fl { TestTCP StdTCP } gl { T S } {
	foreach tp { pkt byte } pre { p b } {
	    foreach act { arr dep drop } var { arrivals_ departures_ drops_ } {
		set currstat($fl,$tp,$act,F) 0
		set currstat($fl,$tp,$act,R) 0
		for { set l 0 } { $l < $num_tmix_node } { incr l } {
		    for { set r 0 }  { $r < $num_tmix_node } { incr r } {
			set flF [$fcF lookup auto [$tmix_L($gl,$l) id] [$tmix_R($gl,$r) id] 0]
			set flR [$fcR lookup auto [$tmix_R($gl,$r) id] [$tmix_L($gl,$l) id] 0]
			if { $flF != "" } {
			    set currstat($fl,$tp,$act,F) \
				[expr $currstat($fl,$tp,$act,F) + [ $flF set $pre$var ] ]
			}
			if { $flR != "" } {
			    set currstat($fl,$tp,$act,R) \
				[expr $currstat($fl,$tp,$act,R) + [ $flR set $pre$var ] ]
			}
		    }
		}
		set incrstat($fl,$tp,$act,F) \
		    [expr $currstat($fl,$tp,$act,F) - $prevstat($fl,$tp,$act,F)]
		set incrstat($fl,$tp,$act,R) \
		    [expr $currstat($fl,$tp,$act,R) - $prevstat($fl,$tp,$act,R)]
	    }
	}
    }
    foreach dir { F R } dn { 0 1 } {
	if { $incrstat(TestTCP,pkt,arr,$dir) > 0 || $incrstat(StdTCP,pkt,arr,$dir)  > 0 } {
	    set pcntPdrop($dir) \
		[expr 100.0 *($incrstat(TestTCP,pkt,drop,$dir)+$incrstat(StdTCP,pkt,drop,$dir)) / ($incrstat(TestTCP,pkt,arr,$dir)+$incrstat(StdTCP,pkt,arr,$dir))]
	} else {
	    set pcntPdrop($dir) 0
	}
	set bbw [lindex $cntlnk_bw $dn]
	set pcntBdepart($dir) \
	    [expr 100.0*($incrstat(TestTCP,byte,dep,$dir)+$incrstat(StdTCP,byte,dep,$dir))*8.0/$Incremental_display_interval/$bbw/1e6]

	set pcntLd($dir) \
	    [expr 100.0 * ($incrstat(TestTCP,byte,arr,$dir)+$incrstat(StdTCP,byte,arr,$dir))*8.0/$Incremental_display_interval/$bbw/1e6]

	lappend pcntBdep($dir) $pcntBdepart($dir)
	lappend pcntLoad($dir) $pcntLd($dir)
	lappend pcntPloss($dir) $pcntPdrop($dir)
    }

    lappend avBQsize(F) [expr ([$fmonFsize set sum_] - [lindex $BQsum(F) end])/ $Incremental_display_interval]
    lappend avBQsize(R) [expr ([$fmonRsize set sum_] - [lindex $BQsum(R) end])/ $Incremental_display_interval]
    lappend BQsum(F) [$fmonFsize set sum_]
    lappend BQsum(R) [$fmonRsize set sum_]
    ####################### collect total statistics ###########################
    foreach tp { pkt byte } pre { p b } {
	foreach act { arr dep drop } var { arrivals_ departures_ drops_ } {
	    set currstat(total,$tp,$act,F) [ $fmonF set $pre$var ]
	    set currstat(total,$tp,$act,R) [ $fmonR set $pre$var ]
	    set incrstat(total,$tp,$act,F) \
		[expr $currstat(total,$tp,$act,F) - $prevstat(total,$tp,$act,F) ]
	    set incrstat(total,$tp,$act,R) \
		[expr $currstat(total,$tp,$act,R) - $prevstat(total,$tp,$act,R) ]
	}
    }
    #
    set incrloadF [expr 100.0*($incrstat(total,byte,arr,F) *8.0/1e6/$Incremental_display_interval)/[lindex $cntlnk_bw 0] ]
    set incrloadR [expr 100.0*($incrstat(total,byte,arr,R) *8.0/1e6/$Incremental_display_interval)/[lindex $cntlnk_bw 1] ]
    #################### print progress statistics ##############################
    puts $nsoutfd_ [format "Time %4.1f Incremental Stats Mbps (F/R): Total %6.3g / %6.3g, Load %5.2g %% / %5.2g %%, AvQ B %5.2g / %5.2g " \
			[expr $TIME+$display_counter*$Incremental_display_interval] \
			[expr $incrstat(total,byte,arr,F) *8.0/1e6/$Incremental_display_interval] \
			[expr $incrstat(total,byte,arr,R) *8.0/1e6/$Incremental_display_interval] \
			$incrloadF $incrloadR [lindex $avBQsize(F) end] [lindex $avBQsize(R) end] ]
    puts $nsoutfd_ [format "   Std (F/R): Arr(Mbps) %6.3g / %6.3g, Loss %5.2f %% / %5.2f %%; Test (F/R): Arr(Mbps) %6.3g / %6.3g, Loss %5.2f %% / %5.2f %%" \
			[expr $incrstat(StdTCP,byte,arr,F) *8.0/1e6/$Incremental_display_interval] \
			[expr $incrstat(StdTCP,byte,arr,R) *8.0/1e6/$Incremental_display_interval] \
			[expr 100.0 * $incrstat(StdTCP,pkt,drop,F)/$incrstat(StdTCP,pkt,arr,F) ] \
			[expr 100.0 * $incrstat(StdTCP,pkt,drop,R)/$incrstat(StdTCP,pkt,arr,R) ] \
			[expr $incrstat(TestTCP,byte,arr,F) *8.0/1e6/$Incremental_display_interval] \
			[expr $incrstat(TestTCP,byte,arr,R) *8.0/1e6/$Incremental_display_interval] \
			[expr 100.0 * $incrstat(TestTCP,pkt,drop,F)/$incrstat(TestTCP,pkt,arr,F) ] \
			[expr 100.0 * $incrstat(TestTCP,pkt,drop,R)/$incrstat(TestTCP,pkt,arr,R) ] ]

    flush $nsoutfd_

    ################# save currstats as prevstats ###########################
    foreach fl { total TestTCP StdTCP } {
	foreach tp { pkt byte } {
	    foreach act { arr dep drop } {
		foreach dir { F R } {
		    set prevstat($fl,$tp,$act,$dir) $currstat($fl,$tp,$act,$dir)
		}
	    }
	}
    }
    ############## set up for next call
    set display_counter [ expr $display_counter + 1 ]
    $ns at [ expr $TIME+$display_counter * $Incremental_display_interval ] "monitorcntlnkflows $fmonF $fmonR"
}


# reset flow monitors
proc resetmonitors {fmonF fmonR} {
    global fmonFsize fmonRsize fmonFdelay fmonRdelay
    global tmix_R tmix_L num_tmix_node
    # reset doesn't reset the per flow statistics properly
    set fcF [$fmonF classifier]
    set fcR [$fmonR classifier]
    foreach gl { T S } {
	foreach pre { p b } {
	    foreach var { arrivals_ departures_ drops_ } {
		for { set l 0 } { $l < $num_tmix_node } { incr l } {
		    for { set r 0 }  { $r < $num_tmix_node } { incr r } {
			set flF [$fcF lookup auto [$tmix_L($gl,$l) id] [$tmix_R($gl,$r) id] 0]
			set flR [$fcR lookup auto [$tmix_R($gl,$r) id] [$tmix_L($gl,$l) id] 0]
			if { $flF != "" } {
			    $flF set $pre$var 0
			}
			if { $flR != "" } {
			    $flR set $pre$var 0
			}
		    }
		}
	    }
	}
    }
    $fmonF reset
    $fmonR reset
    $fmonFsize reset
    $fmonRsize reset
    $fmonFdelay reset
    $fmonRdelay reset
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Simulation setup
#
set NumExperiments [array size ExperimentNames]
set skipexperiment 0
set targetload 0
if {$ExperimentIteration > $NumExperiments} {
    set $NewExperimentIteration -1 
    puts stdout [format "tcpevaliterations,%d" \
		     $NewExperimentIteration]
    exit
} else {
    set ThisExperiment $ExperimentNames($ExperimentIteration)
    set test_time $TestTime($ThisExperiment)
    if {$findtarget == 0 || $TargetIter == 1} {
	set scale  $Scale($ThisExperiment)
    }
    if { $findtarget } {
	if { [array exists TargetLoad] } {
	    if { [ info exists TargetLoad($ThisExperiment) ] } {
		set targetload $TargetLoad($ThisExperiment)
	    } else {
		# No target defined for this experiment, so continue, but don't find target
		puts stderr "Warning findtarget=1. TargetLoad specified, but not for this experiment"
		puts stderr "---> Continuing test with findtarget=0"
		set findtarget 0
	    }
	} else { 
	    puts stderr "Warning findtarget=1, but no TargetLoad specified" 
	    puts stderr "---> Skipping this test"
	    set skipexperiment 1
	}
    }
	
    set result_filename ${result_basename}_$ThisExperiment
    if { [ file exists $tmp_directory_/result_$result_filename ] && $TargetIter == 1} {
	#skip already experiments we have already done
	puts stderr "$tmp_directory_/result_$result_filename exists, so skipping"
	set skipexperiment 1
    }
    if { $skipexperiment == 1 } {
	incr ExperimentIteration
	if {$ExperimentIteration > $NumExperiments} {
	    set NewExperimentIteration -1 
	} else {
	    set NewExperimentIteration $ExperimentIteration
	}
	set scale -1.0
	puts stdout [format "tcpevaliterations,%d,%f" \
			 $NewExperimentIteration $scale]
	exit 0
    }
}
puts stderr "---> $ThisExperiment"
if {$findtarget} {
    set nsoutfd_ [open $tmp_directory_/loadtarget_$result_filename a]
} else {
    set nsoutfd_ [open $tmp_directory_/nsout_$result_filename a]
}

proc amin {arr} {
    upvar $arr a
    set mn Inf
    set an ""
    foreach e [array names a] {
	if { $a($e) < $mn } {
	    set mn $a($e)
	    set an $e
	}
    }
    return $an
}
################################################################################
####### Simulation time parameters #####################################
set buff_s [expr 1.0 * [lindex $buffer_length 0] / 1000]
if { ![info exists BottleneckCapacity] } {
    if { $TargetDirection == "F" } { 
	set BottleneckCapacity [lindex $cntlnk_bw 0]
    } else {
	set BottleneckCapacity [lindex $cntlnk_bw 1]
    }
}
set BCbps [expr $BottleneckCapacity * 1.0 * 10**6]
set maxbuff_in_bits [expr $BCbps * $buff_s ]
puts stderr "scale = $scale, buf = $maxbuff_in_bits, BC=$BCbps"
#heuristic for shuffling bin size - based on 5s for 100Mbps
set binsecs [expr 500.0 / $BottleneckCapacity]
set maxtrace [expr 3000.0 * $scale]
if {[info exists Warmup($ThisExperiment)]} {
    set warmup $Warmup($ThisExperiment)
}
if {[info exists Prefill_t($ThisExperiment)]} {
    set prefill_t $Prefill_t($ThisExperiment)
} else {
    # heuristic based on BDP:
    set prefill_t [expr 1.5 * $targetload * $maxRTT ]
}
if { [info exists warmup] } {
    set sim_time [expr $test_time + $warmup + $prefill_t]
} else {
    set warmup 0.0
    set sim_time [expr $test_time + $warmup + $prefill_t]
}
if { $sim_time > $maxtrace } {
    set sim_time $maxtrace
} 
# concurrent tmix sources is not doubled as we are using half the target load.
set cc_tmix_srcs [llength $tmix_base_cv_name]
set longflowthresh [expr 0.5 * $prefill_t ]
set pkt_overhead 40
set shufbalancetol 0.05
set shufloadtol 0.02
set findstats 0
set shuff [new Shuffle]
#note that the target load is halved since we offer each traffic source twice.
set shufrtns [$shuff shuffle_traces $scale $sim_time $binsecs $tmix_base_cv_name \
		  $findstats $findtarget $prefill_t $BCbps $cc_tmix_srcs $maxRTT \
		  [expr $targetload/2.0] $TargetDirection $longflowthresh \
		  $tmix_pkt_size  $pkt_overhead \
		 $shufbalancetol $shufloadtol]
set tmix_shuff_cv_name [lindex $shufrtns 0]
set tracestats [lindex $shufrtns 1]
set scale [lindex $shufrtns 2]
set sim_time [lindex $shufrtns 3]
set numTmixSrcs [llength $tmix_shuff_cv_name]
set conn_rate [lindex $tracestats 0]
if { $TargetDirection == "F" } { 
    set total_data_in_bits [expr [lindex $tracestats 3] * 8 ]
    set total_prefill_in_bits [expr [lindex $tracestats 1] * 8 ]
} elseif { $TargetDirection == "R" } {
    set total_data_in_bits [expr [lindex $tracestats 4] * 8 ]
    set total_prefill_in_bits [expr [lindex $tracestats 2] * 8 ]
} else {
    set total_data_in_bits [expr [lindex $tracestats 3] * 8 + [lindex $tracestats 4] * 8 ]
    set total_prefill_in_bits [expr [lindex $tracestats 1] * 8 ]
}
if { $prefill_t == 0 } {
    set prefill_si 0
} else {
    # prefill startup interval
    # heuristic based on filling pipes and buffers to targetload
    # if $prefill_si has not been set
    if {[info exists Prefill_si($ThisExperiment)]} {
	set prefill_si $Prefill_si($ThisExperiment)
    } else {
	set prefill_si [expr $total_prefill_in_bits / ($BCbps * $targetload / 100.0) ]	;# prefill startup interval.
	set nsbasefd_ [open $tmp_directory_/tcl_base_setup a]
	puts $nsbasefd_ "set Prefill_si($ThisExperiment) $prefill_si"
	close $nsbasefd_
	puts stderr "prefill data = $total_prefill_in_bits, prefill_si = $prefill_si"
    }
    if {$prefill_t < $prefill_si} {
	puts stderr "Warning: prefill_t must be larger than prefill_si, setting prefill_t and prefill_si to 0, as prefill is not needed for this experiment"
	set prefill_si 0
	set prefill_t 0
    }
}
puts stderr "---> $ThisExperiment , Prefill=$prefill_t, maxRTT = $maxRTT"
###############################################################################
######################### Test Tmix source files  ######################
# At one time I tried to give a random offset for the start of each
# connection vector in the files. But since tmix goes through the connections
# in the order they are in the file, this caused large bursts as the connection 
# start times where not ordered in the resulting connection vector file.
# Of course the whole file could be sorted, but this is time consuming.
# Instead the standard and test connection vectory files are left identical.
# This means that their connections start at exactly the same time, but the 
# inter burst times are relative, and will generally not happen at exactly 
# the same times for the standard and test Tmix sources.
set tmix_Std_cv_name $tmix_shuff_cv_name
set tmix_cv_name $tmix_shuff_cv_name
############################################################################

set error_ $shufbalancetol
set scale_incr 0.0
set Qstatsampleinterval 0.1
set Incremental_display_interval [expr $test_time/100.0]
set finishplus 0.001


#################### set up simulation #####################################
set ns [new Simulator]
remove-all-packet-headers       ; # removes all except common
add-packet-header Flags IP TCP  ; # headers required by TCP

if {$SaveTraceFile > 0} {
    file mkdir $tmp_directory_/trace_data
    set tracefd_ [open $tmp_directory_/trace_data/$result_filename.tr w]
    $ns trace-all $tracefd_
}

########## Initialise per iteration variables ######################
set display_counter 1
foreach fl { total TestTCP StdTCP } {
    foreach tp { pkt byte } {
	foreach act { arr dep drop } {
	    foreach dir { F R } {
		set prevstat($fl,$tp,$act,$dir) 0
	    }
	}
    }
}
foreach dir { F R } {
    set pcntBdep($dir) [list]
    set pcntLoad($dir) [list]
    set pcntPloss($dir) [list]
    set avBQsize($dir) [list]
    set BQsum($dir) [list]
    lappend BQsum($dir) 0.0
}

#####################################################################
set TIME [expr $prefill_t+$warmup]; # only collect results after warmup+prefill_t

puts $nsoutfd_ "Scale = $scale"
flush $nsoutfd_
set traffic [new Create_traffic]
set topo [new Create_topology/Dumb_bell/Basic]

############## Set up Background tmix traffic ##############################
if { $tmix_agent_type == "one-way" } {
    $traffic config_tmix -num_tmix_flow $num_tmix_flow \
	-num_tmix_node $num_tmix_node \
	-tmix_cv_name $tmix_cv_name \
	-tmix_Std_cv_name $tmix_Std_cv_name \
	-tmix_agent_type $tmix_agent_type \
	-tmix_pkt_size $tmix_pkt_size \
	-test_tcp $Test_TCP \
	-standard_tcp $Standard_TCP \
	-useAQM $useAQM \
	-tmix_debug_output $tmix_debug_output
} else {
    $traffic config_tmix -num_tmix_flow $num_tmix_flow \
	-num_tmix_node $num_tmix_node \
	-tmix_cv_name $tmix_cv_name \
	-tmix_Std_cv_name $tmix_Std_cv_name \
	-tmix_agent_type $tmix_agent_type \
	-tmix_debug_output $tmix_debug_output \
	-test_tcp $Test_TCP \
	-standard_tcp $Standard_TCP \
	-useAQM $useAQM \
	-tmix_pkt_size $tmix_pkt_size
    }

############ Configure traffic topology #########################
# scale is now passed as a list for each tmix source
if {[llength $scale] == 1} {
    set scalelist [list]
    for {set i 0 } {$i < $num_tmix_flow} {incr i} {
	lappend scalelist $scale
    }
}
if {[llength $prefill_t] == 1} {
    set prefill_t_list [list]
    for {set i 0 } {$i < $num_tmix_flow} {incr i} {
	lappend prefill_t_list $prefill_t
    }
}
if {[llength $prefill_si] == 1} {
    set prefill_si_list [list]
    for {set i 0 } {$i < $num_tmix_flow} {incr i} {
	lappend prefill_si_list $prefill_si
    }
}
$topo config -cntlnk_bw $cntlnk_bw \
    -num_cntlnk 1 \
    -rttp $core_delay \
    -rtt_diff 0 \
    -edge_delay $edge_delay \
    -edge_bw $edge_bw \
    -core_delay $core_delay \
    -buffer_length $buffer_length \
    -traffic $traffic \
    -sim_time $sim_time \
    -scale $scalelist \
    -end $sim_time \
    -prefill_t $prefill_t_list \
    -prefill_si $prefill_si_list \
    -if_wireless $if_wireless \
    -compare_std 1 \
    -nsoutfd $nsoutfd_

$topo create

array set cntlnk [ $topo array get cntlnk_ ]
#tmix_L left-hand side nodes, tmix_R right-hand side nodes
array set tmix_L [ $topo array get tmix_L ]
array set tmix_R [ $topo array get tmix_R ]
######### Set up Flow monitors and classifiers ##########################
# note that the Dest hash classifier doesn't work properly, so using SrcDest
set fmonF [ $ns makeflowmon SrcDest ]
set fmonR [ $ns makeflowmon SrcDest ]
$ns attach-fmon [ $ns link $cntlnk(0) $cntlnk(1) ] $fmonF
$ns attach-fmon [ $ns link $cntlnk(1) $cntlnk(0) ] $fmonR
######### Aggregate Monitors for central link ###############################
# the sample interval should be changed if a trace file is being output
set Qstatsampleinterval [expr 2*$sim_time]
$fmonF set sampleInterval_ $Qstatsampleinterval
$fmonR set sampleInterval_ $Qstatsampleinterval
# makeflowmon does not set up the bytes integrator, so do it here
set FbytesInt [new Integrator]
$fmonF set-bytes-integrator $FbytesInt
set fmonFsize [$fmonF get-bytes-integrator]
set delaysamplesF [new Samples]
$fmonF set-delay-samples $delaysamplesF
set fmonFdelay [$fmonF get-delay-samples]
#
set RbytesInt [new Integrator]
$fmonR set-bytes-integrator $RbytesInt
set fmonRsize [$fmonR get-bytes-integrator]
set delaysamplesR [new Samples]
$fmonR set-delay-samples $delaysamplesR
set fmonRdelay [$fmonR get-delay-samples]
#
#########################################################################
########### Start Simulation ############################################
#########################################################################
$ns at [expr $TIME] "resetmonitors $fmonF $fmonR"
$ns at  [expr $TIME + $display_counter * $Incremental_display_interval] "monitorcntlnkflows $fmonF $fmonR" 
$ns at [expr $sim_time + $finishplus] "finish $fmonF $fmonR"
$ns run

################################################################## 
############ Simulation Finished #################################
if { $findtarget } {
    #note departure rate at the queue is a good estimate of goodput
    puts $nsoutfd_ [format "Av Mbps %f Scale %f" \
			[expr $prevstat(total,byte,dep,$TargetDirection)*8.0/$measuretime/1.0e6] $scale]

    #remove old trace files while we are trying to find a targetload
    foreach cvf $tmix_cv_name {
	file delete $cvf
    }
    if { $warmup < 1.0 } {
	puts $nsoutfd_ "*********Target has not been properly found *********"
	puts stderr "*********Target has not been properly found *********"
    }
    set scale -1.0
    incr ExperimentIteration
} else {
    set scale -1.0
    incr ExperimentIteration
}

if {$ExperimentIteration > $NumExperiments} {
    set NewExperimentIteration -1 
} else {
    set NewExperimentIteration $ExperimentIteration
}

# Some ns code prints debug messages to stdout,
# so to identify this message it is prefixed with tcpevaliterations.
# some ns code prints debug messages to stdout, so we need to be able to identify this message
puts stdout [format "tcpevaliterations,%d,%f" \
		 $NewExperimentIteration $scale]
close $nsoutfd_
exit 0
