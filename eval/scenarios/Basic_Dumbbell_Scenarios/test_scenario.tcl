#
# Copyright (c) 2011-2012
#  Swinburne University of Technology, Melbourne, Australia
#  All rights reserved.
# Copyright (c) 2013
#  University of Oslo, Norway
#  All rights reserved.
#
# Released under the GNU General Public License version 2.
#
# Author
#  - David Hayes (davihay@ifi.uio.no david.hayes@ieee.org)
#
# Note that inital work on this suite was done by:
# - Gang Wang (wanggang@research.nec.com.cn)
# - Yong Xia   (xiayong@research.nec.com.cn)
#
#
# (see http://tools.ietf.org/html/draft-irtf-tmrg-tests-03>)
#
#            Node_1                                      Node_4 
#                 \                                    / 
#                  \            central link          / 
#          Node_2 --- Router_1 -------------- Router_2 --- Node_5 
#                  /                                  \ 
#                 /                                    \ 
#            Node_3                                      Node_6 
#    
#                       Dumb-bell topology
#

# environment setting
# [include external source code]
source $env(TCPEVAL)/tcl/create_topology.tcl
source $env(TCPEVAL)/tcl/create_traffic.tcl
source $env(TCPEVAL)/tcl/shuffle_traces.tcl
set if_wireless 0               ;# default to not wireless
set w_basicrate 0
set w_datarate 0
#
# defaults for this scenario
#
### Tmix
set num_tmix_flow 9		;# number of tmix flows
set num_tmix_node 3		;# number of tmix nodes
set tmix_pkt_size 1460          ;# default packet size. overriden by m record in cv files
#
#
# debug output files
set tmix_debug_output [list "flow1" "flow2" "flow3" "flow4" "flow5" "flow6" "flow7" "flow8" "flow9"]
#
# new Simulator
set ns [new Simulator]
remove-all-packet-headers       ; # removes all except common
add-packet-header Flags IP TCP  ; # headers required by TCP

# shuffle tolerances here so that specific scenario can override them
set shufbalancetol 0.05
set shufloadtol 0.02
# include parameter files
source ./tcl_base_setup
source ./defaults.tcl


proc debugger {} {
    global ns
    set t [$ns now]
    puts stderr ">>> d $t"
    $ns at [ expr $t + 5.0] "debugger"
 }
# finish process
proc finish {} {
    global ns topo traffic tracefd_ TIME SaveTraceFile
    global findtarget prevstat scale prefill_t prefill_si targetload TargetDirection 
    global pcntLoad  pcntBdep pcntPLoss measuretime test_time
    global QmonFsize QmonFdelay QmonRsize QmonRdelay avBQsize
    global result_filename nsoutfd_ tmp_directory_ error_ if_wireless
    global warmup Incremental_display_interval finishplus longflowthresh
    if {$findtarget } { 
	#Split thoughput in half and stop when two halves are
	#statistically similar after the warmup time.
	if {$TargetDirection == "F"} {
	    set numsamples [llength $pcntBdep(F)]
	} else {
	    set numsamples [llength $pcntBdep(R)]
	}
	if {$longflowthresh > 0} {
	    set minwarmup [expr int(ceil(1.0*$longflowthresh/$Incremental_display_interval))]
	} else {
	    set minwarmup 1
	}
	#search for warmup through first two thirds of test time
	set twothirds [expr 0.67*$numsamples]
	set measuretime [expr [$ns now] - $prefill_t - $finishplus]
	for { set wu $minwarmup; set rest [expr $numsamples - $wu ]} { $wu <= $twothirds } { incr wu; incr rest -1 } {
	    set mid [expr $wu + round($rest / 2.0) ]
	    if { $if_wireless < 1 } {
		set qs_l  [lrange $avBQsize($TargetDirection) $wu [expr $mid - 1 ] ]
		set qs_u  [lrange $avBQsize($TargetDirection) $mid end ]
		set avq_l [expr ([ join $qs_l +]+0.0) / [llength $qs_l]]	  
		set avq_u [expr ([ join $qs_u +]+0.0) / [llength $qs_u]]
	    }
	    if {$TargetDirection == "B"} {
		set pbd_l [lrange $pcntBdep(F) $wu [expr $mid - 1 ] ]
		set pbd_l [concat $pbd_l [lrange $pcntBdep(R) $wu [expr $mid - 1 ] ] ]
		set pbd_u [lrange $pcntBdep(F) $mid end ]
		set pbd_u [concat $pbd_u [lrange $pcntBdep(R) $mid end ] ]
		set avd_l [expr ([ join $pbd_l +]+0.0) / (0.5 * [llength $pbd_l])]	  
		set avd_u [expr ([ join $pbd_u +]+0.0) / (0.5 * [llength $pbd_u])]
	    } else {
		set pbd_l [lrange $pcntBdep($TargetDirection) $wu [expr $mid - 1 ] ]
		set pbd_u [lrange $pcntBdep($TargetDirection) $mid end ]
	    }
	    set avd_l [expr ([ join $pbd_l +]+0.0) / [llength $pbd_l]]	  
	    set avd_u [expr ([ join $pbd_u +]+0.0) / [llength $pbd_u]]
	    if { $if_wireless < 1 } {
		if {$avq_l <= 1  || $avq_u <= 1} {
		    set avq_tol  [expr abs($avq_l - $avq_u)]
		} else {
		    set avq_tol  [expr abs($avq_l - $avq_u) / $avq_l]
		}
		puts stderr "---finding warmup--- wu=$wu, mid=$mid, avd_l=$avd_l, avd_u=$avd_u, avq_l=$avq_l, avq_u=$avq_u"
	    } else {
		# since avg q is not used for wireless
		set avq_tol 0.0
		puts stderr "---finding warmup--- wu=$wu, mid=$mid, avd_l=$avd_l, avd_u=$avd_u"
	    }		
	    if { [expr abs($avd_l - $avd_u) / $avd_l] <= $error_ } {
		# When load is high, the queue lengh is not a stable measure.
		if {($targetload > 70.0 && $targetload < 105)|| $avq_tol <= (4.0*$error_)} {
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
    if { [$ns get-ns-traceall] != "" && $SaveTraceFile >0} {
	$ns flush-trace
	close $tracefd_
    }

    set avlossF [expr 100.0 * $prevstat(pkt,drop,F) / $prevstat(pkt,arr,F) ]
    set avlossR [expr 100.0 * $prevstat(pkt,drop,R) / $prevstat(pkt,arr,R) ]

    set numFsamples [$QmonFdelay cnt]
    set numRsamples [$QmonRdelay cnt]
    if { $numFsamples > 0 && $numRsamples > 0} {
	set avQsizeBF [expr [$QmonFsize set sum_]/ $measuretime]
	set avQsizeBR [expr [$QmonRsize set sum_]/ $measuretime]
	set avQwaitF [$QmonFdelay mean]
	set avQwaitR [$QmonRdelay mean]
    } else {
	set avQsizeBF inf
	set avQsizeBR inf
	set avQwaitF  inf
	set avQwaitR  inf
    }

    if {$TargetDirection == "B"} {
	set pla [lrange $pcntLoad(F) [expr round($warmup/$Incremental_display_interval)] end ]
 	set pla [concat $pla [lrange $pcntLoad(R) [expr round($warmup/$Incremental_display_interval)] end ] ]
	set pcntload [expr ([ join $pla +]+0.0) / (0.5 * [llength $pla])]
    } else {
	set pla [lrange $pcntLoad($TargetDirection) [expr round($warmup/$Incremental_display_interval)] end ]
	set pcntload [expr ([ join $pla +]+0.0) / [llength $pla]]
    }
 
    if {$findtarget} {
	puts $nsoutfd_ [format "Summary (F/R): Arr Mbps %6.3g / %6.3g, Packets - Total %d / %d Dropped %d / %d, Av Q Dly %6.3g s / %6.3g s, Av Q Sz %6.3g B / %6.3g B, Av Loss %6.3f %% / %6.3f %%, Target=%6.3f %%, MeasuredLoad=%6.3f %%, scale=%8.6f prefill_t=%8.6f prefill_si=%8.6f warmup=%8.6f" \
			    [expr $prevstat(byte,arr,F)*8.0/1e6/$measuretime] \
			    [expr $prevstat(byte,arr,R)*8.0/1e6/$measuretime] \
			    $prevstat(pkt,arr,F) $prevstat(pkt,arr,R) \
			    $prevstat(pkt,drop,F) $prevstat(pkt,drop,R) \
			    $avQwaitF $avQwaitR $avQsizeBF $avQsizeBR \
			    $avlossF $avlossR $targetload $pcntload $scale $prefill_t $prefill_si $warmup]
    } else {
	puts $nsoutfd_ [format "Summary (F/R): Arr Mbps %6.3g / %6.3g, Packets - Total %d / %d Dropped %d / %d, Av Q Dly %6.3g s / %6.3g s, Av Q Sz %6.3g B / %6.3g B, Av Loss %6.3f %% / %6.3f %%, scale=%8.6f" \
			    [expr $prevstat(byte,arr,F)*8.0/1e6/$measuretime] \
			    [expr $prevstat(byte,arr,R)*8.0/1e6/$measuretime] \
			    $prevstat(pkt,arr,F) $prevstat(pkt,arr,R) \
			    $prevstat(pkt,drop,F) $prevstat(pkt,drop,R) \
			    $avQwaitF $avQwaitR $avQsizeBF $avQsizeBR \
			    $avlossF $avlossR $scale]

    }
    set resultfd_ [open $tmp_directory_/result_$result_filename w]
    puts $resultfd_ [format "Overall Throughput-OT (bps)\tTotal Packets-TP\tDropped Packets-DP\tAverage Queueing Delay-AQD (s)\tAverage Queue Size-AQS (B)\tAverage Packet Loss-APL (%%)\tTmix scale-Scale"]
    puts $resultfd_ [format "OT L->R, OT R->L, TP L->R, TP R->L, DP L->R, DP R->L, AQD L->R, AQD R->L, AQS L->R, AQS R->L APL L->R, APL R->L, Scale"]
    puts $resultfd_ [format "%8.4f, %8.4f, %d, %d, %d, %d, %6.3g, %6.3g, %6.3g, %6.3g, %6.3f, %6.3f, %8.6f" \
			 [expr $prevstat(byte,dep,F)*8.0/$measuretime] \
			 [expr $prevstat(byte,dep,R)*8.0/$measuretime] \
			 $prevstat(pkt,arr,F) $prevstat(pkt,arr,R) \
			 $prevstat(pkt,drop,F) $prevstat(pkt,drop,R) \
			 $avQwaitF $avQwaitR $avQsizeBF $avQsizeBR \
			 $avlossF $avlossR $scale]
    close $resultfd_

    flush $nsoutfd_

    $topo finish
    $traffic finish
	
    $ns halt
}

# queue statistics

proc monitorcntlnkqueues {QmonF QmonR} {
    global ns Incremental_display_interval display_counter topo traffic TIME
    global prevstat cntlnk_bw
    global scale findtarget targetload TargetDirection nsoutfd_ test_time
    global pcntBdep pcntLoad pcntPloss avBQsize BQsum
    global error_ warmup prefill_t errorMX_

    set measuretime [expr [$ns now] - $TIME]
    foreach tp { pkt byte }  pre { p b } {
	foreach act { arr dep drop } var { arrivals_ departures_ drops_ } {
	    set currstat($tp,$act,F) [ $QmonF set $pre$var ]
	    set currstat($tp,$act,R) [ $QmonR set $pre$var ]
	    set incrstat($tp,$act,F) \
		[expr $currstat($tp,$act,F) - $prevstat($tp,$act,F)]
	    set incrstat($tp,$act,R) \
		[expr $currstat($tp,$act,R) - $prevstat($tp,$act,R)]
	}
    }

    
    foreach dir { F R } dn { 0 1 } {
	if { $incrstat(pkt,arr,$dir) > 0 } {
	    set pcntPdrop($dir) \
		[expr 100.0 * $incrstat(pkt,drop,$dir) / $incrstat(pkt,arr,$dir)]
	} else {
	    set pcntPdrop($dir) 0
	}
	set bbw [lindex $cntlnk_bw $dn]
	if { $incrstat(byte,dep,$dir) > 0 } {
	    set pcntBdepart($dir) \
		[expr 100.0*$incrstat(byte,dep,$dir)*8.0/$Incremental_display_interval/$bbw/1e6]
	} else {
	    set pcntBdepart($dir) 0
	}
	if { $incrstat(byte,arr,$dir) > 0 } {
	    set pcntLd($dir) \
		[expr 100.0 * $incrstat(byte,arr,$dir)*8.0/$Incremental_display_interval/$bbw/1e6]
	} else {
		    set pcntLd($dir) 0
	}
	lappend pcntBdep($dir) $pcntBdepart($dir)
	lappend pcntLoad($dir) $pcntLd($dir)
	lappend pcntPloss($dir) $pcntPdrop($dir)
    }
    set QmonFsize [$QmonF get-bytes-integrator]
    set QmonRsize [$QmonR get-bytes-integrator]
    lappend avBQsize(F) [expr ([$QmonFsize set sum_] - [lindex $BQsum(F) end])/ $Incremental_display_interval]
    lappend avBQsize(R) [expr ([$QmonRsize set sum_] - [lindex $BQsum(R) end])/ $Incremental_display_interval]
    lappend BQsum(F) [$QmonFsize set sum_]
    lappend BQsum(R) [$QmonRsize set sum_]

    puts $nsoutfd_ [format "Time %4.1f Incr. Stats (F/R): Data (Mb) Arr %6.3g / %6.3g, Dep %6.3g / %6.3g, Drop %5.2f %% / %5.2f %% Load %5.2f %% / %5.2f %% AvQ B %5.2g / %5.2g " \
			[expr $TIME+$display_counter*$Incremental_display_interval] \
			[expr $incrstat(byte,arr,F) *8.0/1e6/$Incremental_display_interval] \
			[expr $incrstat(byte,arr,R) *8.0/1e6/$Incremental_display_interval] \
			[expr $incrstat(byte,dep,F) *8.0/1e6/$Incremental_display_interval] \
			[expr $incrstat(byte,dep,R) *8.0/1e6/$Incremental_display_interval] \
			$pcntPdrop(F) $pcntPdrop(R) $pcntLd(F) $pcntLd(R) \
 			[lindex $avBQsize(F) end] [lindex $avBQsize(R) end] ] 
    flush $nsoutfd_

    ################# save currstats as prevstats ###########################
    foreach tp { pkt byte } {
	foreach act { arr dep drop } {
	    foreach dir { F R } {
		set prevstat($tp,$act,$dir) $currstat($tp,$act,$dir)
	    }
	}
    }

    set display_counter [ expr $display_counter + 1 ]
    $ns at [ expr $TIME+$display_counter * $Incremental_display_interval] "monitorcntlnkqueues $QmonF $QmonR"
}


# reset Queue monitors
proc resetmonitors {QmonF QmonR} {
    $QmonF reset
    $QmonR reset
}
# reinitialise Integrators in the queue monitors
# used in warmup calculations
proc reinitIntegrators {QmonF QmonR} {
    set QmonFsize [$QmonF get-bytes-integrator]
    set QmonRsize [$QmonR get-bytes-integrator]
    $QmonFsize reset
    $QmonRsize reset
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Simulation setup
#
set NumExperiments [array size Scale]
set skipexperiment 0
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
# heuristic based on BDP:
if {[info exists Prefill_t($ThisExperiment)]} {
    set prefill_t $Prefill_t($ThisExperiment)
} else {
    set prefill_t [expr 1.5 * $targetload * $maxRTT ]
}
if {[info exists Warmup($ThisExperiment)]} {
    set warmup $Warmup($ThisExperiment)
}
set maxtrace [expr 3000.0 * $scale]
if { [info exists warmup] } {
    set sim_time [expr $test_time + $warmup + $prefill_t]
} else {
    set warmup 0.0
    set test_time [expr max($test_time,[expr 2.5*$prefill_t])]
    set sim_time [expr $test_time + $warmup + $prefill_t]
}
if { $sim_time > $maxtrace } {
    set sim_time $maxtrace
} 

set cc_tmix_srcs [llength $tmix_base_cv_name]
set longflowthresh [expr 0.5*$prefill_t ]
set pkt_overhead 40
set findstats 0
set shuff [new Shuffle]
set shufrtns [$shuff shuffle_traces $scale $sim_time $binsecs $tmix_base_cv_name \
		  $findstats $findtarget $prefill_t $BCbps $cc_tmix_srcs $maxRTT \
		  $targetload $TargetDirection $longflowthresh \
		  $tmix_pkt_size  $pkt_overhead \
		 $shufbalancetol $shufloadtol]
set tmix_cv_name [lindex $shufrtns 0]
set tracestats [lindex $shufrtns 1]
set scale [lindex $shufrtns 2]
set sim_time [lindex $shufrtns 3]

puts stderr "stats: $tracestats, scale: $scale, sim_time: $sim_time"

if {$findstats || $findtarget} {
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
	# heuristic requires adjustment for longer RTTs
	if {[info exists Prefill_si_scale($ThisExperiment)]} {
	    set prefill_si [expr $prefill_si * $Prefill_si_scale($ThisExperiment)]
	}
	set nsbasefd_ [open $tmp_directory_/tcl_base_setup a]
	puts $nsbasefd_ "set Prefill_si($ThisExperiment) $prefill_si"
	close $nsbasefd_
    }
    puts stderr "prefill data = $total_prefill_in_bits, prefill_si = $prefill_si"
    if {$prefill_t < $prefill_si} {
	puts stderr "Warning: prefill_t must be larger than prefill_si, setting prefill_t and prefill_si to 0, as prefill is not needed for this experiment"
	set prefill_si 0
	set prefill_t 0
    }
}
puts stderr "---> $ThisExperiment , Prefill=$prefill_t, maxRTT = $maxRTT"
#for Delay_Throughput_tradeoff
if { [array exists BufferLength] } {
    set buffer_length $BufferLength($ThisExperiment)
}

if { [array exists AQMtarget] } {
    set aqm_target $AQMtarget($ThisExperiment)
} else {
    set aqm_target 0
}

if {$findtarget} {
    set nsoutfd_ [open $tmp_directory_/loadtarget_$result_filename a]
} else {
    set nsoutfd_ [open $tmp_directory_/nsout_$result_filename a]
}
set error_ $shufbalancetol
set scale_incr 0.0
set Incremental_display_interval [expr $test_time/100.0]
set finishplus 0.001

if {$SaveTraceFile > 0} {
    file mkdir $tmp_directory_/trace_data
    set tracefd_ [open $tmp_directory_/trace_data/$result_filename.tr w]
    $ns trace-all $tracefd_
}
########## Initialise per iteration variables ####################
set display_counter 1
########## Initialise per iteration variables ######################
foreach tp { pkt byte } {
    foreach act { arr dep drop } {
	foreach dir { F R } {
	    set prevstat($tp,$act,$dir) 0
	}
    }
}
foreach dir { F R } {
    set pcntBdep($dir) [list]
    set pcntLoad($dir) [list]
    set pcntPloss($dir) [list]
    set avBQsize($dir) [list]
    set BQsum($dir) [list]
    lappend BQsum($dir) 0
}


set TIME [expr $prefill_t+$warmup]; # only collect results after warmup+prefill_t

puts $nsoutfd_ "Scale = $scale"
flush $nsoutfd_
set traffic [new Create_traffic]
set topo [new Create_topology/Dumb_bell/Basic]

############## Set up tmix traffic ###############################
if { $tmix_agent_type == "one-way" } {
    $traffic config_tmix -num_tmix_flow $num_tmix_flow \
	-num_tmix_node $num_tmix_node \
	-tmix_cv_name $tmix_cv_name \
	-tmix_agent_type $tmix_agent_type \
	-tmix_pkt_size $tmix_pkt_size \
	-test_tcp $Test_TCP \
	-useAQM $useAQM \
	-AQMtarget $aqm_target \
	-tmix_debug_output $tmix_debug_output
} else {
    $traffic config_tmix -num_tmix_flow $num_tmix_flow \
	-num_tmix_node $num_tmix_node \
	-tmix_cv_name $tmix_cv_name \
	-tmix_agent_type $tmix_agent_type \
	-tmix_debug_output $tmix_debug_output \
	-test_tcp $Test_TCP \
	-useAQM $useAQM \
	-AQMtarget $aqm_target \
	-tmix_pkt_size $tmix_pkt_size
    }

############ Configure network topology ##########################
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
    -wireless_datarate ${w_datarate}e6 \
    -nsoutfd $nsoutfd_

$topo create

array set cntlnk [$topo array get cntlnk_]

######### Monitors for central link ###########################
# the sample interval should be changed if a trace file is being output
set Qstatsampleinterval [expr 2*$sim_time]
set QmonF [$ns monitor-queue $cntlnk(0) $cntlnk(1) stdout $Qstatsampleinterval]
set QmonFsize [$QmonF get-bytes-integrator]
set delaysamplesF [new Samples]
$QmonF set-delay-samples $delaysamplesF
set QmonFdelay [$QmonF get-delay-samples]
set QmonR [$ns monitor-queue $cntlnk(1) $cntlnk(0) stdout $Qstatsampleinterval]
set QmonRsize [$QmonR get-bytes-integrator]
set delaysamplesR [new Samples]
$QmonR set-delay-samples $delaysamplesR
set QmonRdelay [$QmonR get-delay-samples]
if { $if_wireless >= 1 } {
    # Note that most of the wireless monitoring code is in the wireless
    # tcl_base_setup
    $ns at [expr $TIME] "resetWmons cmon wmon"
    $ns at [ expr $TIME+$display_counter * $Incremental_display_interval ] \
	"monitorWirelessflows cmon wmon"
} else {
    $ns at [expr $TIME] "reinitIntegrators $QmonF $QmonR"
    $ns at  [expr $TIME + $display_counter * $Incremental_display_interval] \
	"monitorcntlnkqueues $QmonF $QmonR"  
}
$ns at [expr $TIME] "resetmonitors $QmonF $QmonR"  
#$ns at [expr 1.0] "debugger"  
#################################################################
########### Start Simulation #####################################
$ns at [expr $sim_time + $finishplus] "finish"

$ns run

if { $findtarget } {
    #note departure rate at the queue is a good estimate of goodup
    if {$TargetDirection == "B"} {
	puts $nsoutfd_ [format "Av Mbps %f / %f Scale %f" \
			    [expr $prevstat(byte,dep,F)*8.0/$measuretime/1.0e6] \
			    [expr $prevstat(byte,dep,R)*8.0/$measuretime/1.0e6] $scale]
    } else {
	puts $nsoutfd_ [format "Av Mbps %f Scale %f" \
			    [expr $prevstat(byte,dep,$TargetDirection)*8.0/$measuretime/1.0e6] $scale]
    }

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

# some ns code prints debug messages to stdout, so we need to be able to identify this message
puts stdout [format "tcpevaliterations,%d,%f" \
		 $NewExperimentIteration $scale]
close $nsoutfd_
exit 0



