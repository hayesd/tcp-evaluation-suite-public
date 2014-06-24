
# Copyright (c) 2011
#  Swinburne University of Technology, Melbourne, Australia
#  All rights reserved.
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
# Note that inital work on this suite was done by:
# - Gang Wang (wanggang@research.nec.com.cn)
# - Yong Xia   (xiayong@research.nec.com.cn)
#
#
#
#             Node_1       Node_3     Node_5       Node_7 
#                 \            |          |           / 
#                  \      A    |    B     |   C      /
#                Router1 ---Router2---Router3--- Router4  
#                  /           |          |          \ 
#                 /            |          |           \ 
#             Node_2       Node_4     Node_6       Node_8 
#    
# Node_1 <--> Node_8    
# Node_2 <--> Node_3
# Node_4 <--> Node_5
# Node_6 <--> Node_7
#
#                     Fig 1. Multiple-bottleneck Scenario topology. 
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
set tmix_pkt_size 1460          ;# default packet size. overriden by m record in cv files
#
# new Simulator
set ns [new Simulator]
remove-all-packet-headers       ; # removes all except common
add-packet-header Flags IP TCP  ; # headers required by TCP

set TargetIter -1
#
# shuffle tolerances here so that specific scenario can override them
set shufbalancetol 0.05
set shufloadtol 0.02
# include parameter files
source ./tcl_base_setup
source ./defaults.tcl

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
	exit
    }
}

puts stderr "---> $ThisExperiment"
# simulation trace results 
if {$findtarget} {
    set nsoutfd_ [open $tmp_directory_/loadtarget_$result_filename a]
} else {
    set nsoutfd_ [open $tmp_directory_/nsout_$result_filename a]
}

proc avarray {arr} {
    upvar $arr a
    set sum 0
    foreach e [array names a] {
	set sum [expr $sum + $a($e)]
    }
    set av [expr $sum*1.0/[array size a]]
    return $av
}

# finish process
proc finish {} {
    global ns topo traffic tracefd_ TIME SaveTraceFile
    global findtarget prevstat scale prefill_t prefill_si targetload TargetDirection 
    global pcntLoad  pcntBdep pcntPLoss measuretime test_time
    global fmonsize fmondelay Qstatsampleinterval avBQsize
    global warmup finishplus longflowthresh Incremental_display_interval
    global result_filename nsoutfd_ num_cntlnk error_
    global tmp_directory_ tracefd_ 

    if { [$ns get-ns-traceall] != ""  && $SaveTraceFile > 0 } {
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

    set resultfd_ [open $tmp_directory_/result_$result_filename w]
    foreach mon { Q t1 t2 t3 t4 } {
	foreach cl { A B C } {
	    if { $mon == "Q" } {
		foreach dir { F R } {
		    set avloss($dir) [expr 100.0 * $prevstat($mon,$cl,pkt,drop,$dir) / $prevstat($mon,$cl,pkt,arr,$dir) ]
		    set nQdlysamp [$fmondelay($cl,$dir) cnt]
		    if { $nQdlysamp > 0 } {
			set avQsizeB($dir) [expr [$fmonsize($cl,$dir) set sum_]/ $measuretime]
			set avQwait($dir) [$fmondelay($cl,$dir) mean]
		    } else {
			set avQsizeB($dir) inf
			set avQwait($dir)  inf
		    }
		}
		if {$findtarget} {
		    puts $nsoutfd_ [format "Summary %s (F/R): Packets - Total %d / %d Dropped %d / %d, Av Q Dly %6.3g s / %6.3g s, Av Q Sz %6.3g B / %6.3g B, Av Loss %6.3f %% / %6.3f %%, Target %6.3f %%, scale %8.6f" \
					$cl \
					$prevstat($mon,$cl,pkt,arr,F) $prevstat($mon,$cl,pkt,arr,R) \
					$prevstat($mon,$cl,pkt,drop,F) $prevstat($mon,$cl,pkt,drop,R) \
					$avQwait(F) $avQwait(R) \
					$avQsizeB(F) $avQsizeB(R) \
					$avloss(F) $avloss(R) \
					$targetload $scale]
		} else {
		    puts $nsoutfd_ [format "Summary %s (F/R): Packets - Total %d / %d Dropped %d / %d, Av Q Dly %6.3g s / %6.3g s, Av Q Sz %6.3g B / %6.3g B, Av Loss %6.3f %% / %6.3f %%, scale %8.6f" \
					$cl \
					$prevstat($mon,$cl,pkt,arr,F) $prevstat($mon,$cl,pkt,arr,R) \
					$prevstat($mon,$cl,pkt,drop,F) $prevstat($mon,$cl,pkt,drop,R) \
					$avQwait(F) $avQwait(R) \
					$avQsizeB(F) $avQsizeB(R) \
					$avloss(F) $avloss(R) \
					$scale]
		}
		if { $cl == "A" } {
		    puts $resultfd_ [format "Overall Throughput-OT (bps)\tTotal Packets-TP\tDropped Packets-DP\tAverage Queueing Delay-AQD (s)\tAverage Queue Size-AQS (B)\tAverage Packet Loss-APL (%%)\tTmix scale-Scale"]
		    puts $resultfd_ [format "Link OT L->R, OT R->L, TP L->R, TP R->L, DP L->R, DP R->L, AQD L->R, AQD R->L, AQS L->R, AQS R->L APL L->R, APL R->L, Scale"]
		}
		puts $resultfd_ [format "%s, %s, %8.4f, %8.4f, %d, %d, %d, %d, %6.3g, %6.3g, %6.3g, %6.3g, %6.3f, %6.3f, %8.6f" \
				     $mon $cl \
				     [expr $prevstat($mon,$cl,byte,dep,F)*8.0/$measuretime] \
				     [expr $prevstat($mon,$cl,byte,dep,R)*8.0/$measuretime] \
				     $prevstat($mon,$cl,pkt,arr,F) $prevstat($mon,$cl,pkt,arr,R) \
				     $prevstat($mon,$cl,pkt,drop,F) $prevstat($mon,$cl,pkt,drop,R) \
				     $avQwait(F) $avQwait(R) \
				     $avQsizeB(F) $avQsizeB(R) \
				     $avloss(F) $avloss(R) \
				     $scale]
	    } else {
		puts $resultfd_ [format "%s, %s, %8.4f, %8.4f, %d, %d, %d, %d" \
				     $mon $cl \
				     [expr $prevstat($mon,$cl,byte,dep,F)*8.0/$measuretime] \
				     [expr $prevstat($mon,$cl,byte,dep,R)*8.0/$measuretime] \
				     $prevstat($mon,$cl,pkt,arr,F) $prevstat($mon,$cl,pkt,arr,R) \
				     $prevstat($mon,$cl,pkt,drop,F) $prevstat($mon,$cl,pkt,drop,R)]
	    }
	}
    }

    close $resultfd_

    flush $nsoutfd_

    $topo finish
    $traffic finish
	
    $ns halt
}


# queue statistics

proc monitorcntlnkqueues { } {
    global fmon fmonsize fmondelay
    global ns Incremental_display_interval display_counter topo traffic TIME cntlnk1 cntlnk2 
    global prevstat cntlnk_bw num_cntlnk tmix_L tmix_R
    global pcntBdep pcntLoad pcntPloss avBQsize BQsum 
    global scale nsoutfd_ test_time

    foreach cl { A B C } {
	foreach dir { F R } {
	    set fc($cl,$dir) [$fmon($cl,$dir) classifier]
	}
    }

    set measuretime [expr [$ns now] - $TIME]
    array set linklabel { 0 "A" 1 "B" 2 "C" }
    foreach mon { Q t1 t2 t3 t4 } tsrc { 0 0 1 2 3 } {
	foreach cl { A B C } {
	    foreach tp { pkt byte }  pre { p b } {
		foreach act { arr dep drop } var { arrivals_ departures_ drops_ } {
		    if { $mon == "Q" } {
			set currstat($mon,$cl,$tp,$act,F) [ $fmon($cl,F) set $pre$var ]
			set currstat($mon,$cl,$tp,$act,R) [ $fmon($cl,R) set $pre$var ]
			set incrstat($cl,$tp,$act,F) \
			    [expr $currstat($mon,$cl,$tp,$act,F) - $prevstat($mon,$cl,$tp,$act,F)]
			set incrstat($cl,$tp,$act,R) \
			    [expr $currstat($mon,$cl,$tp,$act,R) - $prevstat($mon,$cl,$tp,$act,R)]
		    } else {
			set currstat($mon,$cl,$tp,$act,F) 0
			set currstat($mon,$cl,$tp,$act,R) 0
			foreach tsnk { 0 1 2 3 } {
			    set flF [$fc($cl,F) lookup auto [$tmix_L(T,$tsrc) address?] [$tmix_R(T,$tsnk) address?] 0]
			    set flR [$fc($cl,R) lookup auto [$tmix_R(T,$tsnk) address?] [$tmix_L(T,$tsrc) address?] 0]
			    if { $flF != "" } {
				set currstat($mon,$cl,$tp,$act,F)  \
				    [expr $currstat($mon,$cl,$tp,$act,F) + [ $flF set $pre$var ] ]
			    }
			    if { $flR != "" } {
				set currstat($mon,$cl,$tp,$act,R)  \
				    [expr $currstat($mon,$cl,$tp,$act,R) + [ $flR set $pre$var ] ]
			    }
			}
		    }
		}
	    }
	    if { $mon == "Q" } {
		foreach dir { F R } {
		    if { $incrstat($cl,pkt,arr,$dir) > 0 } {
			set pcntPdrop($dir) \
			    [expr 100.0 * $incrstat($cl,pkt,drop,$dir) / $incrstat($cl,pkt,arr,$dir)]
		    } else {
			set pcntPdrop($dir) 0
		    }
		}
	
		puts $nsoutfd_ [format "Time %4.1f %s Incremental Stats (F/R): Data (Mb) Arr %6.3g / %6.3g, Dep %6.3g / %6.3g, DropRate %5.2f %% / %5.2f %%" \
				    [expr $TIME+$display_counter*$Incremental_display_interval] $cl \
				    [expr $incrstat($cl,byte,arr,F) *8.0/1e6/$Incremental_display_interval] \
				    [expr $incrstat($cl,byte,arr,R) *8.0/1e6/$Incremental_display_interval] \
				    [expr $incrstat($cl,byte,dep,F) *8.0/1e6/$Incremental_display_interval] \
				    [expr $incrstat($cl,byte,dep,R) *8.0/1e6/$Incremental_display_interval] \
				    $pcntPdrop(F) $pcntPdrop(R)]
	    }
	}
    }

    flush $nsoutfd_

   ############# overall stats ###################
    foreach dir { F R } dn { 0 1 } {
	set pcntPdp 0.0
	set pcntBdpt 0.0
	set pcntLd 0.0
	foreach cl { A B C } {
	    if { $incrstat($cl,pkt,arr,$dir) > 0 } {
		set pcntPdp \
		    [expr $pcntPdp + 100.0/$num_cntlnk * $incrstat($cl,pkt,drop,$dir) / $incrstat($cl,pkt,arr,$dir)]
	    } else {
		set pcntPdp [expr $pcntPdp + 0.0]
	    }
	    set bbw [lindex $cntlnk_bw $dn]
	    set pcntBdpt \
		[expr $pcntBdpt + 100.0/$num_cntlnk * $incrstat($cl,byte,dep,$dir)*8.0/$Incremental_display_interval/$bbw/1e6]
	    
	    set pcntLd \
		[expr $pcntLd + 100.0/$num_cntlnk * $incrstat($cl,byte,arr,$dir)*8.0/$Incremental_display_interval/$bbw/1e6]
	}
	lappend pcntBdep($dir) $pcntBdpt
	lappend pcntLoad($dir) $pcntLd
	lappend pcntPloss($dir) $pcntPdp
    }
    
    foreach dir { F R } {
	set avqs 0
	foreach cl { A B C } {
	    set avqs [expr $avqs + ([$fmonsize($cl,$dir) set sum_] - [lindex $BQsum($cl,$dir) end]) / 3.0 / $Incremental_display_interval]
	    lappend BQsum($cl,$dir) [$fmonsize($cl,$dir) set sum_]
	}
	lappend avBQsize($dir) $avqs
    }

    ################# save currstats as prevstats ###########################
    foreach mon { Q t1 t2 t3 t4 } {
	foreach cl { A B C } {
	    foreach tp { pkt byte } {
		foreach act { arr dep drop } {
		    foreach dir { F R } {
			set prevstat($mon,$cl,$tp,$act,$dir) $currstat($mon,$cl,$tp,$act,$dir)
		    }
		}
	    }
	}
    }
	
    set display_counter [ expr $display_counter + 1 ]
    $ns at [ expr $TIME+$display_counter * $Incremental_display_interval] "monitorcntlnkqueues"
}


# reset monitors
proc resetmonitors { } {
    global fmon fmonsize fmondelay
    global tmix_L tmix_R
    foreach cl { A B C } {
	foreach dir { F R } {
	    set fc($cl,$dir) [$fmon($cl,$dir) classifier]
	}
	foreach pre { p b } {
	    foreach var { arrivals_ departures_ drops_ } {
		foreach tsrc { 0 1 2 3 } {
		    foreach tsnk { 0 1 2 3 } {
			set flF [$fc($cl,F) lookup auto [$tmix_L(T,$tsrc) address?] [$tmix_R(T,$tsnk) address?] 0]
			set flR [$fc($cl,R) lookup auto [$tmix_R(T,$tsnk) address?] [$tmix_L(T,$tsrc) address?] 0]
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
    # reset flow monitors
    foreach cl { A B C } {
	foreach dir { F R } {
	    $fmon($cl,$dir) reset
	    $fmonsize($cl,$dir) reset
	    $fmondelay($cl,$dir) reset
	}
    }
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Simulation setup
#
# Using one shuffled source
#
#Create new tmix source files
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
if {[info exists Warmup($ThisExperiment)]} {
    set warmup $Warmup($ThisExperiment)
}
if {[info exists Prefill_t($ThisExperiment)]} {
    set prefill_t $Prefill_t($ThisExperiment)
} else {
    set prefill_t [expr 1.5 * $targetload * $maxRTT ]
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

set cc_tmix_srcs 2
set longflowthresh [expr 0.5*$prefill_t ]
set pkt_overhead 40
set findstats 1
# each bottleneck has two tmix flows, so halve the target load
set bn_target_load [expr  1.0 * $targetload / $cc_tmix_srcs]
set shuff [new Shuffle]
set shufrtns [$shuff shuffle_traces $scale $sim_time $binsecs $tmix_base_cv_names \
		  $findstats $findtarget $prefill_t $BCbps $cc_tmix_srcs $maxRTT \
		  $bn_target_load $TargetDirection $longflowthresh \
		  $tmix_pkt_size  $pkt_overhead \
		 $shufbalancetol $shufloadtol]

# repeat for each tmix source
set tmix_cv_name [list]
for {set tmf 1} {$tmf <= $num_tmix_flow} {incr tmf} {
    lappend tmix_cv_name [lindex $shufrtns 0]
}
set tracestats [lindex $shufrtns 1]
set scale [lindex $shufrtns 2]
set sim_time [lindex $shufrtns 3]
puts stderr "stats: $tracestats, scale: $scale, sim_time: $sim_time"

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

# slice time in microseconds
set error_ $shufbalancetol
set Incremental_display_interval [expr $test_time/100.0]
set finishplus 0.001

if {$SaveTraceFile > 0} {
    file mkdir $tmp_directory_/trace_data
    set tracefd_ [open $tmp_directory_/trace_data/$result_filename.tr w]
    $ns trace-all $tracefd_
}

########## Initialise per iteration variables ####################
set display_counter 1
set TIME [expr $prefill_t+$warmup]; # only collect results after warmup+prefill_t

puts $nsoutfd_ "Scale = $scale"
flush $nsoutfd_
set traffic [new Create_traffic]
set topo [new Create_topology/Dumb_bell/Basic]

############## Set up tmix traffic ###############################
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
if { $tmix_agent_type == "one-way" } {
    $traffic config_tmix -num_tmix_flow $num_tmix_flow \
	-tmix_cv_name $tmix_cv_name \
	-tmix_agent_type $tmix_agent_type \
	-tmix_pkt_size $tmix_pkt_size \
	-test_tcp $Test_TCP \
	-useAQM $useAQM \
	-tmix_debug_output $tmix_debug_output
} else {
    $traffic config_tmix -num_tmix_flow $num_tmix_flow \
	-tmix_cv_name $tmix_cv_name \
	-tmix_agent_type $tmix_agent_type \
	-tmix_debug_output $tmix_debug_output \
	-test_tcp $Test_TCP \
	-useAQM $useAQM \
	-tmix_pkt_size $tmix_pkt_size
    }

############ Configure network topology ##########################
$topo config -cntlnk_bw $cntlnk_bw \
    -num_cntlnk $num_cntlnk \
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
    -nsoutfd $nsoutfd_

$topo create

######### Monitors for central link Routers ###########################
# the sample interval should be changed if a trace file is being output
set Qstatsampleinterval [expr 2*$sim_time]
for { set r 0 } { $r <= $num_cntlnk } { incr r } {
    set Router($r) [$topo set cntlnk_($r)]
}

foreach cl { A B C } rtr { 0 1 2 } {
    foreach dir { F R } {
	foreach mon { Q t1 t2 t3 t4 } {
	    foreach tp { pkt byte } {
		foreach act { arr dep drop } {
		    set prevstat($mon,$cl,$tp,$act,$dir) 0
		}
	    }
	}
    }
}

##################Monitors for Flows###############################
array set tmix_L [ $topo array get tmix_L ]
array set tmix_R [ $topo array get tmix_R ]
# One on each central link
# note that the Dest hash classifier doesn't work properly, so using SrcDest
foreach cl { A B C } rtr { 0 1 2 } {
    set fmon($cl,F) [ $ns makeflowmon SrcDest ]
    set fmon($cl,R) [ $ns makeflowmon SrcDest ]
    $ns attach-fmon [ $ns link $Router($rtr) $Router([expr $rtr + 1]) ] $fmon($cl,F)
    $ns attach-fmon [ $ns link  $Router([expr $rtr + 1]) $Router($rtr)] $fmon($cl,R)
}
# makeflowmon does not set up the bytes integrator, so do it here
foreach dir { F R } {
    foreach cl { A B C } {
	$fmon($cl,$dir) set sampleInterval_ $Qstatsampleinterval
	set bytesInt($cl,$dir) [new Integrator]
	$fmon($cl,$dir) set-bytes-integrator $bytesInt($cl,$dir)
	set fmonsize($cl,$dir) [$fmon($cl,$dir) get-bytes-integrator]
	set delaysamples($cl,$dir) [new Samples]
	$fmon($cl,$dir) set-delay-samples $delaysamples($cl,$dir)
	set fmondelay($cl,$dir) [$fmon($cl,$dir) get-delay-samples]

    }
}
foreach dir { F R } {
    foreach cl { A B C } {
	# initialise BQsum stats
	set BQsum($cl,$dir) [list]
	lappend BQsum($cl,$dir) 0
    }
    set avBQsize($dir) [list]
}

$ns at [expr $TIME] "resetmonitors"

########### Start Simulation #####################################
$ns at  [expr $TIME + $display_counter * $Incremental_display_interval] "monitorcntlnkqueues"  
$ns at [expr $sim_time + $finishplus] "finish"
$ns run

set measuretime [expr [$ns now] - $TIME - $finishplus]


############## Calculation of next scale factor ##################
# This code was used to determine the scale factor that will
# yield the targetload for NewReno.
# It was used to determine the scale parameter outlined in the 
# irtf draft.
array set drops [array get prevstat Q,*,pkt,drop,$TargetDirection ]
set Tdrop [avarray drops ]
array set Parrs [array get prevstat Q,*,pkt,arr,$TargetDirection ]
set TavPloss [expr 100.0 * $Tdrop / [avarray Parrs ] ]
set Tcntlnk_bw $cntlnk_bw
# average forward arrivals as Mbps
array set Barrs [array get prevstat Q,*,byte,arr,$TargetDirection ]
set TavBarr [expr [avarray Barrs ] * 8.0/$measuretime/1.0e6 ]

if { $findtarget } {
    puts $nsoutfd_ [format "Average loss of three links %f" $TavPloss]
    puts $nsoutfd_ [format "Av Mbps %f, Scale %f" \
			$TavBarr $scale]
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
puts stdout [format "tcpevaliterations,%d,%f" \
		 $NewExperimentIteration $scale]
close $nsoutfd_

exit 0



