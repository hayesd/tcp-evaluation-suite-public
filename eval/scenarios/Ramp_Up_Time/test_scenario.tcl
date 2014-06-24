#
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
# (see http://tools.ietf.org/html/draft-irtf-tmrg-tests-03>)
#
#              T_n2                        T_n4
#               |                           |
#               |                           |
#         T_n1  |                           |  T_n3
#            \  |                           | /
#             \ |                           |/
#      B_n1--- R1--------------------------R2--- B_n4
#             / |                           |\
#            /  |                           | \
#        B_n2   |                           |  B_n5
#               |                           |
#              B_n3                        B_n6
#
#               Figure: Dumbbell test topology
# 
# Where Test TCP source 1 is connected T_n1 to T_n3
# and test TCP soiurce 2 is connected T_n2 to T_n3
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
set num_tmix_flow 9		;# number of tmix flows
set num_tmix_node 3		;# number of tmix nodes
set tmix_pkt_size 1460          ;# default packet size. overriden by m record in cv files
set min_test_time 300.0         ;# test must run for at least 300s
#
#
# debug output files
set tmix_debug_output [list "flow1" "flow2" "flow3" "flow4" "flow5" "flow6" "flow7" "flow8" "flow9"]
#
# include parameter files
#
source ./tcl_base_setup
source ./defaults.tcl

proc outputresults { n bytes src} {
    global resultfd_ starttime ns TIME
    set measuretime [expr [$ns now] - $TIME]
    puts $resultfd_ [format "%d,%d, %d, %f" $src $n $bytes $measuretime]
    flush $resultfd_
}
#Receive function for nbytes
Class Application/FTPrecv -superclass Application
Application/FTPrecv instproc init { } {
    $self instvar totalreceived
    $self instvar recvBlkCnt
    $self instvar recvBlkThresh
    $self instvar start_counting
    
    set start_counting 0
    set totalreceived 0
    set recvBlkCnt 1
    set recvBlkThresh [expr 1500*10**$recvBlkCnt]
    $self next
}

Application/FTPrecv instproc recv {nbytes} {
    $self instvar totalreceived
    $self instvar recvBlkThresh
    $self instvar recvBlkCnt
    $self instvar test_source
    $self instvar start_counting

    if { $start_counting } {
	set totalreceived [expr $totalreceived + $nbytes]
	while { $totalreceived >= $recvBlkThresh } {
	    outputresults $recvBlkCnt $totalreceived $test_source
	    incr recvBlkCnt
	    set recvBlkThresh [expr 1500*10**$recvBlkCnt]
	}
    }
}
Application/FTPrecv instproc StartCounting { } {
    $self instvar start_counting
    set start_counting 1
}
Application/FTPrecv instproc label { src } {
    $self instvar test_source
    set test_source $src
}
# finish process
proc finish { fmonF fmonR } {
    global ns tracefd_ TIME SaveTraceFile
    global findtarget prevstat scale prefill_t prefill_si targetload TargetDirection 
    global pcntLoad  pcntBdep pcntPLoss measuretime test_time min_test_time
    global fmonFsize fmonFdelay fmonRsize fmonRdelay avBQsize
    global warmup Incremental_display_interval finishplus longflowthresh
    global fmonFsize fmonRsize fmonFdelay fmonRdelay
    global Btopo Btraffic
    global resultfd_ nsoutfd_ error_

    if { [$ns get-ns-traceall] != "" && $SaveTraceFile > 0} {
	$ns flush-trace
	close $tracefd_
    }

    if {$findtarget } { 
	#Split thoughput in half and stop when two halves are
	#statistically similar after the warmup time.
	set numsamples [llength $pcntBdep($TargetDirection)]
	set minwarmup 2
	#search for warmup through first half or up to testtime
	set maxwarmup [expr int($numsamples*(1.0-($min_test_time/$test_time)))]
        set measuretime [expr [$ns now] - $prefill_t - $finishplus]
	for { set wu $minwarmup; set rest [expr $numsamples - $wu ]} { $wu <= $maxwarmup } { incr wu; incr rest -1 } {
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
	    if { [expr abs($avd_l - $avd_u) / $avd_l] <= 2.0 * $error_ } {
	    	# When load is high, the queue lengh is not a stable measure.
	    	if {($targetload > 70.0 && $targetload < 105.0)|| $avq_tol <= (4.0*$error_)} {
		    set warmup [expr $wu * $Incremental_display_interval]
		    set TIME [expr $warmup + $prefill_t]
		    puts $nsoutfd_ [format "Calculated Times: Warmup = %8.5g, Test time = %8.5g" $warmup [expr $measuretime - $warmup] ]
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
    if { $numFsamples > 0 && $numRsamples > 0} {
	set avQsizeBF [expr [$fmonFsize set sum_]/ $measuretime]
	set avQsizeBR [expr [$fmonRsize set sum_]/ $measuretime]
	set avQwaitF [$fmonFdelay mean]
	set avQwaitR [$fmonRdelay mean]
    } else {
	set avQsizeBF inf
	set avQsizeBR inf
	set avQwaitF  inf
	set avQwaitR  inf
    }
    foreach tf { t1 t2} {
	if { $prevstat($tf,pkt,arr,$TargetDirection) > 0 } {
	    set droprate($tf) [expr 100.0*$prevstat($tf,pkt,drop,$TargetDirection)/$prevstat($tf,pkt,arr,$TargetDirection)]
	} else {
	    set droprate($tf) 0
	}
    }
    puts $nsoutfd_ [format "Traffic Summary : Total F/R(Mbps) %6.4g / %6.4g, Bkg F/R(Mbps) %6.4g / %6.4g, Test flows %s Arr(Mbps)/Drop: T1 %6.4g / %5.2f %%, T2 %6.4g / %5.2f %%" \
			[expr $prevstat(total,byte,arr,F)*8.0/1e6/$measuretime] \
			[expr $prevstat(total,byte,arr,R)*8.0/1e6/$measuretime] \
			[expr $prevstat(background,byte,arr,F)*8.0/1e6/$measuretime] \
			[expr $prevstat(background,byte,arr,R)*8.0/1e6/$measuretime] \
			$TargetDirection \
			[expr $prevstat(t1,byte,arr,$TargetDirection)*8.0/1e6/$measuretime] \
			$droprate(t1) \
			[expr $prevstat(t2,byte,arr,$TargetDirection)*8.0/1e6/$measuretime] \
			$droprate(t2) ]
    if {$findtarget} {
	puts $nsoutfd_ [format "Summary (F/R): Av Q Dly %6.4g s / %6.4g s, Av Q Sz %6.4g B / %6.4g B, Av Loss %6.4f %% / %6.4f %%, Target %4.2f %%, scale %8.6f" \
			    $avQwaitF $avQwaitR $avQsizeBF $avQsizeBR $avlossF $avlossR \
			    $targetload $scale ]
    } else {
	puts $nsoutfd_ [format "Summary (F/R): Av Q Dly %6.4g s / %6.4g s, Av Q Sz %6.4g B / %6.4g B, Av Loss %6.4f %% / %6.4f %%, scale %8.6f" \
			    $avQwaitF $avQwaitR $avQsizeBF $avQsizeBR $avlossF $avlossR \
			    $scale ]
    }

    flush $nsoutfd_
    flush $resultfd_

    $Btopo finish
    $Btraffic finish
	
    $ns halt
}

proc monitorcntlnkflows {fmonF fmonR} {
    global ns Incremental_display_interval display_counter TIME
    global pcntBdep pcntLoad pcntPloss avBQsize BQsum
    global fmonFsize fmonFdelay fmonRsize fmonRdelay
    global prevstat tmix_R tmix_L TNsrc TNsnk
    global cntlnk_bw num_tmix_node
    global scale findtarget targetpcntbps TargetDirection nsoutfd_ test_time

    set measuretime [expr [$ns now] - $TIME]

    set fcF [$fmonF classifier]
    set fcR [$fmonR classifier]
    ####################### collect background traffic stats ###################
    # need to aggrigate for each background flow
    #
    foreach tp { pkt byte } pre { p b } {
	foreach act { arr dep drop } var { arrivals_ departures_ drops_ } {
	    set currstat(background,$tp,$act,F) 0
	    set currstat(background,$tp,$act,R) 0
	    for { set l 0 } { $l < $num_tmix_node } { incr l } {
		for { set r 0 }  { $r < $num_tmix_node } { incr r } {
		    set bflF [$fcF lookup auto [$tmix_L(T,$l) id] [$tmix_R(T,$r) id] 0]
		    set bflR [$fcR lookup auto [$tmix_R(T,$r) id] [$tmix_L(T,$l) id] 0]
		    if { $bflF != "" } {
			set currstat(background,$tp,$act,F) \
			    [expr $currstat(background,$tp,$act,F) + [ $bflF set $pre$var ] ]
		    }
		    if { $bflR != "" } {
			set currstat(background,$tp,$act,R) \
			    [expr $currstat(background,$tp,$act,R) + [ $bflR set $pre$var ] ]
		    }
		}
	    }
	    set incrstat(background,$tp,$act,F) \
		[expr $currstat(background,$tp,$act,F) - $prevstat(background,$tp,$act,F)]
	    set incrstat(background,$tp,$act,R) \
		[expr $currstat(background,$tp,$act,R) - $prevstat(background,$tp,$act,R)]
	}
    }
    foreach dir { F R } dn { 0 1 } {
	if { $incrstat(background,pkt,arr,$dir) > 0 } {
	    set pcntPdrop($dir) \
		[expr 100.0 *$incrstat(background,pkt,drop,$dir) / $incrstat(background,pkt,arr,$dir)]
	} else {
	    set pcntPdrop($dir) 0
	}
	set bbw [lindex $cntlnk_bw $dn]
	if { $incrstat(background,byte,dep,$dir) > 0 } {
	    set pcntBdepart($dir) \
		[expr 100.0*$incrstat(background,byte,dep,$dir)*8.0/$Incremental_display_interval/$bbw/1e6]
	} else {
	    set pcntBdepart($dir) 0
	}
	if { $incrstat(background,pkt,arr,$dir) > 0 } {
	    set pcntLd($dir) \
		[expr 100.0 * $incrstat(background,byte,arr,$dir)*8.0/$Incremental_display_interval/$bbw/1e6]
	} else {
		    set pcntLd($dir) 0
	}
	lappend pcntBdep($dir) $pcntBdepart($dir)
	lappend pcntLoad($dir) $pcntLd($dir)
	lappend pcntPloss($dir) $pcntPdrop($dir)
    }

    lappend avBQsize(F) [expr ([$fmonFsize set sum_] - [lindex $BQsum(F) end])/ $Incremental_display_interval]
    lappend avBQsize(R) [expr ([$fmonRsize set sum_] - [lindex $BQsum(R) end])/ $Incremental_display_interval]
    lappend BQsum(F) [$fmonFsize set sum_]
    lappend BQsum(R) [$fmonRsize set sum_]
    ####################### collect test flow statistics #######################
    if { $TargetDirection == "R" } {
	set FC $fcR
    } else {
	set FC $fcF
    }
    #Test sources don't operate when finding the target background rate
    if {$findtarget < 1} {
	set tmon(t1) [$FC lookup auto [$TNsrc(1) id] [$TNsnk(1) id] 0]
	set tmon(t2) [$FC lookup auto [$TNsrc(2) id] [$TNsnk(2) id] 0]
	
	foreach fl { t1 t2 } {
	    foreach tp { pkt byte } pre { p b } {
		foreach act { arr dep drop } var { arrivals_ departures_ drops_ } {
		    if { $tmon($fl) != "" } {
			set currstat($fl,$tp,$act,$TargetDirection) [ $tmon($fl) set $pre$var ]
		    } else {
			set currstat($fl,$tp,$act,$TargetDirection) 0
		    }
		    set incrstat($fl,$tp,$act,$TargetDirection) \
			[expr $currstat($fl,$tp,$act,$TargetDirection) - $prevstat($fl,$tp,$act,$TargetDirection)]
		}
	    }
	}
    }
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
    #################### print progress statistics ##############################
    foreach dir { F R } {
	if { $incrstat(total,byte,arr,$dir) > 0 } {
	    set pcntbacktraff($dir) [expr 100.0*$incrstat(background,byte,arr,$dir)/$incrstat(total,byte,arr,$dir)]
	} else {
	    set pcntbacktraff($dir) 0
	}
    }
    puts $nsoutfd_ [format "Time %4.1f Incremental Stats Mbps (F/R): Total %6.4g / %6.4g, Bkg %6.4g / %6.4g, %% Bg Traff %5.2f %% / %5.2f %%" \
			[expr $TIME+$display_counter*$Incremental_display_interval] \
			[expr $incrstat(total,byte,arr,F) *8.0/1e6/$Incremental_display_interval] \
			[expr $incrstat(total,byte,arr,R) *8.0/1e6/$Incremental_display_interval] \
			[expr $incrstat(background,byte,arr,F) *8.0/1e6/$Incremental_display_interval] \
			[expr $incrstat(background,byte,arr,R) *8.0/1e6/$Incremental_display_interval] \
			$pcntbacktraff(F) $pcntbacktraff(R) ]

    #Test sources don't operate when finding the target background rate
    if {$findtarget < 1} {
	foreach tf { t1 t2} {
	    if { $incrstat($tf,pkt,arr,$TargetDirection) > 0 } {
		set droprate($tf) [expr 100.0*$incrstat($tf,pkt,drop,$TargetDirection)/$incrstat($tf,pkt,arr,$TargetDirection)]
	    } else {
		set droprate($tf) 0
	    }
	}
	
	puts $nsoutfd_ [format "    Test flow incr stats (%s): T1 (Mbps) Arr %6.4g, T2 (Mbps) %6.4g, DropRate (T1/T2) %5.2f %% / %5.2f %%" \
			    $TargetDirection \
			    [expr $incrstat(t1,byte,arr,$TargetDirection) *8.0/1e6/$Incremental_display_interval] \
			    [expr $incrstat(t2,byte,arr,$TargetDirection) *8.0/1e6/$Incremental_display_interval] \
			    $droprate(t1) $droprate(t2) ]
    }
    flush $nsoutfd_

    ################# save currstats as prevstats ###########################
    foreach fl { total background } {
	foreach tp { pkt byte } {
	    foreach act { arr dep drop } {
		foreach dir { F R } {
		    set prevstat($fl,$tp,$act,$dir) $currstat($fl,$tp,$act,$dir)
		}
	    }
	}
    }
    if {$findtarget < 1} {
	foreach fl { t1 t2 } {
	    foreach tp { pkt byte } {
		foreach act { arr dep drop } {
		    set prevstat($fl,$tp,$act,$TargetDirection) $currstat($fl,$tp,$act,$TargetDirection)
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
    global tmix_R tmix_L TNsrc TNsnk num_tmix_node
    global findtarget
    # reset doesn't reset the per flow statistics properly
    set fcF [$fmonF classifier]
    set fcR [$fmonR classifier]
    foreach pre { p b } {
	foreach var { arrivals_ departures_ drops_ } {
	    for { set l 0 } { $l < $num_tmix_node } { incr l } {
		for { set r 0 }  { $r < $num_tmix_node } { incr r } {
		    set bflF [$fcF lookup auto [$tmix_L(T,$l) id] [$tmix_R(T,$r) id] 0]
		    set bflR [$fcR lookup auto [$tmix_R(T,$r) id] [$tmix_L(T,$l) id] 0]
		    if { $bflF != "" } {
			$bflF set $pre$var 0
		    }
		    if { $bflR != "" } {
			$bflR set $pre$var 0
		    }
		}
	    }
	    if {$findtarget < 1} {
		foreach sn {1 2} {
		    set tflF [$fcF lookup auto [$TNsrc($sn) id] [$TNsnk($sn) id] 0]
		    set tflR [$fcR lookup auto [$TNsrc($sn) id] [$TNsnk($sn) id] 0]
		    if { $tflF != "" } {
			$tflF set $pre$var 0
		    }
		    if { $tflR != "" } {
			$tflR set $pre$var 0
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
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Simulation setup
#
set NumExperiments [array size ExperimentNames]
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
	#for this scenario the balance doesn't matter as much
	set findtarget 2
	if { [ info exists TargetLoad ] } {
	    set targetload [expr $TargetLoad*$TargetLoadFactor($ThisExperiment)]
	} else {
	    puts stderr "Warning findtarget=1, but no TargetLoad specified" 
	    puts stderr "---> Skipping these tests"
	    set NewExperimentIteration -1 
	    set scale -1.0
	    puts stdout [format "tcpevaliterations,%d,%f" \
			     $NewExperimentIteration $scale]
	    exit
	}
    }
	
    set result_filename ${result_basename}_$ThisExperiment
    if { [ file exists $tmp_directory_/result_$result_filename ] && $TargetIter == 1} {
	#skip already experiments we have already done
	puts stderr "$tmp_directory_/result_$result_filename exists, so skipping"
	set skipexperiment 1
    }

    if { $skipexperiment == 1 } {
	if { $findtarget } {
	    set ExperimentIteration [expr $ExperimentIteration + [array size rtts] ]
	} else {
	    incr ExperimentIteration
	}
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

array set T_delay  $TestDelays($ThisExperiment)
set cntlnk_bw  $CntlnkBws($ThisExperiment)
set buffer_length $Buffers($ThisExperiment)
set maxRTT $MaxRTT($ThisExperiment)
puts stderr "***** Experiment $ExperimentIteration $ThisExperiment Scale=$scale *****"
##################### set up result file ####################################
set resultfd_ [open $tmp_directory_/result_$result_filename w]
puts $resultfd_ "src,n,bytes,time"
#AQM experiments?
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
# heuristic based on BDP:
set maxtrace [expr 3000.0 * $scale]
if {[info exists Warmup($ThisExperiment)]} {
    set warmup $Warmup($ThisExperiment)
}
set prefill_t $Prefill_t($ThisExperiment)
set longflowthresh [expr 0.5 * $prefill_t ]

if { [info exists warmup] } {
    set sim_time [expr $test_time + $warmup + $prefill_t]
} else {
    set warmup 0.0
    set sim_time [expr $test_time + $warmup + $prefill_t + $longflowthresh]
}
set binsecs [expr $test_time/200.0]
if { $sim_time > $maxtrace } {
    set sim_time $maxtrace
} 
# concurrent tmix sources.
set cc_tmix_srcs [expr [llength $tmix_base_cv_name]]
set pkt_overhead 40
set shufbalancetol 0.10
set shufloadtol 0.05
set error_ $shufbalancetol
set findstats $findtarget
set shuff [new Shuffle]
#set the random number stream we start from
$shuff set_start_stream $StartStream($ThisExperiment)
set shufrtns [$shuff shuffle_traces $scale $sim_time $binsecs $tmix_base_cv_name \
		  $findstats $findtarget $prefill_t $BCbps $cc_tmix_srcs $maxRTT \
		  $targetload $TargetDirection $longflowthresh \
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
puts stderr "---> $ThisExperiment , Sim_time=$sim_time, Prefill=$prefill_t, maxRTT = $maxRTT"
set avlossF 0
set avlossR 0
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
foreach fl { total background t1 t2 } {
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
    lappend BQsum($dir) 0
}
#####################################################################
set ftpstarttime(1) [expr $prefill_t+$warmup]
set TIME $ftpstarttime(1); # collect results after 
set ftpstarttime(2) [expr $ftpstarttime(1) + $test_time/2]
puts stderr [format "Start times (Tsrc1/Tsrc2): %f / %f" $ftpstarttime(1) $ftpstarttime(2)]

#
puts $nsoutfd_ "Scale = $scale"
flush $nsoutfd_
set Btraffic [new Create_traffic]
set Btopo [new Create_topology/Dumb_bell/Basic]
set tmix_cv_name $tmix_shuff_cv_name
############## Set up Background tmix traffic ##############################
if { $tmix_agent_type == "one-way" } {
    $Btraffic config_tmix -num_tmix_flow $num_tmix_flow \
	-num_tmix_node $num_tmix_node \
	-tmix_cv_name $tmix_cv_name \
	-tmix_agent_type $tmix_agent_type \
	-tmix_pkt_size $tmix_pkt_size \
	-test_tcp $Test_TCP \
	-useAQM $useAQM \
	-tmix_debug_output $tmix_debug_output
} else {
    $Btraffic config_tmix -num_tmix_flow $num_tmix_flow \
	-num_tmix_node $num_tmix_node \
	-tmix_cv_name $tmix_cv_name \
	-tmix_agent_type $tmix_agent_type \
	-tmix_debug_output $tmix_debug_output \
	-test_tcp $Test_TCP \
	-useAQM $useAQM \
	-tmix_pkt_size $tmix_pkt_size
    }

############ Configure Background traffic topology #########################
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
$Btopo config -cntlnk_bw $cntlnk_bw \
    -num_cntlnk 1 \
    -rttp $core_delay \
    -rtt_diff 0 \
    -edge_delay $edge_delay \
    -edge_bw $edge_bw \
    -core_delay $core_delay \
    -buffer_length $buffer_length \
    -traffic $Btraffic \
    -sim_time $sim_time \
    -scale $scalelist \
    -end $sim_time \
    -prefill_t $prefill_t_list \
    -prefill_si $prefill_si_list \
    -if_wireless $if_wireless \
    -nsoutfd $nsoutfd_

$Btopo create
array set cntlnk [$Btopo array get cntlnk_]
#tmix_L left-hand side nodes, tmix_R right-hand side nodes
array set tmix_L [ $Btopo array get tmix_L ]
array set tmix_R [ $Btopo array get tmix_R ]
##################  Setup Test Traffic ##################################
set SRC [$Btopo set SRC(T)]
set SINK [$Btopo set SINK(T)]
# for 1.5kBpacket
set Tbuffer [lindex $buffer_length 0]
set bufflen [expr 1000.0 * $Tbuffer / 8.0 / 1.5]
#only add test sources when we are not finding a target background rate
if {$findtarget < 1} {
    foreach tn { 1 2 } {
	# Set up test source and sink nodes and links
	if { $TargetDirection == "R" } {
	    set cnsrc 1
	    set cnsnk 0
	} else {
	    set cnsrc 0
	    set cnsnk 1
	}
	set TNsrc($tn) [$ns node]
	$ns duplex-link $TNsrc($tn) $cntlnk($cnsrc) 1000Mb $T_delay($tn)ms DropTail
	$ns queue-limit $TNsrc($tn) $cntlnk($cnsrc) $bufflen
	$ns queue-limit $cntlnk($cnsrc) $TNsrc($tn) $bufflen
	set TNsnk($tn) [$ns node] 
	$ns duplex-link $TNsnk($tn) $cntlnk($cnsnk) 1000Mb $T_delay([expr $tn + 2])ms DropTail
	$ns queue-limit $TNsnk($tn) $cntlnk($cnsnk) $bufflen
	$ns queue-limit $cntlnk($cnsnk) $TNsnk($tn) $bufflen
	#
	# Set up TCP source and sink agents
	set TCPsrc($tn) [new Agent/TCP/$SRC]
	if {$tn == 2} {
	    $TCPsrc set ssthresh_ [$TCPsrc set windowInit_]
	}
	set TCPsnk($tn) [new Agent/TCPSink/$SINK]
	$ns attach-agent $TNsrc($tn) $TCPsrc($tn)
	$ns attach-agent $TNsnk($tn) $TCPsnk($tn)
	# ftp for a long lived flow
	set Ftp($tn) [$TCPsrc($tn) attach-app FTP]
	set FtpRecv($tn) [$TCPsnk($tn) attach-app FTPrecv ]
	# set up variables for receive callback
	$FtpRecv($tn) start
	$FtpRecv($tn) label $tn
	# glue it all together
	$ns connect $TCPsrc($tn) $TCPsnk($tn)
	$ns at $ftpstarttime($tn)  "$Ftp($tn) start"
	$ns at $ftpstarttime(2) "$FtpRecv($tn) StartCounting"
	$ns at [expr $sim_time + $finishplus] "$Ftp($tn) stop"
    }
}
######### Set up Flow monitors and classifiers ##########################
# note that the Dest hash classifier doesn't work properly, so using SrcDest
set fmonF [ $ns makeflowmon SrcDest ]
set fmonR [ $ns makeflowmon SrcDest ]
$ns attach-fmon [ $ns link $cntlnk(0) $cntlnk(1) ] $fmonF
$ns attach-fmon [ $ns link $cntlnk(1) $cntlnk(0) ] $fmonR
######### Aggregate Monitors for central link ###############################
$fmonF set sampleInterval_ $Qstatsampleinterval
# makeflowmon does not set up the bytes integrator, so do it here
set FbytesInt [new Integrator]
$fmonF set-bytes-integrator $FbytesInt
set fmonFsize [$fmonF get-bytes-integrator]
set delaysamplesF [new Samples]
$fmonF set-delay-samples $delaysamplesF
set fmonFdelay [$fmonF get-delay-samples]
$fmonR set sampleInterval_ $Qstatsampleinterval
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

############ Simulation Finished #################################
close $resultfd_
set measuretime [expr [$ns now] - $TIME - $finishplus]
############## Calculation of next scale factor ##################
# This code was used to determine the scale factor that will
# yield the targetpcntbps for NewReno.
# It was used to determine the scale parameter outlined in the 
# irtf draft.

if { $findtarget } {
     #remove old trace files while we are trying to find a targetload
    foreach cvf $tmix_cv_name {
	file delete $cvf
    }
    if { $warmup < 1.0 } {
	puts $nsoutfd_ "*********Target has not been properly found *********"
	puts stderr "*********Target has not been properly found *********"
    }
    set scale -1.0
    # scale is the same for each or the rtts, it only differs for the central link BWs
    set ExperimentIteration [expr $ExperimentIteration + [array size rtts] ]
} else {
    incr ExperimentIteration
}
if {$ExperimentIteration > $NumExperiments} {
    set NewExperimentIteration -1 
} else {
    set NewExperimentIteration $ExperimentIteration
}

# Some ns code prints debug messages to stdout,
# so to identify this message it is prefixed with tcpevalscales.
puts stdout [format "tcpevaliterations,%d,%f" \
		 $NewExperimentIteration $scale]
close $nsoutfd_
exit 0



