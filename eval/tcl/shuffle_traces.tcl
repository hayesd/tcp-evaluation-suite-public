# Copyright (c) 2011
#  Swinburne University of Technology, Melbourne, Australia
#  All rights reserved.
# Copyright (c) 2013
#  University of Oslo, Norway
#  All rights reserved.
#
# Released under the GNU General Public License version 2.
#
# This script is used to shuffle the tmix traces to help with non-stationarity
# It uses tmix_cv_name.orig, and writes to tmix_cv_name.shuf
#
#
# Author:
# - David Hayes (davihay@ifi.uio.no or david.hayes@ieee.org)
Class Shuffle
#
Shuffle instproc init args {
    $self instvar srng
    $self instvar Bin_conn_data_list
    $self instvar burst_t_est
    $self instvar numSbins
    $self instvar binsize_us
    $self instvar conn_data
    $self instvar pre_conn_data
    $self instvar overhead_est
    $self instvar pre_conn_overhead
    $self instvar lastidle
    $self instvar Prefill_us Bpus CVtype
    $self instvar MSS pkt_oh
    $self instvar zip
    $self instvar base_seed
    $self instvar r_start_stream

    set Bin_conn_data_list(>) [list]
    set Bin_conn_data_list(<) [list]
    set burst_t_est(>) 0
    set burst_t_est(<) 0
    set binsize_us 10000000.0
    set numSbins(>) 0
    set numSbins(<) 0
    set conn_data(>) 0
    set conn_data(<) 0
    set pre_conn_data(>) 0
    set pre_conn_data(<) 0
    set pre_conn_overhead(>) 0
    set pre_conn_overhead(<) 0
    set lastidle(>) 0
    set lastidle(<) 0
    # default MSS, but should be overwritten by trace file
    set MSS(<) 1460
    set MSS(>) 1460
    set zip 0
    set base_seed 7654321
    set r_start_stream 0
    eval $self next $args
}
Shuffle instproc set_base_seed { bs } {
    $self instvar base_seed
    set base_seed $bs
}
Shuffle instproc set_start_stream { rss } {
    $self instvar r_start_stream
    set r_start_stream $rss
}
Shuffle instproc get_base_seed { } {
    $self instvar base_seed
    return $base_seed
}
#
# from http://wiki.tcl.tk/941.html
Shuffle instproc shuffle10 { binlist reqlen } {
    $self instvar srng
    set len [llength $binlist]
    set len2 $len
    for {set i 0} {$i < $len-1} {incr i} {
	set n [expr {int($i + $len2 * ([$srng next-random] * 1.0 / 0x7fffffff))}]
	incr len2 -1
	
	# Swap elements at i & n
	set temp [lindex $binlist $i]
	lset binlist $i [lindex $binlist $n]
	lset binlist $n $temp
     }
    return [lrange $binlist 0 $reqlen-1]
 }

Shuffle instproc add_burst_stats { data overhead dir} {
    $self instvar Bin_conn_data_list
    $self instvar burst_t_est
    $self instvar numSbins
    $self instvar binsize_us
    $self instvar conn_data
    $self instvar pre_conn_data
    $self instvar overhead_est
    $self instvar pre_conn_overhead
    $self instvar lastidle
    $self instvar Prefill_us Bpus CVtype
    $self instvar MSS pkt_oh

    if { $dir == ">" } {
	set oppdir <
    } else {
	set oppdir >
    }
    # Only post prefill. Prefill start compression will mess with this,
    # but we can't allow for it as we don't know what it is.
    set binidx [expr int(($burst_t_est($dir) - $Prefill_us)/$binsize_us)]
    set tdata [expr $data + $overhead + 0.0]
    set ackoh [expr $overhead + 0.0]
    set data_dur [expr $tdata/$Bpus]
    if {$data_dur > $binsize_us} {
	set bin_data [expr $binsize_us/$data_dur * $tdata]
	set bin_oh [expr $binsize_us/$data_dur * $ackoh]
    } else {
	set bin_data $tdata
	set bin_oh $ackoh
    }
    for {set idx $binidx} {$tdata > 0.0} {incr idx} {
	if {$idx >= 0} {
	    while {$idx >= $numSbins($dir)} {
		lappend Bin_conn_data_list($dir) 0.0
		incr numSbins($dir)
	    }
	    while {$idx >= $numSbins($oppdir)} {
		lappend Bin_conn_data_list($oppdir) 0.0
		incr numSbins($oppdir)
	    }
	    lset Bin_conn_data_list($dir) $idx [expr [lindex $Bin_conn_data_list($dir) $idx] + $bin_data]
	    #estimate acks in opposite direction
	    lset Bin_conn_data_list($oppdir) $idx [expr [lindex $Bin_conn_data_list($oppdir) $idx] + $bin_oh]
	}
	set tdata [expr $tdata - $bin_data]
	set ackoh [expr $ackoh - $bin_oh]
	if {$tdata < $bin_data} {
	    set bin_data $tdata
	    if {$ackoh > 0.0 } {
		set bin_oh $ackoh
	    } else {
		set bin_oh 0.0
	    }
	}
    }
    return $data_dur
}

Shuffle instproc process_burst {bdata dir first} {
# process a data burst
    $self instvar Bin_conn_data_list
    $self instvar burst_t_est
    $self instvar conn_data
    $self instvar pre_conn_data
    $self instvar overhead_est
    $self instvar pre_conn_overhead
    $self instvar lastidle
    $self instvar Prefill_us Bpus CVtype
    $self instvar MSS pkt_oh

    set conn_data($dir) [expr 0.0 + $conn_data($dir) + $bdata ]
    #$first adds overhead to account for connection start and end
    set oh_est  [expr ceil(1.0*$bdata/$MSS($dir))*$pkt_oh + 2.0*$first*$pkt_oh]
    set overhead_est($dir) [expr $overhead_est($dir) + $oh_est ]
    if { $burst_t_est($dir) < $Prefill_us } {
	set tbd [expr 1.0*$bdata / $Bpus]
	set tpl [expr $Prefill_us - $burst_t_est($dir)]
	if { $tbd > $tpl } {
	    set propbdata [expr ceil(($tbd - $tpl) * $bdata / $tbd)]
	} else {
	    set propbdata [expr $bdata + 0.0]
	}
	set pre_conn_data($dir) [expr $pre_conn_data($dir) + $propbdata ]
	set pre_conn_overhead($dir) [expr $pre_conn_overhead($dir) + int(ceil(1.0*$propbdata/$MSS($dir))*$pkt_oh) ]
    }
    set burst_dur [$self add_burst_stats $bdata $oh_est $dir ]
    set burst_t_est($dir) [expr $burst_t_est($dir) + $burst_dur]
    set lastidle($dir) 0
    if { $CVtype == "S" } {
	set lastidle($dir) 0
    }
}

# Shuffles traces, returning the average connection rate, and the sum of data in
# each direction over the simulation time
Shuffle instproc shuffle_traces { scale simtime binsecs tmix_base_cv_name \
				      findstats findtarget prefill_t bps \
				      cc_tmix_srcs maxrtt \
				      targetload targetdirection \
				      longflowthresh mss pktoh \
				      balancetol loadtol } {
    $self instvar srng
    $self instvar Bin_conn_data_list
    $self instvar numSbins
    $self instvar binsize_us
    $self instvar burst_t_est
    $self instvar conn_data
    $self instvar pre_conn_data
    $self instvar overhead_est
    $self instvar pre_conn_overhead
    $self instvar lastidle
    $self instvar Prefill_us Bpus CVtype
    $self instvar MSS pkt_oh
    $self instvar zip
    $self instvar base_seed
    $self instvar r_start_stream
    # scale - scale for CV start times
    # simtime - total simulation time
    # bps - bottleneck link capacity
    # binsecs - size of bin in simulation time seconds
    # cc_tmix_srcs - tmix source operating concurrently
    # maxrtt - maximum rtt tmix sources experience
    # targetload - target load as a percentage of the bottleneck link capacity
    # targetdirection - direction for load target 
    # prefill_t - only include flows longer than this during prefill
    #                  the idea being to rapidly start enough long flows
    # mss - maximum segment size'
    # pkt_oh - protocol overhead per packet
    #
    # Bytes per microsecond

    set pkt_oh $pktoh
    #set default MSS, but this should be overwritten by trace file
    set MSS(>) $mss					  
    set MSS(<) $mss					  
    set Prefill_us [expr $prefill_t * 10**6]
    set longflowthresh_us [expr $longflowthresh * 10**6]
    set minbins [expr int(ceil(1.0*$simtime/$binsecs))]
    set min_num_balance 5
    if {$findtarget} {
	set findstats 1
    }

    set awkinfo 1					  
    set looparound 1
    set highscale 0.0
    set lowscale 0.0
    set highload 0.0
    set lowload 0.0
    set iterationtrouble 0					  
    set multiiterationtrouble 0					  
    while { $looparound } {
	# concurrancy factor -  scales roughtly with scale and rtt
	# This is dependent on the actual traces, and when they change this will need changing.
	set concur [expr max(1.0, ($cc_tmix_srcs * 500.0/$scale*$maxrtt))]
	set Bpus [expr 1.0*$bps/8/$concur/(10**6)]
	set num_balance 0
	if {[info exists srng]} {
	    $srng destroy
	    unset srng
	}
	set srng [new RNG]
	set seed $base_seed
	$srng seed $seed
	set outfilelist [list]
	set ofwritelist [list]
	foreach cvf $tmix_base_cv_name {
	    set cvf_base [lindex [file split $cvf] end]
	    if {$zip} {
		set ofname ${cvf_base}_${scale}.shuf.gz
	    } else {
		set ofname ${cvf_base}_${scale}.shuf
	    }
	    lappend outfilelist $ofname
	    set fileexists 0
	    if { [ file exists $ofname ] } {
		if {[ file size $ofname ] > 0} {
		    puts stderr "${cvf_base}_${scale}.shuf already exists"
		    lappend ofwritelist 0
		    set fileexists 1
		}
	    }
	    if {$fileexists == 0 } {
		lappend ofwritelist 1
		if {$findtarget == 0} {
		    if {$zip} {
			set shufFid($cvf) [open "| gzip -c > $ofname" w]
		    } else {
			set shufFid($cvf) [open $ofname w]
		    }
		    fconfigure $shufFid($cvf) -buffering full -buffersize 10000
		}
	    }
	}
	if {$targetdirection == "F" } {
	    set td >
	} elseif {$targetdirection == "R" } {
	    set td <
	} else {
	    # both directions are necessary for shared channels, such as wifi
	    set td b
	}
	# traces are at least 3000 long
	
	set tracelength 3000
	set totalbins [expr ceil(1.0*$scale*$tracelength/$binsecs) ]
	set maxbins [expr ceil(1.0*$simtime/$binsecs) ]
	if {$maxbins > $totalbins || $findtarget == 1} {
	    set maxbins $totalbins
	}
	#bin size in microseconds
	set binsize_us [expr 1.0*$binsecs*10**6]
	set scale_binsize_us [expr $binsize_us / $scale]
	set prefill_bins [expr int(ceil(1.0*$prefill_t / $binsecs))] 
	if {[info exists baseseq]} {
	    unset baseseq
	    unset totaldataperbin
	}
	set baseseq [list]
	set totaldataperbin [list]
	
	for { set b 0 } { $b < $totalbins } { incr b } { lappend baseseq $b }
	
	set numtracefiles [llength tmix_base_cv_name]
	for { set rss 0 } { $rss < $r_start_stream } {incr rss} {
	    $srng next-substream
	}
	foreach cvf $tmix_base_cv_name {
	    if {[ info exists newbinseq($cvf) ]} {
		unset newbinseq($cvf)
	    }
	    $srng next-substream
	    set newbinseq($cvf) [$self shuffle10 $baseseq [expr int($maxbins)] ]
	    set lastnbs($cvf) 0
	}
	
	#initialise stat collection
	set num_conns 0
	set Sum_prefill_data(>) 0
	set Sum_prefill_data(<) 0
	set Sum_prefill_overhead(>) 0
	set Sum_prefill_overhead(<) 0
	if {[ info exists Bin_conn_data_list(>) ]} {
	    unset Bin_conn_data_list(>)
	    unset Bin_conn_data_list(<)
	}
	set Bin_conn_data_list(>) [list]
	set Bin_conn_data_list(<) [list]
	set numSbins(>) 0
	set numSbins(<) 0
	foreach bs $baseseq {
	    if { $bs >= $maxbins } {
		# have enough for this simulation
		break
	    }
	    set Bin_conn_data(>) 0
	    set Bin_conn_data(<) 0
	    #
	    # cycle through tmix traffic files
	    foreach cvf $tmix_base_cv_name wo $ofwritelist {
		if {$wo == 0 && $findstats == 0} {
		    continue
		}
		set nbs [lindex $newbinseq($cvf) $bs]
		set lower_time [expr $nbs * $scale_binsize_us]
		set upper_time [expr ($nbs + 1) * $scale_binsize_us]
		set offset_time [expr ($bs - $nbs) * $scale_binsize_us]
		# This is a bit clumbsy, but faster than plane tcl line by line reading
		#NB mawk does not handle the large integer times well, so gawk is needed
		if { [catch {set cvfFid($cvf) [open "| gawk \"BEGIN \{ out=0 \} \{if(\$1 ~ /^\[SC\]/) \{if (\$2 >= $lower_time) \{ if (\$2 < $upper_time) \{out=1; print \$0;\} else \{out=0;exit;\}\}\} else \{ if (out) print \$0\}\}\"  ${cvf}.orig" "r"]} ] } {
		    if { $awkinfo } {
			puts stderr "--No gawk, using awk. Consider installing gawk for speedup and abilities with large integers"
			set awkinfo 0
		    }
		    set cvfFid($cvf) [open "| awk \"BEGIN \{ out=0 \} \{if(\$1 ~ /^\[SC\]/) \{if (\$2 >= $lower_time) \{ if (\$2 < $upper_time) \{out=1; print \$0;\} else \{out=0;exit;\}\}\} else \{ if (out) print \$0\}\}\"  ${cvf}.orig" "r"]
		}
		fconfigure $cvfFid($cvf) -buffering full -buffersize 100000
	    }
	    foreach cvf $tmix_base_cv_name wo $ofwritelist {
		if {$wo == 0 && $findstats == 0} {
		    continue
		}
		set nbs [lindex $newbinseq($cvf) $bs]
		set offset_time [expr ($bs - $nbs) * $scale_binsize_us]

		set cvfiledata [read $cvfFid($cvf)]
		set cvdata [split $cvfiledata "\n"]
		set ThisConnOut 0
		set conn_data(>) 0
		set conn_data(<) 0
		set overhead_est(>) 0
		set overhead_est(<) 0
		set pre_conn_data(>) 0
		set pre_conn_data(<) 0
		set pre_conn_overhead(>) 0
		set pre_conn_overhead(<) 0
		set idle(<) 0
		set idle(>) 0
		set lastidle(>) 0
		set lastidle(<) 0
		set connout [list]
		set writethis 0
		set firstburst(>) 0
		set firstburst(<) 0
		foreach line $cvdata {
		    if { [regexp ^(S|C) $line match] } {
			set firstburst(>) 1
			set firstburst(<) 1
			if { [llength $connout] > 0 } {
			    set outdata 0
			    set idle(<) [expr $idle(<) - $lastidle(<)]
			    set idle(>) [expr $idle(>) - $lastidle(>)]
			    set scaled_conntime [expr $scale * $newConnTime]
			    if { $scaled_conntime > $Prefill_us } {
				set writethis 1
			    } else {
				if { [expr $scaled_conntime + $idle(>) + ($conn_data(>) + $overhead_est(>))/ $Bpus] > $longflowthresh_us \
					 || [expr $scaled_conntime + $idle(<) + ($conn_data(<) + $overhead_est(<)) / $Bpus] > $longflowthresh_us } {
				    # this in effect changes the connection departure
				    # rate, and only include the earlier connections
				    # that would have been active past longflowthresh
				    set Sum_prefill_data(>) [expr $Sum_prefill_data(>) + $pre_conn_data(>)]
				    set Sum_prefill_data(<) [expr $Sum_prefill_data(<) + $pre_conn_data(<)]
				    set Sum_prefill_overhead(>) [expr $Sum_prefill_overhead(>) + $pre_conn_overhead(>)]
				    set Sum_prefill_overhead(<) [expr $Sum_prefill_overhead(<) + $pre_conn_overhead(<)]
				    set writethis 1
				} 
			    }
			    if  { $writethis } {
				if { $wo && $findtarget == 0} {
				    foreach co $connout {
					puts $shufFid($cvf) $co
				    }
				}
				set writethis 0
			    }
			    set connout [list]
			}
			set CVtype [string range $line 0 0 ]
			set splitline [split $line " "]
			set ConnTime [lindex $splitline 1]
			set newConnTime [expr round($ConnTime + $offset_time) ]
			set burst_t_est(<) [expr $scale * $newConnTime]
			set burst_t_est(>) $burst_t_est(<)
			set splitline [lreplace $splitline 1 1 $newConnTime]
			set newConnline [join $splitline " "]
			lappend connout $newConnline
			set conn_data(>) 0
			set conn_data(<) 0
			set overhead_est(>) 0
			set overhead_est(<) 0
			set pre_conn_data(>) 0
			set pre_conn_data(<) 0
			set pre_conn_overhead(>) 0
			set pre_conn_overhead(<) 0
			set idle(<) 0
			set idle(>) 0
			set lastidle(>) 0
			set lastidle(<) 0
			set ThisConnOut 1
			incr num_conns
		    } else {
			#lappend connout $line
			# collect some statistics
			if { [regexp ^(>|c>) $line match] } {
			    set splitline [split $line " "]
			    set bdata [lindex $splitline 1]
			    set splitline [lreplace $splitline 1 1 $bdata]
			    set line [join $splitline " "]
			    $self process_burst $bdata > $firstburst(>)
			    set firstburst(>) 0
			} elseif { [regexp ^(<|c<) $line match] } {
			    set splitline [split $line " "]
			    set bdata [lindex $splitline 1]
			    set splitline [lreplace $splitline 1 1 $bdata]
			    set line [join $splitline " "]
			    $self process_burst $bdata < $firstburst(<)
			    set firstburst(<) 0
			} elseif { [regexp ^(t>) $line match] } {
			    set splitline [split $line " "]
			    set wt [lindex $splitline 1]
			    set idle(>) [expr $idle(>) + $wt ]
			    set burst_t_est(>) [expr $burst_t_est(>) + $wt]
			    set lastidle(>) $wt
			} elseif { [regexp ^(t<) $line match] } {
			    set splitline [split $line " "]
			    set wt [lindex $splitline 1]
			    set idle(<) [expr $idle(<) + $wt ]
			    set burst_t_est(<) [expr $burst_t_est(<) + $wt]
			    set lastidle(<) $wt
			} elseif { [regexp ^(t) $line match] } {
			    # sequential
			    set splitline [split $line " "]
			    set wt [lindex $splitline 1]
			    set burst_t_est(>) [expr $burst_t_est(>) + $wt]
			    set burst_t_est(<) [expr $burst_t_est(<) + $wt]
			    set idle(>) [expr $idle(>) + $wt ]
			    set idle(<) $idle(>)
			    set lastidle(>) $wt
			    set lastidle(<) $wt
			} elseif { [regexp ^(m) $line match] } {
			    # mss
			    set splitline [split $line " "]
			    set MSS(>) [lindex $splitline 1]
			    set MSS(<) [lindex $splitline 2]
			}
			 
			lappend connout $line
			
		    }
		}
		set lastnbs($cvf) $nbs
		close $cvfFid($cvf)
	    }
	    if {$findtarget && $targetload && $bs > $minbins + $prefill_bins} {
		# check load balance
		#
		# This is used to estimate if the simulation is long
		# enough.  If the first and second half offered loads
		# average to almost the same value, then the
		# simulation is considered long enough.
		#
		# Real traces often have very heavy tails. Although
		# the statistics gathering by add_burst_stats
		# mitigates this, a single large bin can skew
		# results.
		#
		set bcdl_length [expr $bs - $prefill_bins]
		set third [expr $bcdl_length/3]
		# only work with the last 2/3
		set working_length [expr $bcdl_length - $third]
		set mid [expr $working_length/2 + $third]
		if { $td == "b" } {
		    set connB_l [lrange $Bin_conn_data_list(>)  $third  $mid-1 ]
		    set connB_u [lrange $Bin_conn_data_list(>) $mid $bcdl_length ]
		    set connB_l [concat $connB_l [lrange $Bin_conn_data_list(<)  $third  $mid-1 ]]
		    set connB_u [concat $connB_u [lrange $Bin_conn_data_list(<) $mid $bcdl_length ]]
		    set avB_l [expr ([ join $connB_l +]+0.0)/(0.5 * [llength $connB_l])]	  
		    set avB_u [expr ([ join $connB_u +]+0.0)/(0.5 * [llength $connB_u])]
		} else {
		    set connB_l [lrange $Bin_conn_data_list($td)  $third  $mid-1 ]
		    set connB_u [lrange $Bin_conn_data_list($td) $mid $bcdl_length ]
		    set avB_l [expr ([ join $connB_l +]+0.0)/[llength $connB_l]]	  
		    set avB_u [expr ([ join $connB_u +]+0.0)/[llength $connB_u]]
		}		    
		set est_load_l [expr 100.0 * $avB_l * 8.0 / $binsecs / $bps]
		set est_load_u [expr 100.0 * $avB_u * 8.0 / $binsecs / $bps]
		if { $avB_l > 0 } {
		    if { [expr abs($est_load_l - $est_load_u) / $est_load_l] <= $balancetol} {
			incr num_balance
			if { $num_balance < $min_num_balance } {
			    #puts stderr ">>>>>> Balanced [expr $num_balance] times $est_load_l / $est_load_u"
			} else {
			    set simtime [expr $binsecs * $bs]
			    puts stderr "== $tmix_base_cv_name"
			    puts stderr "  >>>> Balanced consecutively $num_balance times, $est_load_l / $est_load_u, Simtime $simtime"
			    break;
			}
		    } else {
			set num_balance 0
		    }
		}
	    }
	}
	
	if { $findtarget == 0 } {
	    set looparound 0
	} else {
	    set bl [expr $bs - $prefill_bins]
	    #only count later 2/3
	    set third [expr $bl/3]
	    if { $td == "b" } {
		set ur [lrange $Bin_conn_data_list(>) $third $bl]
		set ur [concat $ur [lrange $Bin_conn_data_list(<) $third $bl ]]
		set est_load [expr 100.0 * ([ join $ur +]+0.0)/(0.5 * [llength $ur]) * 8.0 / $binsecs / $bps]
	    } else {
		set ur [lrange $Bin_conn_data_list($td) $third $bl]
		set est_load [expr 100.0 * ([ join $ur +]+0.0)/[llength $ur] * 8.0 / $binsecs / $bps]
	    }
	    puts stderr "== $tmix_base_cv_name"
	    puts stderr "  ¤¤¤¤ Estload $est_load"
	    if { abs($est_load - $targetload)/$targetload <= $loadtol } {
		#done but need to write out the trace files
		puts stderr "  ¤¤¤¤ Found estimate=$est_load, target=$targetload -- write out file"
		set findtarget 0
		# already written this file
		if {$wo == 0} {
		    set looparound 0
		}  
	    } else {
		if {$est_load > $targetload} {
		    if {$highload != 0.0} {
			if {$est_load >= $highload} {
			    incr iterationtrouble
			} else {
			    if {$iterationtrouble > 0 } {
				incr iterationtrouble -1
			    }
			}
		    }
		    set highload $est_load
		    set lowscale $scale
		} else {
		    if {$lowload != 0.0} {
			if {$est_load <= $lowload} {
			    incr iterationtrouble
			} else {
			    if {$iterationtrouble > 0 } {
				incr iterationtrouble -1
			    }
			}
		    }
		    set lowload $est_load
		    set highscale $scale
		}
		if {$iterationtrouble > 5 } {
		    puts stderr "== $tmix_base_cv_name"
		    puts stderr "  §§§§§ trouble iterating, reseting max and min"
		    if {$est_load > $targetload} {
			set highload $est_load
			set lowscale $scale
			set highscale 0.0
		    } else {
			set lowload $est_load
			set highscale $scale
			set lowscale 0.0
		    }
		    set inerationtrouble 0
		    incr multiiterationtrouble
		}
		if {$multiiterationtrouble > 4 } {
		    puts stderr "== $tmix_base_cv_name"
		    puts stderr "  §§§§§ Target load cannot be found with this level of averaging"
		    set looparound 0
		}
		set oldscale $scale
		if {$lowload == 0.0 || $highload == 0.0} {
		    set scale [expr $scale + $scale * 0.5*($est_load - $targetload)/$targetload]
		} else {
		    if {$est_load > $targetload} {
			set scale [expr $scale + ($highscale - $scale)/2.0]
		    } else {
			set scale [expr $scale - ($scale - $lowscale)/2.0]
		    }
		}
		if { abs($oldscale - $scale) < 1e-10 } {
		    puts stderr "== $tmix_base_cv_name"
		    puts stderr "  §§§§§ Target load cannot be found with this level of averaging"
		    set looparound 0
		} else {
		    puts stderr "== $tmix_base_cv_name"
		    puts stderr "  ¤¤¤¤¤¤ New Scale $scale"
		    # continue with new scale
		}
	    }
	}
    }
    if {$findtarget == 0} {
	foreach cvf $tmix_base_cv_name  wo $ofwritelist {
	    if {$wo} {
		close $shufFid($cvf)
	    }
	}
    }
    set conn_arr_rate [expr 1.0*$num_conns / $simtime]
    set bl [expr $bs - $prefill_bins]
    return [list $outfilelist \
		[list $conn_arr_rate \
		     [expr $Sum_prefill_data(>) + $Sum_prefill_overhead(>)] \
		     [expr $Sum_prefill_data(<) + $Sum_prefill_overhead(<)] \
		     [expr ([ join [lrange $Bin_conn_data_list(>) 0 $bl] +]+0.0)] \
		     [expr ([ join [lrange $Bin_conn_data_list(<) 0 $bl] +]+0.0)]] \
		$scale $simtime]
}

# Shuffles within a small section of a trace - used when comparing to standard TCP
Shuffle instproc intra_shuffle { scale simtime binsecs intrabinsecs tmix_cv_name } {
    $self instvar srng
    # scale - scale for CV start times
    # simtime - total simulation time
    # binsecs - size of bin in simulation time seconds

    # set seed for consistent results
    set srng [new RNG]
    set seed 54321
    $srng seed $seed
    
    # traces are at least 3000 long

    set tracelength $simtime
    set totalbins [expr ceil(1.0*$scale*$tracelength/$binsecs) ]
    #bin size in microseconds
    set scale_binsize_us [expr 1.0*$binsecs*10**6 / $scale]
    set intrascale_binsize_us [expr 1.0*$intrabinsecs*10**6 / $scale]
    set numintrabins [expr ceil(1.0*$scale_binsize_us/$intrascale_binsize_us)]
    #
    # cycle through tmix traffic files
    set baseseq [list]
    for { set b 0 } { $b < $numintrabins } { incr b } { lappend baseseq $b }
 
    foreach cvf $tmix_cv_name {
	$srng next-substream
	set cvfFid [open ${cvf}.orig r]
	set binstart [tell $cvfFid]
	if { [ file exists ${StdSrc}${cvf} ] } {
	    puts stderr "${StdSrc}${cvf} already exists"
	    break
	}
	set stdFid [open ${StdSrc}${cvf} w]
	set lastline 0
	set lastnibs 0
	for { set bin 0 } { $bin < $totalbins } {incr bin} {
	    unset newintrabinseq
	    set newintrabinseq [$self shuffle10 $baseseq [expr int($numintrabins)]]
	    foreach nibs $newintrabinseq bs $baseseq {
		if { $bs >= $numintrabins } {
		    # finished this intrabin
		    break
		}
		if { $nibs < $lastnibs } {
		    seek $cvfFid $lastbinstart
		} else {
		    seek $cvfFid $lastline
		}
		puts stderr "$nibs $bs"
		set ThisConnOut 0
		
		set lastbinstart [tell $cvfFid]
		while { [gets $cvfFid line ] > 0 } {
		    if { [regexp ^(S|C) $line match] } {
			set splitline [split $line " "]
			set ConnTime [lindex $splitline 1]
			if { $ConnTime >= [expr $bin * $scale_binsize_us + $nibs * $intrascale_binsize_us] } {
			    if { $ConnTime < [expr $bin * $scale_binsize_us + ($nibs + 1) * $intrascale_binsize_us] } {
				set newConnTime [expr round($ConnTime + (($bs - $nibs) * $intrascale_binsize_us)) ]
				set splitline [lreplace $splitline 1 1 $newConnTime]
				set newConnline [join $splitline " "]
				puts $stdFid $newConnline
				set ThisConnOut 1
			    } else {
				break
			    }
			} else {
			    set ThisConnOut 0
			}
		    } elseif { $ThisConnOut } {
			puts $stdFid $line
		    }
		    set lastline [tell $cvfFid]
		}
		set lastnibs $nibs
	    }
	}
	close $cvfFid
	close $stdFid
    }
}
