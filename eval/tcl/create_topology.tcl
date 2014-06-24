#
# Copyright (c) 2007  NEC Laboratories China.
# All rights reserved.
# Copyright (c) 2010-2011
#  Swinburne University of Technology, Melbourne, Australia
#  All rights reserved.
#
#
# Authors:
# - Gang Wang (wanggang@research.nec.com.cn)
# - Yong Xia   (xiayong@research.nec.com.cn)
#
# Enhanced, updated and many parts rewritten
# - David Hayes (dahayes@swin.edu.au or david.hayes@ieee.org)
#
# Initial work on scale parameter
# - Dongxia Xu
#
# Released under the GNU General Public License version 2.
#
# This code creates the simulation topology with settable 
# parameters. It contains one base class "Create_topology" and  
# inherited classes, "Dumb_bell" and "Parking log (or Multiple-bottleneck)".
#
#
#            Src_1                                    Sink_1 
#                 \                                  / 
#                  \          central link          / 
#          Src_2 --- Router1 -------------- Router2 --- Sink_2 
#                  /                                \ 
#                 /                                  \ 
#            Src_N                                    Sink_N 
#    
#                     Figure 1: Dumb-bell topology. 
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
#               Figure 2. Multiple-bottleneck Scenario topology. 
#
#
Class Create_topology 
Class Create_topology/Dumb_bell -superclass Create_topology
# basic scenario
Class Create_topology/Dumb_bell/Basic -superclass Create_topology/Dumb_bell

source $env(TCPEVAL)/tcl/tcp_schemes.tcl
source $env(TCPEVAL)/tcl/aqm_defaults.tcl

Create_topology instproc init args {
    # system parameters
    $self instvar traffic_         ;# the traffic instance
    $self instvar graph_           ;# the graph instance
    $self instvar sim_time_	   ;# simulation stop time
    $self instvar nsoutfd_         ;# output file
    
    # topology parameters
    $self instvar cntlnk_bw_       ;# bottleneck capacity, Mbps
    $self instvar num_cntlnk_      ;# number of cntlnk, for Parking lot
    $self instvar rttp_            ;# round trip propagation delay, ms
    $self instvar verbose_         ;# output format
    $self instvar rtt_diff_        ;# flow rtt difference, ms
    $self instvar delay_diff_      ;# delay differences
    $self instvar cntlnk_delay_	   ;# bottleneck delay
    $self instvar non_cntlnk_delay_ ;# non bottleneck delay
    $self instvar non_cntlnk_bw_   ;# non bottleneck bandwidth
    $self instvar cntlnk_buf_	   ;# bottleneck buffer
    $self instvar non_cntlnk_buf_  ;# non bottleneck buffer
    $self instvar crs_cntlnk_delay_ ;# cross cntlnk delay
    $self instvar edge_delay_      ;# edge link delay
    $self instvar edge_bw_         ;# edge link bandwidth
    $self instvar core_delay_      ;# core delay for intermediate link
    $self instvar if_wireless_     ;# wireless enabled
    $self instvar w_datarate_      ;# wireless datarate
    $self instvar tmix_enabled_    ;# tmix enabled, compatible for former release
    $self instvar cntlnk_buf_bdp_  ;# buffer size % bdp
    $self instvar prefill_t_       ;# prefill time
    $self instvar prefill_si_      ;# prefill starting interval
    $self instvar scale_           ;#scale rate
    $self instvar end_             ;# Simulation end time
    $self instvar compare_std_	   ;# Compare with standard TCP
    $self instvar flow_groups_     ;# normally { T for test flows, S
				    # for standard TCP flows }
    
    eval $self next $args
    set verbose_ 0
    set if_wireless_ 0
    set w_datarate_ 0
    set compare_std_ 0
    set flow_groups_ [list T]
    set edge_delay_ [list]
    set edge_bw_ [list]
    set cntlnk_bw_ [list]
    set cntlnk_buf_ [list]
    set scale_ [list]
    set prefill_t_ [list]
    set prefill_si_ [list]
    set tmix_enabled_ 0
    set cntlnk_buf_bdp_ 1.0
}


# Config procedures

Create_topology instproc cntlnk_bw {val} {
    $self instvar cntlnk_bw_
    lappend cntlnk_bw_ $val
}

Create_topology instproc num_cntlnk {val} {
    $self set num_cntlnk_ $val
}

Create_topology instproc prefill_t {val} {
    $self instvar prefill_t_
    lappend prefill_t_ $val
}

Create_topology instproc prefill_si {val} {
    $self instvar prefill_si_
    lappend prefill_si_ $val
}

Create_topology instproc scale {val} {
    $self instvar scale_
    lappend scale_ $val
}

Create_topology instproc end {val} {
    $self set end_ $val
}

Create_topology instproc rttp {val} {
    $self set rttp_ $val
}

Create_topology instproc crs_cntlnk_delay {val} {
    $self set crs_cntlnk_delay_ $val
}
Create_topology instproc verbose {val} {
    $self set verbose_ $val
}

Create_topology instproc rtt_diff {val} {
    $self set rtt_diff_ $val
}

Create_topology instproc traffic {val} {
    $self set traffic_  $val
}

Create_topology instproc graph {val} {
    $self set graph_  $val
}

Create_topology instproc sim_time {val} {
    $self set sim_time_  $val
}

Create_topology instproc edge_delay {val} {
    $self instvar edge_delay_
    lappend edge_delay_ $val
}

Create_topology instproc edge_bw {val} {
    $self instvar edge_bw_
    lappend edge_bw_ $val
}

Create_topology instproc core_delay {val} {
    $self set core_delay_ $val
}

Create_topology instproc buffer_length {val} {
    $self instvar buffer_length_
    lappend buffer_length_ $val
}

Create_topology instproc if_wireless {val} {
    $self set if_wireless_ $val
}

Create_topology instproc wireless_datarate {val} {
    $self set w_datarate_ $val
}

Create_topology instproc compare_std {val} {
    $self set compare_std_ $val
    if { $val > 0 } {
	$self set flow_groups_ [list T S]
    }
}

Create_topology instproc cntlnk_buf_bdp {val} {
    $self set cntlnk_buf_bdp_ $val
}

Create_topology instproc nsoutfd {val} {
    $self set nsoutfd_ $val
}

# Dispatch args
Create_topology instproc init_var args {
    set shadow_args ""
    $self instvar cntlnk_bw_
    for {} {$args != ""} {set args [lrange $args 2 end]} {
        set key [lindex $args 0]
        set val [lindex $args 1]
        if {$val != "" && [string match {-[A-z]*} $key]} {
            set cmd [string range $key 1 end]
            foreach arg_item $val {
                $self $cmd $arg_item
		lappend shadow_args $key $arg_item
	    }
	}
    }
    return $shadow_args
}

# Config the parameters
Create_topology instproc config args {
    set args [ eval $self init_var $args]
}

# Choose a subset to display
Create_topology instproc show_subset {total} {
    set total_ [expr round($total)] ;# disable cases such as 15.0
    set interval_ 1
    if { $total_ >3 } {
	set interval_ [expr $total_ /3]
    }
    set start_ 0
    set list_ 0
    for { set start_ [expr $start_ + $interval_] } { $start_ < $total_ } { set start_ [expr $start_+$interval_] } {
	set list_ "$list_ $start_"
    }
    return $list_
}

# Bottleneck parameters
Create_topology instproc set_parameters {topo num_cntlnk} {
    $self instvar rttp_ rtt_diff_ cntlnk_bw_ traffic_ graph_
    $self instvar delay_diff_ cntlnk_delay_ non_cntlnk_delay_ non_cntlnk_bw_ 
    $self instvar cntlnk_buf_ non_cntlnk_buf_ verbose_
    $self instvar sim_time_
    $self instvar nsoutfd_
    $self instvar crs_cntlnk_delay_
    $self instvar if_html_ html_index_
    $self instvar traffic_
    $self instvar tmix_enabled_
    $self instvar cntlnk_buf_bdp_
    # Initialize parameters
    set cntlnk_delay_ [expr $rttp_ * 0.5 * 0.8 / $num_cntlnk] 
    set delay_diff_ [expr $rtt_diff_ * 1.0 / 4.0]  ;# ms
    set non_cntlnk_delay_ [expr $rttp_ * 0.5 * 0.2 / 2.0]
    if { [llength $cntlnk_bw_] > 1 } {
	set non_cntlnk_bw_ [expr [lindex [lsort -decreasing $cntlnk_bw_] 0] * 1.0 * 2]    ;# Mbps
    } else {
	set non_cntlnk_bw_ [expr $cntlnk_bw_ * 1.0 * 2]    ;# Mbps
    }
    if { $tmix_enabled_ == 1 } {
        $self instvar buffer_length_
	if { [llength $cntlnk_bw_] > 1 } {
	    lappend cntlnk_buf_ [format "%0.2f" [expr $cntlnk_buf_bdp_ * [lindex $cntlnk_bw_ 0] * [lindex $buffer_length_ 0] / 8.0 / 1.5] ]  ;# in 1.5KB pkt
	    lappend cntlnk_buf_ [format "%0.2f" [expr $cntlnk_buf_bdp_ * [lindex $cntlnk_bw_ 1] * [lindex $buffer_length_ 1] / 8.0 / 1.5] ]  ;# in 1.5KB pkt
	    set non_cntlnk_buf_ [lindex [lsort -decreasing $cntlnk_buf_] 0]
	} else {
	    set cntlnk_buf_ [format "%0.2f" [expr $cntlnk_buf_bdp_ * $cntlnk_bw_ * [lindex $buffer_length_ 0] / 8.0 / 1.5] ]  ;# in 1.5KB pkt
	    lappend buffer_length_ [lindex $buffer_length_ 0]
	    set non_cntlnk_buf_ [expr $cntlnk_buf_]
	}
        set num_tmix [$traffic_ set num_tmix_flow_]
    } else {
	set cntlnk_buf_ [expr $cntlnk_buf_bdp_ * $cntlnk_bw_ * $avg_rtt_ / 8.0]  ;# in 1KB pkt
	if { $cntlnk_buf_ < $min_cntlnk_buf_ } { set cntlnk_buf_ $min_cntlnk_buf_ }
        set num_tmix 0
	set non_cntlnk_buf_ [expr $cntlnk_buf_]
    }

    set test_tcp [$traffic_ set test_tcp_]
    # show verbose
    if { $verbose_== 1 } {
	    puts $nsoutfd_ "+++++++++++++++++++++++++++++++"
	    puts $nsoutfd_ "fixed parameter settings:"
	    puts $nsoutfd_ "+++++++++++++++++++++++++++++++"
	    puts $nsoutfd_ "  TCP:                 $test_tcp"
	    puts $nsoutfd_ "  cntlnk num:          $num_cntlnk"
	    puts $nsoutfd_ "  cntlnk bw:           $cntlnk_bw_ Mbps"
	    puts $nsoutfd_ "  cntlnk buf:          $cntlnk_buf_ KB"
	    puts $nsoutfd_ "  rtt:                 $rttp_ ms"
	    puts $nsoutfd_ "  rtt diff:            $rtt_diff_ ms"
	    puts $nsoutfd_ "  tmix flow:           $num_tmix"
	    puts $nsoutfd_ "  sim time:       	   $sim_time_ s"
	    puts $nsoutfd_ "+++++++++++++++++++++++++++++++"
	    puts $nsoutfd_ ""
	    puts $nsoutfd_ "Simulation starts..."
    } else {
        # put in simple text in order to mass data extraction.
	if { $tmix_enabled_ == 1 } {
	    if { [llength $cntlnk_bw_] > 1 } {
		puts $nsoutfd_ [format "%s %2d %6.3f  %6.3f %4d " $test_tcp $num_cntlnk [lindex $cntlnk_bw_ 0] [lindex $cntlnk_bw_ 1] $num_tmix]
	    } else { 
		puts $nsoutfd_ [format "%s %2d %6.3f %4d " $test_tcp $num_cntlnk $cntlnk_bw_ $num_tmix]
	    }
     } else {
	    puts $nsoutfd_ -nonewline [format "%s %2d %6.3f %6.1f %4d %4d %4d %4d %4d %4d " $test_tcp $num_cntlnk $cntlnk_bw_ $rttp_ $num_ftp $num_rev $rate_http $num_voice $num_streaming_fwd $num_streaming_rev]
     }
            
    }
}

# Create cntlnk like topologies, such as Dumb-Bell and Parking-Lot.
Create_topology instproc cntlnk_topology {} {
    $self instvar cntlnk_             ;# bottleneck nodes
    $self instvar cntlnk_bw_ num_cntlnk_ cntlnk_buf_ cntlnk_delay_ ; # bottleneck parameters
    $self instvar graph_ traffic_ 
    $self instvar SRC SINK QUEUE OTHERQUEUE
    set test_tcp [$traffic_ set test_tcp_] 
    $self get_tcp_params $test_tcp T
    if { [$traffic_ set useAQM_] != 0} {
        $self set_aqm_params $cntlnk_buf_
    }

    # bottleneck links
    set ns [Simulator instance]
    for { set i 0 } { $i <= $num_cntlnk_ } { incr i } {
        set cntlnk_($i) [$ns node]
    }
    for { set i 0 } { $i < $num_cntlnk_ } { incr i } {
        $ns duplex-link $cntlnk_($i) $cntlnk_([expr $i+1]) [expr $cntlnk_bw_]Mb  [expr $cntlnk_delay_]ms $QUEUE
	if { $QUEUE == "XCP" } {
	    $ns queue-limit $cntlnk_($i) $cntlnk_([expr $i+1]) [expr $cntlnk_buf_]
	    $ns queue-limit $cntlnk_([expr $i+1]) $cntlnk_($i) [expr $cntlnk_buf_]
	} else {
	    $ns queue-limit $cntlnk_($i) $cntlnk_([expr $i+1]) [expr $cntlnk_buf_]
	    $ns queue-limit $cntlnk_([expr $i+1]) $cntlnk_($i) [expr $cntlnk_buf_]
	}
        
	if { $QUEUE == "XCP" || $QUEUE == "DropTail2/VcpQueue" } {
	    set flink [$ns link $cntlnk_($i) $cntlnk_([expr $i+1])]
	    set rlink [$ns link $cntlnk_([expr $i+1])  $cntlnk_($i)] 
	    set fq [$flink queue]
	    set rq [$rlink queue]
	    $fq set-link-capacity [[$flink set link_] set bandwidth_]
	    $rq set-link-capacity [[$rlink set link_] set bandwidth_]
        }
    }

}


# tmix support implementation -common.
Create_topology instproc cntlnk_traffic_tmix {} {
    $self instvar cntlnk_ num_cntlnk_ cntlnk_bw_ cntlnk_buf_
    $self instvar QUEUE OTHERQUEUE SINK SRC
    $self instvar traffic_ sim_time_ graph_
    $self instvar nsoutfd_
    $self instvar edge_delay_ edge_bw_ core_delay_ buffer_length_ tmix_L tmix_R ; #tracefd_;# basic 
    $self instvar prefill_t_ prefill_si_ scale_ end_
    $self instvar tmix flow_groups_
    $self instvar num_tmix_flow
    
    set ns [Simulator instance]
    
    #set default TCP and AQM parameters
    set test_tcp [$traffic_ set test_tcp_] 
    set standard_tcp [$traffic_ set standard_tcp_] 
    $self get_tcp_params $test_tcp T
    if { [$traffic_ set useAQM_] != 0} {
        $self set_aqm_params $cntlnk_buf_
    }
  
    foreach f $flow_groups_ {
	set num_tmix_flow($f) [$traffic_ set num_tmix_flow_]
	set num_tmix_node($f) [$traffic_ set num_tmix_node_]
    }
    
    ######################################################################
    # Create links
    #   num_flow_by_node = Number of flows each left-hand link carries.
    #                    = Number of right-hand links
    #   (Round *up* if number of flows not divisible by  num_tmix_node)
    if { $num_tmix_node(T) > 0 } {
	set num_flow_by_node [expr ($num_tmix_flow(T) + $num_tmix_node(T)-1) / $num_tmix_node(T)]
    
	#   Check that correct number of edge bandwidths and delays have been specified
	set num_link [ expr 2 * ($num_tmix_node(T) + $num_flow_by_node)]
	set tmp [llength $edge_bw_]
	if { $num_link != $tmp } {
	    puts $nsoutfd_ "Specified $tmp edge bandwidths instead of $num_link"
	    exit
	}
	set tmp [llength $edge_delay_]
	if { $num_link != $tmp } {
	    puts $nsoutfd_ "Specified $tmp edge delays instead of $num_link"
	    exit
	}
    } else {
	# used in multiple bottleneck scenario
	set num_tmix_node(T) $num_tmix_flow(T)
	set num_flow_by_node $num_tmix_flow(T)
    }
    
    # create bottleneck links
    for { set i 0 } { $i <= $num_cntlnk_ } { incr i } {
        set cntlnk_($i) [$ns node]
    }
    for { set i 0 } { $i < $num_cntlnk_ } { incr i } {
	if { [llength $cntlnk_bw_] > 1 } {
	    #This allows an asymetric central link
	    $ns simplex-link $cntlnk_($i) $cntlnk_([expr $i+1]) \
		[lindex $cntlnk_bw_ 0]Mb  [expr $core_delay_]ms $QUEUE
	    $ns simplex-link $cntlnk_([expr $i+1]) $cntlnk_($i) \
		[lindex $cntlnk_bw_ 1]Mb  [expr $core_delay_]ms $QUEUE
	    $ns queue-limit $cntlnk_($i) $cntlnk_([expr $i+1]) \
		[lindex $cntlnk_buf_ 0]
	    $ns queue-limit $cntlnk_([expr $i+1]) $cntlnk_($i) \
		[lindex $cntlnk_buf_ 1]
	} else {	
	    $ns duplex-link $cntlnk_($i) $cntlnk_([expr $i+1]) \
		[expr $cntlnk_bw_]Mb  [expr $core_delay_]ms $QUEUE
	    $ns queue-limit $cntlnk_($i) $cntlnk_([expr $i+1]) [expr $cntlnk_buf_]
	    $ns queue-limit $cntlnk_([expr $i+1]) $cntlnk_($i) [expr $cntlnk_buf_]
	}
    }
    
    set fgi 0
    foreach f $flow_groups_  {
	# create non-bottleneck left-hand links
	for { set i 0 } { $i < $num_tmix_node($f) } { incr i } {
	    set tmix_L($f,$i) [$ns node]
	}
	# create non-bottleneck right-hand links.
	for { set i 0 } { $i < $num_flow_by_node } { incr i } {
	    set tmix_R($f,$i) [$ns node]
	}
	# Create tmix Traffic
	set debug_output($f) [$traffic_ set tmix_debug_output_] 

	set tmix_agent_type [$traffic_ set tmix_agent_type_]
	if { $tmix_agent_type == "one-way" } {
	    set tmix_pkt_size [$traffic_ set tmix_pkt_size_]
	}
    
	if { $f == "T" } {
	    $self get_tcp_params $test_tcp $f
	    set cv_name($f) [$traffic_ set tmix_cv_name_]
	} else {
	    $self get_tcp_params $standard_tcp $f
	    set cv_name($f) [$traffic_ set tmix_Std_cv_name_]
	}

	if { [ llength $cv_name($f) ] != $num_tmix_flow($f) } {
	    puts $nsoutfd_ "Specified $tmp tmix connection vectors instead of $num_tmix_flow($f)."
	    exit
	}
	
   
	if {$num_cntlnk_==1} {
	    for { set i 0 } { $i < $num_tmix_node(T) } { incr i } {
		set j [expr $i * 2]
		$ns simplex-link $tmix_L($f,$i) $cntlnk_(0) \
		    [lindex $edge_bw_ $j]Mb [lindex $edge_delay_ $j]ms $OTHERQUEUE
		$ns simplex-link $cntlnk_(0) $tmix_L($f,$i) \
		    [lindex $edge_bw_ [expr $j+1] ]Mb [lindex $edge_delay_ [expr $j+1] ]ms $OTHERQUEUE
		$ns queue-limit $tmix_L($f,$i) $cntlnk_(0) \
		    [expr [lindex $edge_bw_ $j] * [lindex $buffer_length_ 0] /8.0]
		$ns queue-limit $cntlnk_(0) $tmix_L($f,$i) \
		    [expr [lindex $edge_bw_ [expr $j+1] ] * [lindex $buffer_length_ 1] /8.0] 
	    }
        
	    # enough right-hand links to allow  num_tmix_flow  paths
	    for { set i 0 } { $i < $num_flow_by_node } { incr i } {
		set j [expr ($i + $num_tmix_node(T)) * 2]
		$ns simplex-link $tmix_R($f,$i) $cntlnk_(1) \
		    [lindex $edge_bw_ $j]Mb [lindex $edge_delay_ $j]ms $OTHERQUEUE
		$ns simplex-link $cntlnk_(1) $tmix_R($f,$i) \
		    [lindex $edge_bw_ [expr $j+1] ]Mb [lindex $edge_delay_ [expr $j+1] ]ms $OTHERQUEUE
		$ns queue-limit $tmix_R($f,$i) $cntlnk_(1) \
		    [expr [lindex $edge_bw_ $j] * [lindex $buffer_length_ 1] /8.0]
		$ns queue-limit $cntlnk_(1) $tmix_R($f,$i) \
		    [expr [lindex $edge_bw_ [expr $j+1]] * [lindex $buffer_length_ 0] /8.0]
	    }
	
	    # Create  tmix  generators
	    for { set j 0 } { $j < $num_tmix_flow(T) } { incr j } {
		set right_node [ expr $j / $num_tmix_node(T) ]
		set left_node  [ expr $j - $num_tmix_node(T)*$right_node ]
		set tmix($f,$j) [new Tmix]
		$tmix($f,$j) set-agent-type $tmix_agent_type
		if { $tmix_agent_type == "one-way" } {
		    $tmix($f,$j) set-sink $SINK($f)
		    $tmix($f,$j) set-pkt-size $tmix_pkt_size
		    $tmix($f,$j) check-oneway-closed
		}
		$tmix($f,$j) set-init $tmix_L($f,$left_node);  # tmix_L as initiator
		$tmix($f,$j) set-acc  $tmix_R($f,$right_node); # tmix_R as acceptor
		$tmix($f,$j) set-ID [expr $j+10+ $fgi*$num_tmix_flow(T)]
		set starttmix 1
		if { [file size [lindex $cv_name($f) $j] ] < 1 } {
		    puts stderr "Warning [lindex $cv_name($f) $j] is empty"
		    set starttmix 0
		} elseif { [catch {$tmix($f,$j) set-cvfile [lindex $cv_name($f) $j]} ] } {
		    #skip this one as file is empty
		    puts stderr "Warning [lindex $cv_name($f) $j] open error"
		    set starttmix 0
		}
		$tmix($f,$j) set-TCP $SRC($f)
		$tmix($f,$j) set-prefill-t [lindex $prefill_t_ $j]
		$tmix($f,$j) set-prefill-si [lindex $prefill_si_ $j]
		if { [llength $debug_output($f)] > 0 } {
		    $tmix($f,$j) set-outfile $f[lindex $debug_output($f) $j]
		}
		$tmix($f,$j) set-debug 0
		$tmix($f,$j) set-scale [lindex $scale_ $j]
		$tmix($f,$j) set-end $end_
		if {$starttmix} {
		    $ns at 0.0 "$tmix($f,$j) start"
		    $ns at $sim_time_ "$tmix($f,$j) stop"
		    if { [llength $debug_output($f)] > 0 } {
			for { set s 0 } {$s < $sim_time_} { incr s } {
			    set tmp $tmix($f,$j)
			    $ns at $s "$tmp active-connections"
			    $ns at $s "$tmp total-connections"
			    $ns at $s "$tmp active-bursts"
			} 
		    }
		}
	    }	
	} else {
	    # multiple bottleneck
	    set i 0
	    set j 0
	    $ns simplex-link $tmix_L($f,$i) $cntlnk_($i) \
		[lindex $edge_bw_ $j]Mb [lindex $edge_delay_ $j]ms $OTHERQUEUE
	    $ns simplex-link $cntlnk_($i) $tmix_L($f,$i) \
		[lindex $edge_bw_ [expr $j+1] ]Mb [lindex $edge_delay_ [expr $j+1] ]ms $OTHERQUEUE
	    $ns queue-limit $tmix_L($f,$i) $cntlnk_($i) \
		[expr [lindex $edge_bw_ $j] * [lindex $buffer_length_ 0] /8.0]
	    $ns queue-limit $cntlnk_($i) $tmix_L($f,$i) \
		[expr [lindex $edge_bw_ [expr $j+1] ] * [lindex $buffer_length_ 0] /8.0] 
	
	    set j [expr ($num_cntlnk_ + $num_tmix_flow(T)) * 2]
	    $ns simplex-link $tmix_R($f,$i) $cntlnk_($num_cntlnk_) \
		[lindex $edge_bw_ $j]Mb [lindex $edge_delay_ $j]ms $OTHERQUEUE
	    $ns simplex-link $cntlnk_($num_cntlnk_) $tmix_R($f,$i) \
		[lindex $edge_bw_ [expr $j+1] ]Mb [lindex $edge_delay_ [expr $j+1] ]ms $OTHERQUEUE
	    $ns queue-limit $tmix_R($f,$i) $cntlnk_($num_cntlnk_) \
		[expr [lindex $edge_bw_ $j] * [lindex $buffer_length_ 0] /8.0]
	    $ns queue-limit $cntlnk_($num_cntlnk_) $tmix_R($f,$i) \
		[expr [lindex $edge_bw_ [expr $j+1]] * [lindex $buffer_length_ 0] /8.0]
	
	    # tmix traffic setup
	    set tmix($f,$i) [new Tmix]
	    $tmix($f,$i) set-agent-type $tmix_agent_type
	    if { $tmix_agent_type == "one-way" } {
		$tmix($f,$i) set-sink $SINK($f)
		$tmix($f,$i) set-pkt-size $tmix_pkt_size
		$tmix($f,$i) check-oneway-closed
	    }
	    $tmix($f,$i) set-init $tmix_L($f,$i);                 # tmix_L as initiator
	    $tmix($f,$i) set-acc $tmix_R($f,$i);                  # tmix_R as acceptor
	    $tmix($f,$i) set-ID [expr $i+7+$fgi*$num_tmix_flow(T)]
	    set starttmix 1
	    if { [file size [lindex $cv_name($f) $i] ] < 1 } {
		puts stderr "Warning [lindex $cv_name($f) $i] is empty"
		set starttmix 0
	    } elseif { [catch {$tmix($f,$i) set-cvfile [lindex $cv_name($f) $i]} ] } {
		#skip this one as file is empty
		puts stderr "Warning [lindex $cv_name($f) $i] open error"
		set starttmix 0
	    }
	    $tmix($f,$i) set-TCP $SRC($f)
	    $tmix($f,$i) set-prefill-t [lindex $prefill_t_ $i]
	    $tmix($f,$i) set-prefill-si [lindex $prefill_si_ $i]
	    if { [llength $debug_output($f)] > 0 } {
		$tmix($f,$i) set-outfile $f[lindex $debug_output($f) $i]
	    }
	    $tmix($f,$i) set-debug 0
	    $tmix($f,$i) set-scale [lindex $scale_ $i]
	    $tmix($f,$i) set-end $end_
	    if {$starttmix} {
		$ns at 0.0 "$tmix($f,$i) start"
		$ns at $sim_time_ "$tmix($f,$i) stop"
		if { [llength $debug_output($f)] > 0 } {
		    for { set s 0 } {$s < $sim_time_} { incr s } {
			set tmp $tmix($f,$i)
			$ns at $s "$tmp active-connections"
			$ns at $s "$tmp total-connections"
			$ns at $s "$tmp active-bursts"
		    } 
		}
	    }
	    
	    for { set cntlnk_i 0 } { $cntlnk_i < $num_cntlnk_ } { incr cntlnk_i } {
		set i [expr $cntlnk_i + 1]
		set j [expr ($i + $cntlnk_i) * 2]
		$ns simplex-link $tmix_L($f,$i) $cntlnk_($cntlnk_i) \
		    [lindex $edge_bw_ $j]Mb [lindex $edge_delay_ $j]ms $OTHERQUEUE
		$ns simplex-link $cntlnk_($cntlnk_i) $tmix_L($f,$i) \
		    [lindex $edge_bw_ [expr $j+1] ]Mb [lindex $edge_delay_ [expr $j+1] ]ms $OTHERQUEUE
		$ns queue-limit $tmix_L($f,$i) $cntlnk_($cntlnk_i) \
		    [expr [lindex $edge_bw_ $j] * [lindex $buffer_length_ 0] /8.0]
		$ns queue-limit $cntlnk_($cntlnk_i) $tmix_L($f,$i) \
		    [expr [lindex $edge_bw_ [expr $j+1] ] * [lindex $buffer_length_ 0] /8.0] 
		
		set j [expr $i + 2]
		$ns simplex-link $tmix_R($f,$i) $cntlnk_($i) \
		    [lindex $edge_bw_ $j]Mb [lindex $edge_delay_ $j]ms $OTHERQUEUE
		$ns simplex-link $cntlnk_($i) $tmix_R($f,$i) \
		    [lindex $edge_bw_ [expr $j+1] ]Mb [lindex $edge_delay_ [expr $j+1] ]ms $OTHERQUEUE
		$ns queue-limit $tmix_R($f,$i) $cntlnk_($i) \
		    [expr [lindex $edge_bw_ $j] * [lindex $buffer_length_ 0] /8.0]
		$ns queue-limit $cntlnk_($i) $tmix_R($f,$i) \
		    [expr [lindex $edge_bw_ [expr $j+1]] * [lindex $buffer_length_ 0] /8.0]
		
		# tmix traffic setup
		set tmix($f,$i) [new Tmix]
		$tmix($f,$i) set-agent-type $tmix_agent_type
		if { $tmix_agent_type == "one-way" } {
		    $tmix($f,$i) set-sink $SINK($f)
		    $tmix($f,$i) set-pkt-size $tmix_pkt_size
		    $tmix($f,$i) check-oneway-closed
		}
		$tmix($f,$i) set-init $tmix_L($f,$i);                 # tmix_L as initiator
		$tmix($f,$i) set-acc $tmix_R($f,$i);                  # tmix_R as acceptor
		$tmix($f,$i) set-ID [expr $i+7]
		set starttmix 1
		if { [file size [lindex $cv_name($f) $i] ] < 1 } {
		    puts stderr "Warning [lindex $cv_name($f) $i] is empty"
		    set starttmix 0
		} elseif { [catch {$tmix($f,$i) set-cvfile [lindex $cv_name($f) $i]} ] } {
		    #skip this one as file is empty
		    puts stderr "Warning [lindex $cv_name($f) $i] open error"
		    set starttmix 0
		}
		$tmix($f,$i) set-TCP $SRC($f)
		$tmix($f,$i) set-prefill-t [lindex $prefill_t_ $i]
		$tmix($f,$i) set-prefill-si [lindex $prefill_si_ $i]
		if { [llength $debug_output($f)] > 0 } {
		    $tmix($f,$i) set-outfile $f[lindex $debug_output($f) $i]
		}
		$tmix($f,$i) set-debug 0
		$tmix($f,$i) set-scale [lindex $scale_ $i]
		$tmix($f,$i) set-end $end_
		if {$starttmix} {
		    $ns at 0.0 "$tmix($f,$i) start"
		    $ns at $sim_time_ "$tmix($f,$i) stop"
		    if { [llength $debug_output($f)] > 0 } {
			for { set s 0 } {$s < $sim_time_} { incr s } {
			    set tmp $tmix($f,$i)
			    $ns at $s "$tmp active-connections"
			    $ns at $s "$tmp total-connections"
			    $ns at $s "$tmp active-bursts"
			} 
		    }
		}
	    }
	}
	incr fgi
    }
}

# Create basic.
Create_topology/Dumb_bell/Basic instproc cntlnk_topology_basic {} {
    $self instvar cntlnk_             ;# bottleneck nodes
    $self instvar cntlnk_bw_ num_cntlnk_ cntlnk_buf_ cntlnk_delay_ ; # bottleneck parameters
    $self instvar graph_ traffic_ 
    $self instvar SRC SINK QUEUE OTHERQUEUE
    $self instvar edge_delay_ edge_bw_ core_delay_ buffer_length_ ;# basic
    set test_tcp [$traffic_ set test_tcp_] 
    $self get_tcp_params $test_tcp T
    
    set cntlnk_buf_ [expr $cntlnk_bw_ * $buffer_length_ / 8.0]  ;# in 1KB pkt
    
    if { [$traffic_ set useAQM_] != 0} {
        $self set_aqm_params $cntlnk_buf_
    }
    
    # bottleneck links
    set ns [Simulator instance]
    for { set i 0 } { $i <= $num_cntlnk_ } { incr i } {
        set cntlnk_($i) [$ns node]
    }
    for { set i 0 } { $i < $num_cntlnk_ } { incr i } {
        $ns duplex-link $cntlnk_($i) $cntlnk_([expr $i+1]) [expr $cntlnk_bw_]Mb  [expr $core_delay_]ms $QUEUE
        $ns queue-limit $cntlnk_($i) $cntlnk_([expr $i+1]) [expr $cntlnk_buf_]
        $ns queue-limit $cntlnk_([expr $i+1]) $cntlnk_($i) [expr $cntlnk_buf_]
    }

}

# create basic wireless.
Create_topology/Dumb_bell/Basic instproc cntlnk_wireless_basic {} {

    $self instvar cntlnk_ num_cntlnk_ cntlnk_bw_ cntlnk_buf_
    $self instvar traffic_ sim_time_ graph_ 
    $self instvar edge_delay_ edge_bw_ core_delay_ buffer_length_ racefd_;# basic
    $self instvar mobile tmix_L tmix_R
    $self instvar SRC SINK QUEUE OTHERQUEUE
    $self instvar prefill_t_ prefill_si_ scale_ end_
    $self instvar if_wireless_ w_datarate_ tmix
    
    set ns [Simulator instance]
    set test_tcp [$traffic_ set test_tcp_] 
    $self get_tcp_params $test_tcp T
    if { [$traffic_ set useAQM_] != 0} {
        $self set_aqm_params $cntlnk_buf_
    }

    # this needs to be set even if it isn't used
    # because of a hack in lib/ns-mobilenode.tcl
    if { [$ns get-ns-traceall] == "" } {
	set tracefd_ [open /dev/null w]
	$ns trace-all $tracefd_
    }


    set num_tmix_flow(T) [$traffic_ set num_tmix_flow_]
    set num_tmix_node(T) [$traffic_ set num_tmix_node_]
    set cv_name [$traffic_ set tmix_cv_name_]
    set debug_output [$traffic_ set tmix_debug_output_] 
    
    set tmix_agent_type [$traffic_ set tmix_agent_type_]
    if { $tmix_agent_type == "one-way" } {
	set tmix_pkt_size [$traffic_ set tmix_pkt_size_]
    }
    
   
    # set up for hierarchical routing
    $ns node-config -addressType hierarchical

    #set defaults
    Phy/WirelessPhyExt set freq_ 2.447e9
    Phy/WirelessPhyExt set BasicModulationScheme_ 0
    Mac/802_11Ext set BasicModulationScheme_ 0
    Antenna/OmniAntenna set Y_ 0
    Antenna/OmniAntenna set Z_ 1.5
    Antenna/OmniAntenna set Gt_ 1.0
    Antenna/OmniAntenna set Gr_ 1.0

    # wireless start, part from ~/tcl/ex/wired-cum-wireless.tcl
    set opt(chan)       Channel/WirelessChannel
    set opt(prop)       Propagation/TwoRayGround
    set opt(netif)      Phy/WirelessPhyExt
    set opt(mac)        Mac/802_11Ext
    set opt(ifq)        Queue/DropTail/PriQueue
    set opt(ll)         LL
    set opt(ant)        Antenna/OmniAntenna
    set opt(x)          100   
    set opt(y)          100   
    set opt(ifqlen)     [expr $w_datarate_ * [lindex $buffer_length_ 0] / 8.0 / 1.5]
    set opt(adhocRouting)  DSDV                     
    #set opt(adhocRouting)  DumbAgent
    if { $if_wireless_ == 3 } {
	#   Ts...            _Td 
	#       :           /
	#   Ts..BsC0------C1---Td
	#       :           \_
	#   Ts..:             Td
	AddrParams set domain_num_ 2
	# Ts and Bs clusters
	lappend cluster_num 1
	# Td and C1 clusters
	lappend cluster_num [expr $num_tmix_node(T) + 1]            
	AddrParams set cluster_num_ $cluster_num
	# Ts and Bs
	lappend eilastlevel [expr $num_tmix_node(T) + 1]
	# C1
	lappend eilastlevel 1
	# Td nodes
	for { set i 0} { $i < $num_tmix_node(T)} {incr i} {
	    lappend eilastlevel 1
	}
	AddrParams set nodes_num_ $eilastlevel 
	
	# create wired nodes
	set domain 1
	set cluster 0
	set cntlnk_(1) [$ns node $domain.$cluster.0]
	incr cluster
	# set td cluster
	for { set c $cluster } { $c < [expr $num_tmix_node(T) + $cluster]} { incr c } {
	    set tmix_R([expr $c - $cluster]) [$ns node $domain.$c.0]
	}
	# connect wired nodes to link (right side)
	for { set i 0 } { $i < $num_tmix_node(T) } { incr i } {
	    set j [expr ($i + $num_tmix_node(T)) * 2]
	    $ns simplex-link $tmix_R($i) $cntlnk_(1) \
		[lindex $edge_bw_ $j]Mb [lindex $edge_delay_ $j]ms $OTHERQUEUE
	    $ns simplex-link $cntlnk_(1) $tmix_R($i) \
		[lindex $edge_bw_ [expr $j+1] ]Mb [lindex $edge_delay_ [expr $j+1] ]ms $OTHERQUEUE
	    $ns queue-limit $tmix_R($i) $cntlnk_(1) \
		[expr [lindex $edge_bw_ $j] * [lindex $buffer_length_ 0] /8.0]
	    $ns queue-limit $cntlnk_(1) $tmix_R($i) \
	    [expr [lindex $edge_bw_ [expr $j+1] ] * [lindex $buffer_length_ 1] /8.0] 
	}
	
	set topo   [new Topography]
	$topo load_flatgrid $opt(x) $opt(y)
	create-god [expr  $num_tmix_node(T)+1]
	
	set channel [new $opt(chan)] 
	
	$ns node-config \
	    -adhocRouting $opt(adhocRouting) \
	    -llType $opt(ll) \
	    -macType $opt(mac) \
	    -ifqType $opt(ifq) \
	    -ifqLen $opt(ifqlen) \
	    -antType $opt(ant) \
	    -propInstance [new $opt(prop)] \
	    -phyType $opt(netif) \
	    -channel $channel \
	    -topoInstance $topo \
	    -wiredRouting ON \
	    -agentTrace OFF \
	    -routerTrace ON \
	    -macTrace OFF

	# configure for wirelessnodes
	set cntlnk_(0) [$ns node 0.0.0]
	$cntlnk_(0) random-motion 0 ; # AP setting 
	$cntlnk_(0) set X_ 0.0
	$cntlnk_(0) set Y_ 0.0
	$cntlnk_(0) set Z_ 1.5
	# define the cntlnk 
	for { set i 0 } { $i < $num_cntlnk_ } { incr i } {
	    $ns simplex-link $cntlnk_($i) $cntlnk_([expr $i+1]) \
		[lindex $cntlnk_bw_ 0]Mb  [expr $core_delay_]ms $QUEUE
	    $ns simplex-link $cntlnk_([expr $i+1]) $cntlnk_($i) \
		[lindex $cntlnk_bw_ 1]Mb  [expr $core_delay_]ms $QUEUE
	    $ns queue-limit $cntlnk_($i) $cntlnk_([expr $i+1]) [lindex $cntlnk_buf_ 0]
	    $ns queue-limit $cntlnk_([expr $i+1]) $cntlnk_($i) [lindex $cntlnk_buf_ 1]
	}
	$ns node-config -wiredRouting OFF
	if {$num_tmix_node(T) == 3} {
	    set XX [list 5.0 3.5 0.0]
	    set YY [list 0.0 3.5 5.0]
	} else {
	    set XX [list]
	    set YY [list]
	    for { set i 0 } { $i < $num_tmix_node(T) } { incr i } { lappend XX $i }
	    for { set i $num_tmix_node(T) } { $i > 0 } { incr i -1 } { lappend YY $i }
	}
	for { set i 0 } { $i < $num_tmix_node(T) } { incr i } {
	    set tmix_L($i) [$ns node 0.0.[expr $i+1]]
	    $tmix_L($i) base-station [AddrParams addr2id [$cntlnk_(0) node-addr]]
#	    $ns initial_node_pos $tmix_L($i) 15
	    #adjust link layer delay to implement topology network delay
	    set ll [$tmix_L($i) set ll_(0)]
	    #set the delay throuth the ll layer to be t seconds (bidirectional delay)
	    set j [expr $i * 2]
	    $tmix_L($i) set X_ [lindex $XX $i]
	    $tmix_L($i) set Y_ [lindex $YY $i]
	    $tmix_L($i) set Z_ 1.5
	    $ll set delay_ [expr [lindex $edge_delay_ $j] / 1000.0]
	    incr wirelessdomain
	}
	
    }
    if { $if_wireless_ == 2 } {
	#   Ts....Bs0            _Td 
	#            \          /
	#   Ts...Bs1--C0------C1---Td
	#            /          \_
	#   Ts....Bs2             Td
	AddrParams set domain_num_ [expr $num_tmix_node(T) + 2]
	# Ts and Bs clusters
	for { set i 0} { $i < $num_tmix_node(T)} {incr i} {
	    lappend cluster_num 1
	}
	# Td and C clusters
	lappend cluster_num 1           
	lappend cluster_num [expr $num_tmix_node(T) + 1]            
	AddrParams set cluster_num_ $cluster_num
	# Ts and Bs
	for { set i 0} { $i < $num_tmix_node(T)} {incr i} {
	    lappend eilastlevel 2
	}
	# C0
	lappend eilastlevel 1
	# C1
	lappend eilastlevel 1
	# Td nodes
	for { set i 0} { $i < $num_tmix_node(T)} {incr i} {
	    lappend eilastlevel 1
	}
	AddrParams set nodes_num_ $eilastlevel 
	
	# create wired nodes
	set domain 3
	set cluster 0
	set cntlnk_(0) [$ns node $domain.$cluster.0]
	incr domain
	set cntlnk_(1) [$ns node $domain.$cluster.0]
	incr cluster
	# set td cluster
	for { set c $cluster } { $c < [expr $num_tmix_node(T) + $cluster]} { incr c } {
	    set tmix_R([expr $c - $cluster]) [$ns node $domain.$c.0]
	}
	# connect wired nodes to link (right side)
	for { set i 0 } { $i < $num_tmix_node(T) } { incr i } {
	    set j [expr ($i + $num_tmix_node(T)) * 2]
	    $ns simplex-link $tmix_R($i) $cntlnk_(1) \
		[lindex $edge_bw_ $j]Mb [lindex $edge_delay_ $j]ms $OTHERQUEUE
	    $ns simplex-link $cntlnk_(1) $tmix_R($i) \
		[lindex $edge_bw_ [expr $j+1] ]Mb [lindex $edge_delay_ [expr $j+1] ]ms $OTHERQUEUE
	    $ns queue-limit $tmix_R($i) $cntlnk_(1) \
		[expr [lindex $edge_bw_ $j] * [lindex $buffer_length_ 0] /8.0]
	    $ns queue-limit $cntlnk_(1) $tmix_R($i) \
	    [expr [lindex $edge_bw_ [expr $j+1] ] * [lindex $buffer_length_ 1] /8.0] 
	}
	# define the cntlnk 
	for { set i 0 } { $i < $num_cntlnk_ } { incr i } {
	    $ns simplex-link $cntlnk_($i) $cntlnk_([expr $i+1]) [lindex $cntlnk_bw_ 0]Mb  [expr $core_delay_]ms $QUEUE
	    $ns simplex-link $cntlnk_([expr $i+1]) $cntlnk_($i) [lindex $cntlnk_bw_ 1]Mb  [expr $core_delay_]ms $QUEUE
	    $ns queue-limit $cntlnk_($i) $cntlnk_([expr $i+1]) [lindex $cntlnk_buf_ 0]
	    $ns queue-limit $cntlnk_([expr $i+1]) $cntlnk_($i) [lindex $cntlnk_buf_ 1]
	}
	# wireless start, part from ~/tcl/ex/wired-cum-wireless.tcl
	
	set topo   [new Topography]
	$topo load_flatgrid $opt(x) $opt(y)
	create-god [expr  $num_tmix_node(T)*2]
	
	set channel [new $opt(chan)] 
	
	$ns node-config \
	    -adhocRouting $opt(adhocRouting) \
	    -llType $opt(ll) \
	    -macType $opt(mac) \
	    -ifqType $opt(ifq) \
	    -ifqLen $opt(ifqlen) \
	    -antType $opt(ant) \
	    -propInstance [new $opt(prop)] \
	    -phyType $opt(netif) \
	    -channel $channel \
	    -topoInstance $topo \
	    -wiredRouting ON \
	    -agentTrace OFF \
	    -routerTrace ON \
	    -macTrace OFF

	# configure for wirelessnodes
	for { set i 0 } { $i < $num_tmix_node(T) } { incr i } {
	    set Bs($i) [$ns node $i.0.0]
	    $Bs($i) random-motion 0 ; # AP setting 
	    $Bs($i) set X_ 0.0
	    $Bs($i) set Y_ 0.0
	    $Bs($i) set Z_ 1.5
	}
	$ns node-config -wiredRouting OFF
	for { set i 0 } { $i < $num_tmix_node(T) } { incr i } {
	    set tmix_L($i) [$ns node $i.0.1]
	    $tmix_L($i) base-station [AddrParams addr2id [$Bs($i) node-addr]]
#	    $ns initial_node_pos $tmix_L($i) 15
	    incr wirelessdomain
	}
	
	#connect Bs and C0 wired links
	for { set i 0 } { $i < $num_tmix_node(T) } { incr i } {
	    set j [expr $i * 2]
	    $ns simplex-link $Bs($i) $cntlnk_(0) \
		[lindex $edge_bw_ $j]Mb [lindex $edge_delay_ $j]ms $QUEUE
	    $ns simplex-link $cntlnk_(0) $Bs($i) \
		[lindex $edge_bw_ [expr $j+1] ]Mb [lindex $edge_delay_ [expr $j+1] ]ms $QUEUE
	    $ns queue-limit  $Bs($i) $cntlnk_(0) \
		[expr [lindex $edge_bw_ $j] * [lindex $buffer_length_ 0] /8.0]
	    $ns queue-limit $cntlnk_(0) $Bs($i) \
		[expr [lindex $edge_bw_ [expr $j+1] ] * [lindex $buffer_length_ 1] /8.0] 
	}
    }
    if { $if_wireless_ == 1 } {
	# * each wired node (Ts) that connects to a mobile
	#   node has its own domain (one cluster with one node)
	# * the base station(C0) and mobiles are a domain of
	#   one cluster with (num_tmix_flows + 1) nodes
	# * the central link to wired nodes (Td) is a domain
	#   of (num_tmix_flows + 1) clusters of one node
	#
	#
	#   Ts---M.              _Td 
	#          :            /
	#   Ts---M.:.BsC0-----C1---Td
	#          :            \_
	#   Ts---M.               Td
	#
	AddrParams set domain_num_ [expr $num_tmix_node(T) + 2]
	# Ts clusters
	#set rtproto Manual
	for { set i 0} { $i < $num_tmix_node(T)} {incr i} {
	    lappend cluster_num 1
	}
	# M and Bs cluster
	lappend cluster_num 1
	# Td and C1 cluster
	lappend cluster_num [expr $num_tmix_node(T) + 1]            
	AddrParams set cluster_num_ $cluster_num
	# Ts nodes
	for { set i 0} { $i < $num_tmix_node(T)} {incr i} {
	    lappend eilastlevel 1
	}
	# M and Bs
	lappend eilastlevel [expr $num_tmix_node(T) + 1]
	# C1
	lappend eilastlevel 1
	# Td nodes
	for { set i 0} { $i < $num_tmix_node(T)} {incr i} {
	    lappend eilastlevel 1
	}
	AddrParams set nodes_num_ $eilastlevel 
	
	# create wired nodes
	set domain 0
	for { set d $domain } { $d < $num_tmix_node(T) } { incr d } {
	    set tmix_L($d) [$ns node $d.0.0]
	}
	set wirelessdomain $d
	# skip wireless domain
	incr d
	set cntlnk_(1) [$ns node $d.0.0]
	# set td cluster
	for { set c 1 } { $c <= $num_tmix_node(T) } { incr c } {
	    set tmix_R([expr $c - 1]) [$ns node $d.$c.0]
	}
	
	# connect wired nodes to link (right side)
	for { set i 0 } { $i < $num_tmix_node(T) } { incr i } {
	    set j [expr ($i + $num_tmix_node(T)) * 2]
	    $ns simplex-link $tmix_R($i) $cntlnk_(1) \
		[lindex $edge_bw_ $j]Mb [lindex $edge_delay_ $j]ms $OTHERQUEUE
	    $ns simplex-link $cntlnk_(1) $tmix_R($i) \
		[lindex $edge_bw_ [expr $j+1] ]Mb [lindex $edge_delay_ [expr $j+1] ]ms $OTHERQUEUE
	    $ns queue-limit $tmix_R($i) $cntlnk_(1) \
		[expr [lindex $edge_bw_ $j] * [lindex $buffer_length_ 0] /8.0]
	    $ns queue-limit $cntlnk_(1) $tmix_R($i) \
		[expr [lindex $edge_bw_ [expr $j+1] ] * [lindex $buffer_length_ 1] /8.0] 
	}
	
	# wireless start, part from ~/tcl/ex/wired-cum-wireless.tcl
	set opt(ifqlen)     $QUEUE   
	#set opt(adhocRouting)  AODV                      
	#set opt(adhocRouting)  DumbAgent
	set opt(adhocRouting)  DSDV
	
	set topo   [new Topography]
	$topo load_flatgrid $opt(x) $opt(y)
	create-god [expr  $num_tmix_node(T)+1]
	
	$ns node-config \
	    -adhocRouting $opt(adhocRouting) \
	    -llType $opt(ll) \
	    -macType $opt(mac) \
	    -ifqType $opt(ifq) \
	    -ifqLen $opt(ifqlen) \
	    -antType $opt(ant) \
	    -propInstance [new $opt(prop)] \
	    -phyType $opt(netif) \
	    -channel [new $opt(chan)] \
	    -topoInstance $topo \
	    -wiredRouting ON \
	    -agentTrace OFF \
	    -routerTrace ON \
	    -macTrace OFF
	
	
	set cntlnk_(0) [$ns node $wirelessdomain.0.0]
	$cntlnk_(0) random-motion 0 ; # AP setting 
	$cntlnk_(0) set X_ 0.0
	$cntlnk_(0) set Y_ 0.0
	$cntlnk_(0) set Z_ 1.5
	# define the cntlnk 
	for { set i 0 } { $i < $num_cntlnk_ } { incr i } {
	    $ns simplex-link $cntlnk_($i) $cntlnk_([expr $i+1]) [lindex $cntlnk_bw_ 0]Mb  [expr $core_delay_]ms $QUEUE
	    $ns simplex-link $cntlnk_([expr $i+1]) $cntlnk_($i) [lindex $cntlnk_bw_ 1]Mb  [expr $core_delay_]ms $QUEUE
	    $ns queue-limit $cntlnk_($i) $cntlnk_([expr $i+1]) [lindex $cntlnk_buf_ 0]
	    $ns queue-limit $cntlnk_([expr $i+1]) $cntlnk_($i) [lindex $cntlnk_buf_ 1]
	}
	
	# configure for mobilenodes
	#   $ns node-config -wiredRouting OFF
	for { set i 0 } { $i < $num_tmix_node(T) } { incr i } {
	    set mobile($i) [$ns node $wirelessdomain.0.[expr $i+1]]
	    $mobile($i) base-station [AddrParams addr2id [$cntlnk_(0) node-addr]]
#	    $ns initial_node_pos $mobile($i) 15
	}
	#connect Ts and M wired links
	for { set i 0 } { $i < $num_tmix_node(T) } { incr i } {
	    set j [expr $i * 2]
	    $ns simplex-link $tmix_L($i) $mobile($i) \
		[lindex $edge_bw_ $j]Mb [lindex $edge_delay_ $j]ms $QUEUE
	    $ns simplex-link $mobile($i) $tmix_L($i) \
		[lindex $edge_bw_ [expr $j+1] ]Mb [lindex $edge_delay_ [expr $j+1] ]ms $QUEUE
	    $ns queue-limit $tmix_L($i) $mobile($i) \
		[expr [lindex $edge_bw_ $j] * [lindex $buffer_length_ 0] /8.0]
	    $ns queue-limit $mobile($i) $tmix_L($i) \
		[expr [lindex $edge_bw_ [expr $j+1] ] * [lindex $buffer_length_ 1] /8.0] 
	    for {set r 0 } { $r < $num_tmix_node(T) } { incr r } {
		$mobile($i) add-route [AddrParams addr2id [$tmix_R($r) node-addr]] [$mobile($i) set ragent_]
	    }
	}
    }

    set cv_name [$traffic_ set tmix_cv_name_]
    for { set i 0 } { $i < $num_tmix_flow(T) } { incr i } {
	set right_node [ expr $i / $num_tmix_node(T) ]
	set left_node  [ expr $i - $num_tmix_node(T)*$right_node ]

	set tmix(T,$i) [new Tmix]
	$tmix(T,$i) set-agent-type $tmix_agent_type
	if { $tmix_agent_type == "one-way" } {
	    $tmix(T,$i) set-sink $SINK(T)
	    $tmix(T,$i) set-pkt-size $tmix_pkt_size
	    $tmix(T,$i) check-oneway-closed
	}
	$tmix(T,$i) set-init $tmix_L($left_node);                 # tmix_L as initiator
	$tmix(T,$i) set-acc $tmix_R($right_node);                  # tmix_R as acceptor
	$tmix(T,$i) set-ID [expr $i+7]
	set starttmix 1
	if { [file size [lindex $cv_name $i] ] < 1 } {
	    puts stderr "Warning [lindex $cv_name $i] is empty"
	    set starttmix 0
	} elseif { [catch {$tmix(T,$i) set-cvfile [lindex $cv_name $i]} ] } {
	    #skip this one as file is empty
	    puts stderr "Warning [lindex $cv_name $i] open error"
	    set starttmix 0
	}
	$tmix(T,$i) set-TCP $SRC(T)
	$tmix(T,$i) set-prefill-t [lindex $prefill_t_ $i]
	$tmix(T,$i) set-prefill-si [lindex $prefill_si_ $i]
	if { [llength $debug_output] > 0 } {
	    $tmix(T,$i) set-outfile [lindex $debug_output $i]
	}
	$tmix(T,$i) set-debug 0
	$tmix(T,$i) set-scale [lindex $scale_ $i]
	$tmix(T,$i) set-end $end_
	if {$starttmix} {
	    $ns at 0.0 "$tmix(T,$i) start"
	    $ns at $sim_time_ "$tmix(T,$i) stop"
	    if { [llength $debug_output] > 0 } {
		for { set s 0 } {$s < $sim_time_} { incr s } {
		    set tmp $tmix(T,$i)
		    $ns at $s "$tmp active-connections"
		    $ns at $s "$tmp total-connections"
		    $ns at $s "$tmp active-bursts"
		} 
	    }
	}
    }
}


# End setting topology and traffic
# -----------------------------------------------


Create_topology/Dumb_bell instproc init args {
    eval $self next $args
}

Create_topology/Dumb_bell instproc create {} {             
    $self instvar num_cntlnk_
    set num_cntlnk_ 1
    eval $self set_parameters "Dumb_bell" $num_cntlnk_
    eval $self create_dumb_bell
}

# Create one bottleneck Dumb-Bell scenario
Create_topology/Dumb_bell instproc create_dumb_bell {} {
    $self instvar traffic_ graph_
    $self instvar sim_time_ neglect_time_
    set neglect_time_ [expr $sim_time_ * 0.2 ]
    
    # Start setting topology and traffic
    
    eval $self cntlnk_topology
    
    # Set traffic

    if { [ $traffic_ set num_ftp_flow_fwd_] > 0 || [  $traffic_ set num_ftp_flow_rev_ ] > 0  } {
        eval $self cntlnk_traffic_ftp
    }
    
    if { [$traffic_ set rate_http_flow_ ] > 0 } {
        eval $self cntlnk_traffic_http
    }
    
    if { [$traffic_ set num_voice_flow_] > 0 } {
        eval $self cntlnk_traffic_voice
    }
    
    if { [$traffic_ set num_streaming_flow_fwd_] > 0 || [$traffic_ set num_streaming_flow_rev_] > 0 } {
        eval $self cntlnk_traffic_streaming
    }

}

Create_topology/Dumb_bell/Basic instproc init args {
    eval $self next $args
}

Create_topology/Dumb_bell/Basic instproc create {} {             
    $self instvar num_cntlnk_ tmix_enabled_
    set tmix_enabled_ 1
    eval $self set_parameters "Dumb_bell" $num_cntlnk_
    eval $self create_dumb_bell
}

# Create basic scenario
Create_topology/Dumb_bell/Basic instproc create_dumb_bell {} {
    $self instvar traffic_ graph_
    $self instvar sim_time_ neglect_time_
    $self instvar if_wireless_
    set neglect_time_ [expr $sim_time_ * 0.2 ]
    
    if { $if_wireless_ >= 1 } {
        # tmix wireless part
        eval $self cntlnk_wireless_basic 
    } else {
	eval $self cntlnk_traffic_tmix
    }
}



# The finish routine
Create_topology instproc finish {} {
}
	
