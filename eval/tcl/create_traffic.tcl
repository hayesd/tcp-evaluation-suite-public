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
# Revised and updated
# (see http://tools.ietf.org/html/draft-irtf-tmrg-tests-03>)
# - David Hayes (dahayes@swin.edu.au or david.hayes@ieee.org)
# - Dongxia Xu
#
#
# Released under the GNU General Public License version 2.
#
#
# $Id: create_traffic.tcl,v 1.4 2008/11/03 06:22:49 wanggang Exp $
#
# This code creates the traffic settings with settable parameters.
#

Class Create_traffic

# Traffic class

Create_traffic instproc init args {
    $self instvar num_tmix_flow_           ;# num of tmix flows
    $self instvar num_tmix_node_           ;# num of links on left of bottleneck
    $self instvar tmix_cv_name_            ;# tmix connection vector name
    $self instvar tmix_Std_cv_name_        ;# tmix connection vector name for Std tests
    $self instvar tmix_agent_type_	   ;# tmix tcp mode - "full" or "one-way"
    $self instvar tmix_pkt_size_           ;# pkt size of one-way mode
    $self instvar tmix_debug_output_       ;# tmix output, basic scenario

    $self instvar test_tcp_                  ;# the transport scheme
    $self instvar standard_tcp_            ;# the standard TCP scheme used for
					    # comparisons (usually NewReno SACK)
    $self instvar useAQM_		   ;# if use AQM
    $self instvar AQMtarget_		   ;# target proportion of Q size

    # Initialize parameters
    set useAQM_ 0
    set AQMtarget_ 0

    set num_tmix_flow_ 0
	
    set num_tmix_node_ 0
    set tmix_cv_name_ [list]
    set tmix_Std_cv_name_ [list]
    set tmix_debug_output_ [list]

    set standard_tcp_ Sack1
    set tmix_agent_type_ "one-way"
    set tmix_pkt_size 1460
    eval $self next $args
}

# Config procedures
# tmix traffic
Create_traffic instproc num_tmix_flow {val} {
    $self set num_tmix_flow_ $val
}

Create_traffic instproc tmix_cv_name {val} {
    $self instvar tmix_cv_name_
    lappend tmix_cv_name_ $val 
}
Create_traffic instproc tmix_Std_cv_name {val} {
    $self instvar tmix_Std_cv_name_
    lappend tmix_Std_cv_name_ $val 
}
Create_traffic instproc tmix_debug_output {val} {
    $self instvar tmix_debug_output_
    lappend tmix_debug_output_ $val 
}

Create_traffic instproc num_tmix_node {val} {
    $self set num_tmix_node_ $val
}

Create_traffic instproc tmix_agent_type {val} {
    $self set tmix_agent_type_ $val
}

Create_traffic instproc tmix_pkt_size {val} {
    $self set tmix_pkt_size_ $val
}

Create_traffic instproc test_tcp {val} {
    $self set test_tcp_ $val
}

Create_traffic instproc standard_tcp {val} {
    $self set standard_tcp_ $val
}

Create_traffic instproc useAQM {val} {
    $self set useAQM_ $val
}

Create_traffic instproc AQMtarget {val} {
    # target proportion of queue size
    $self set AQMtarget_ $val
}

# Dispatch args
Create_traffic instproc init_var args {
    set shadow_args ""
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

# Config parameters

# tmix traffic
Create_traffic instproc config_tmix args {
    set args [eval $self init_var $args]
}

# Finish routine
Create_traffic instproc finish {} {
}
