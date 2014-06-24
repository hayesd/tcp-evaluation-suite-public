#
#
# Copyright (c) 2007  NEC Laboratories China.
# All rights reserved.
#
# Copyright (c) 2010-2011
#  Swinburne University of Technology, Melbourne, Australia
#  All rights reserved.
#
# Released under the GNU General Public License version 2.
#
# Original authors:
# - Gang Wang (wanggang@research.nec.com.cn)
# - Yong Xia   (xiayong@research.nec.com.cn)
#
# Revised, enhanced and updated
# - David Hayes (dahayes@swin.edu.au or david.hayes@ieee.org)
#
##################################################################
#
# This is where parameters for new TCP schemes should be added
#
##################################################################
#
# Each Test scheme (or special settings) should be a class so that tmix can
# handle it properly
#
Class Agent/TCP/Sack1/Eval_HSTCP -superclass Agent/TCP/Sack1
Class Agent/TCP/Sack1/IW_Sack1 -superclass Agent/TCP/Sack1
Class Agent/TCP/Linux/Eval_LEDBAT -superclass Agent/TCP/Linux
Class Agent/TCP/Linux/Eval_COMPOUND -superclass Agent/TCP/Linux
Class Agent/TCP/Linux/Eval_CUBIC -superclass Agent/TCP/Linux
Class Agent/TCP/Linux/IW_CTCP -superclass Agent/TCP/Linux
Class Agent/TCP/Linux/IW_Cubic -superclass Agent/TCP/Linux
Class Agent/TCP/Linux/Eval_BIC -superclass Agent/TCP/Linux
Class Agent/TCP/Linux/Eval_L_NewReno -superclass Agent/TCP/Linux
# Sink
Class Agent/TCPSink/Sack1/Eval_L_SINK -superclass Agent/TCPSink/Sack1/DelAck

Agent/TCP/Sack1/Eval_HSTCP instproc init args {
    set ns [Simulator instance]
    set st [$ns now] 
    $ns at $st "$self set windowOption_ 8"
    $ns at $st "$self set hstcp_fix_ 1"
    eval $self next $args
}
Agent/TCP/Sack1/IW_Sack1 instproc init args {
    set ns [Simulator instance]
    set st [$ns now] 
    $ns at $st "$self set windowInit_ 10"
    $ns at $st "$self set windowInitOption_ 1"
    eval $self next $args
}
#
#
# Linux based congestion control
#
Agent/TCP/Linux/Eval_LEDBAT instproc init args {
    set ns [Simulator instance]
    set st [$ns now] 
    $ns at $st "$self set timestamps_ true"
    $ns at $st "$self set partial_ack_ true"
    $ns at $st "$self select_ca ledbat"
    eval $self next $args
}
Agent/TCP/Linux/IW_CTCP instproc init args {
    set ns [Simulator instance]
    set st [$ns now] 
    $ns at $st "$self set timestamps_ true"
    $ns at $st "$self set partial_ack_ true"
    $ns at $st "$self set windowInit_ 10"
    $ns at $st "$self set windowInitOption_ 1"
    $ns at $st "$self select_ca compound"
    eval $self next $args
}
Agent/TCP/Linux/Eval_COMPOUND instproc init args {
    set ns [Simulator instance]
    set st [$ns now] 
    $ns at $st "$self set timestamps_ true"
    $ns at $st "$self set partial_ack_ true"
    $ns at $st "$self select_ca compound"
    eval $self next $args
}
Agent/TCP/Linux/IW_Cubic instproc init args {
    set ns [Simulator instance]
    set st [$ns now] 
    $ns at $st "$self set timestamps_ true"
    $ns at $st "$self set partial_ack_ true"
    $ns at $st "$self set windowInit_ 10"
    $ns at $st "$self set windowInitOption_ 1"
    $ns at $st "$self select_ca cubic"
    eval $self next $args
}
Agent/TCP/Linux/Eval_CUBIC instproc init args {
    set ns [Simulator instance]
    set st [$ns now] 
    $ns at $st "$self set timestamps_ true"
    $ns at $st "$self set partial_ack_ true"
    $ns at $st "$self select_ca cubic"
    eval $self next $args
}
Agent/TCP/Linux/Eval_BIC instproc init args {
    set ns [Simulator instance]
    set st [$ns now] 
    $ns at $st "$self set timestamps_ true"
    $ns at $st "$self set partial_ack_ true"
    $ns at $st "$self select_ca bic"
    eval $self next $args
}
Agent/TCP/Linux/Eval_L_NewReno instproc init args {
    set ns [Simulator instance]
    set st [$ns now] 
    $ns at $st "$self set timestamps_ true"
    $ns at $st "$self set partial_ack_ true"
    $ns at $st "$self select_ca reno"
    eval $self next $args
}
#
Agent/TCPSink/Sack1/Eval_L_SINK instproc init args {
    set ns [Simulator instance]
    set st [$ns now] 
    $ns at $st "$self set generateDSacks_ false"
    $ns at $st "$self set ts_echo_rfc1323_ true"
    eval $self next $args
}
#
# Choose approate TCP src, TCP sink and Queue.
Create_topology instproc get_tcp_params { scheme group} {
    $self instvar SRC SINK QUEUE OTHERQUEUE SRC_INIT
    $self instvar queue_core_ queue_transit_ queue_stub_ 
    $self instvar btnk_buf_ traffic_
    
    set AQM [$traffic_ set useAQM_]
    set QUEUE DropTail
    set OTHERQUEUE DropTail
    set queue_core_ DropTail
    set queue_transit_ DropTail
    set queue_stub_ DropTail
    set SRC_INIT($group) ""
    
    Agent/TCP set packetSize_ 1460; # packet size for one-way TCP
    Agent/TCP/FullTcp set segsize_ 1460; # packet size for full-TCP

    Agent/TCP         set window_ 100000
    Agent/TCP/FullTcp set window_ 100000
    Agent/TCP/Sack1   set window_ 100000
    
    Agent/TCP         set ssthresh_ 100000
    Agent/TCP/FullTcp set ssthresh_ 100000
    Agent/TCP/Sack1   set ssthresh_ 100000
    
    

    if { $AQM == "RED" } {
	set QUEUE RED
	set queue_core_ RED
	set queue_transit_ RED
	Agent/TCP set ecn_ 1 ;
    }
    if { $AQM == "REM" } {
	set QUEUE REM
	set queue_core_ REM
	set queue_transit_ REM
	Agent/TCP set ecn_ 1 ;
    }

    if { $scheme == "Reno" } {
        set SRC($group)   Reno
        set SINK($group)  DelAck
    }
    
    if { $scheme == "Newreno" } {
        set SRC($group)   Newreno
        set SINK($group)  DelAck
    }
        
    if { $scheme == "L_NewReno" } {
        set SRC($group)   Linux/Eval_L_NewReno
        set SINK($group)  Sack1/Eval_L_SINK
    }
        
    if { $scheme == "Sack1" } {
	    set SRC($group)   Sack1
	    set SINK($group)  Sack1/DelAck
    }
    
    if { $scheme == "iw10Sack1" } {
	    set SRC($group)   Sack1/IW_Sack1
	    set SINK($group)  Sack1/DelAck
    }

    if { $scheme == "HSTCP" } {
	    set SRC($group)   Sack1/Eval_HSTCP
	    set SINK($group)  Sack1/DelAck
    }
    
    if { $scheme == "BIC" } {
        set SRC($group)   Linux/Eval_L_BIC
	set SINK($group)  Sack1/Eval_L_SINK
    }
    
    if { $scheme == "Ledbat" } {
        set SRC($group)   Linux/Eval_LEDBAT
	set SINK($group)  Sack1/Eval_L_SINK 
   }

    if { $scheme == "iw10Compound" } {
        set SRC($group)   Linux/IW_CTCP
	set SINK($group)  Sack1/Eval_L_SINK
    }
    if { $scheme == "Compound" } {
        set SRC($group)   Linux/Eval_COMPOUND
	set SINK($group)  Sack1/Eval_L_SINK
    }

    if { $scheme == "iw10Cubic" } {
        set SRC($group)   Linux/IW_Cubic
	set SINK($group)  Sack1/Eval_L_SINK
    }
    if { $scheme == "Cubic" } {
        set SRC($group)   Linux/Eval_CUBIC
	set SINK($group)  Sack1/Eval_L_SINK
    }

   if { $scheme == "XCP" } {
       set SRC($group)   Reno/XCP
       set SINK($group)  XCPSink
       set QUEUE XCP
       set OTHERQUEUE XCP
       set queue_core_ XCP
       set queue_transit_ XCP
       set queue_stub_ XCP
       #Agent/TCP set minrto_ 1 ;# frin XCP sample script
   }
    
    if { $scheme == "VCP" } {
        set SRC($group)   Reno/VcpSrc
        set SINK($group)  VcpSink
        set QUEUE DropTail2/VcpQueue
        set queue_core_ DropTail2/VcpQueue
        set queue_transit_ DropTail2/VcpQueue
    }
    
# FullTCP initialization.
# Reno
    if { $scheme == "FullTcp" } {
        Agent/TCP/FullTcp set segsize_ 1460;           # set MSS to 1460 bytes
        Agent/TCP/FullTcp set nodelay_ true;           # disabling nagle
        Agent/TCP/FullTcp set segsperack_ 2;           # delayed ACKs
        Agent/TCP/FullTcp set interval_ 0.1;           # 100 ms
        #Agent/TCP/FullTcp set ssthresh_ 64;           # slow start threshold ms
    }   
}

