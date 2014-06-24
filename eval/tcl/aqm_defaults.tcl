#
# Copyright (c) 2010-2011
#  Swinburne University of Technology, Melbourne, Australia
#  All rights reserved.
#
#
# Released under the GNU General Public License version 2.
#
# Author
#  - David Hayes (dahayes@swin.edu.au or david.hayes@ieee.org)
#
# Set AQM parameters, if not called, use default in ns-default.tcl.
Create_topology instproc set_aqm_params { qsize } {
    $self instvar traffic_ cntlnk_bw_ buffer_length_
    set AQM [$traffic_ set useAQM_]
    set AQMtarget [$traffic_ set AQMtarget_]

    if { $AQM == "RED" } {
	# http://www.icir.org/floyd/papers/adaptiveRed.pdf
	# defaults in ns-defaults.tcl
	Queue/RED set adaptive_ 1
	Queue/RED set targetdelay_ [expr [lindex $buffer_length_ 0] * $AQMtarget]
	Queue/RED set queue_in_bytes_ false
    }
    if { $AQM == "REM" } {
	#mainly uses defaults in ns-defaults.tcl
	# 90% 1500B 10% 536B
	QUEUE/REM set mean_pktsize_ 1400
	QUEUE/REM set pbo_       [expr $qsize * $AQMtarget]
    }
}

