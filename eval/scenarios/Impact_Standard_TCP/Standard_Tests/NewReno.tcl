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
# Note that inital work on this suite was done by:
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
################## Impact on Standard TCP #################
#
# Using default number of experiments to iterate through
# Using default names for experiments to iterate through
set TargetLoad(50_pcnt_load) 50.0
set TargetLoad(100_pcnt_load) 100.0

# TCP test variety 
set Test_TCP Sack1

### Iteration run variables will be added after here
