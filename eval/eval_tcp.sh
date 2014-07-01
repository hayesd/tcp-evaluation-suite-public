#!/bin/bash
### or !/bin/bash
# Copyright (c) 2011
#  Swinburne University of Technology, Melbourne, Australia
#  All rights reserved.
# Copyright (c) 2013,2014
#  University of Oslo, Norway
#  All rights reserved.
#
# Released under the GNU General Public License version 2.
#
# This script runs a particular TCP evaluation test (or all of them).
# The ResultBaseDir is also the working directory.
#
#
# Author:
#  - David Hayes (davihay@ifi.uio.no david.hayes@ieee.org)
#
echo 
echo
if [ $# -lt 1 ]
then
    echo "$0 testscenario|all [--resultdir=resultdirbasename] [--parallel=1-N]] [--savetracefile=1|0]] [--findtarget=1|0]]"
    echo "      examples: $0 Basic_Dumbbell_Scenarios/access_link"
    echo "                $0 all"
    echo "                $0 all --resultdir=\"results\" --parallel=8"
    echo "      defaults: --resultdir=\"$CWD/results\", --parallel=1, savetracefile=0, findtarget=0"
    echo
    echo "Note: export TCPEVAL=\"path of $0\"  to run from another directory"
    echo
    exit 1
fi

TestScenario=$1
CurDir=`pwd`
if [ -z  $TCPEVAL ]
then
    TCPEVAL=$CurDir
    export TCPEVAL
fi

ResultBaseDir="$CurDir/results"
Parallel=1
SaveTraceFile=0
FindTarget=0
# shift past testscenario
shift
while (($#));
do
    case $1 in
	--resultdir=*)
	    ResultBaseDir=${1#*=}
	    shift
	    ;;
	--parallel=*)
	    Parallel=${1#*=}
	    shift
	    ;;
	--savetracefile=*)
	    SaveTraceFile=${1#*=}
	    shift
	    ;;
	--findtarget=*)
	    FindTarget=${1#*=}
	    shift
	    ;;
	--help)
	    echo "$0 testscenario|all [--resultdir=resultdirbasename] [--parallel=1-N]] [--savetracefile=1|0]] [--findtarget=1|0]]"
	    echo "      examples: $0 Basic_Dumbbell_Scenarios/access_link"
	    echo "                $0 all"
	    echo "                $0 all --resultdir=\"results\" --parallel=8 0"
	    echo "      defaults: --resultdir=\"$CWD/results\", --parallel=1, savetracefile=0, findtarget=0"
	    exit 1
	    ;;
	*)
	    echo "*** INVALID OPTION ***"
	    echo 
	    echo "$0 testscenario|all [--resultdir=resultdirbasename] [--parallel=1-N]] [--savetracefile=1|0]] [--findtarget=1|0]]"
	    echo "      examples: $0 Basic_Dumbbell_Scenarios/access_link"
	    echo "                $0 all"
	    echo "                $0 all --resultdir=\"results\" --parallel=8 0"
	    echo "      defaults: --resultdir=\"$CWD/results\", --parallel=1, savetracefile=0, findtarget=0"
	    exit 1
	    ;;
    esac
done


TmixCv="$TCPEVAL/scenarios/Tmix_Traffic"

EvaluationTests="Ramp_Up_Time/TwoSrcDumbell
Multiple_Bottlenecks/FourFlowThreeBottleneck
Basic_Dumbbell_Scenarios/trans_oceanic
Basic_Dumbbell_Scenarios/data_center
Basic_Dumbbell_Scenarios/geo_satellite
Basic_Dumbbell_Scenarios/access_link
Basic_Dumbbell_Scenarios/wireless_access
Basic_Dumbbell_Scenarios/dial_up
Basic_Dumbbell_Scenarios/Delay_Throughput_Tradeoff
Impact_Standard_TCP/Standard_Tests"
TargetExemptionList="Basic_Dumbbell_Scenarios/Delay_Throughput_Tradeoff"

# set working DIR
if [ ! -e "$ResultBaseDir" ]
then
    mkdir "$ResultBaseDir"
fi

if [ "$TestScenario" != "all" ]
then
     EvaluationTests=$TestScenario
fi

echo "Starting Simulations"
echo "______________________________________________________________________"
datestr=`date`
echo "          $datestr"
echo

runtestscenario() {
    FuncEvaltest=$1
    testgroup=${FuncEvaltest%/*}
    if [ ! -e "$ResultBaseDir/$testgroup" ]
    then
        mkdir "$ResultBaseDir/$testgroup"
    fi
    if [ ! -e "$ResultBaseDir/$FuncEvaltest" ]
    then
	mkdir "$ResultBaseDir/$FuncEvaltest"
    fi
    WorkDir=$ResultBaseDir/$FuncEvaltest

    cd $WorkDir
    if [ ! -e $TCPEVAL/scenarios/$testgroup/test_scenario.tcl ]
    then
	echo "ERROR: $TCPEVAL/scenarios/$testgroup/test_scenario.tcl does not exist"
	break
    fi
    cp -f $TCPEVAL/scenarios/$testgroup/test_scenario.tcl .
    if [ ! -e $TCPEVAL/scenarios/$FuncEvaltest/tcl_base_setup ]
    then
	echo "ERROR: $TCPEVAL/scenarios/$FuncEvaltest/tcl_base_setup does not exist"
	break
    fi
    echo "set tmix_cv_dir $TmixCv" | cat -  $TCPEVAL/scenarios/$FuncEvaltest/tcl_base_setup | ( cat > tcl_base_setup )
    for defTCL in $TCPEVAL/scenarios/$FuncEvaltest/*.tcl
    do
	defTCLname=${defTCL//*\//}
	defTCLbase=${defTCLname%.*}
	cp $defTCL .
	echo "global tmp_directory_" >> $defTCLname
	echo "set tmp_directory_ $WorkDir" >> $defTCLname
	echo "set result_basename $defTCLbase" >> $defTCLname
	echo "set SaveTraceFile $SaveTraceFile" >> $defTCLname
	reallyFindTarget=$FindTarget
	if [[ $FindTarget ]]
	then
	    for t in $TargetExemptionList
	    do 
		if [ $FuncEvaltest == $t ]
		then
		    reallyFindTarget=0
		fi
	    done
	fi
	echo "set findtarget $reallyFindTarget" >> $defTCLname

	ln -fs $defTCLname defaults.tcl
	echo
	echo "In $WorkDir,"
	echo "  executing \"ns test_scenario.tcl\" "
	echo "  with $defTCLname"
	TargetIter=1
	echo "set TargetIter $TargetIter" >>  $defTCLname
	ExperimentIteration=1
	echo "set ExperimentIteration $ExperimentIteration" >> $defTCLname
	echo " Experiment $ExperimentIteration"
	while true
	do
	    ns test_scenario.tcl > _~_ns_rtn_vals
	    #test exit code from from ns
	    if [ $? -gt 0 ]
	    then
		echo "ns error"
		echo "   stdout: $RTN_VALS"
		echo "   Bad return"
		break
	    fi	
	    RTN_VALS=`cat _~_ns_rtn_vals | awk 'BEGIN{FS=",";OFS=","} $1 ~ /tcpevaliterations/ {print $2,$3}'`
	    datestr=`date`
	    echo "          $datestr"
	    ExperimentIteration=${RTN_VALS/,*/}
	    if  (( `echo "$ExperimentIteration < 0" | bc` ))
	    then
		# finished running experiments in this tcl file
		break
	    fi
	    echo "set ExperimentIteration $ExperimentIteration" >> $defTCLname
	done
    done
    if [ $SaveTraceFile -eq 0 ]
    then
	for SpecSrc in *Src[0-8]
	do
	    if [ -e $SpecSrc ]
	    then
		rm -f $SpecSrc
	    fi
	done
    fi
    #below is unnecessary now
    #cd $CurDir
    exit 0
}

for evaltest in $EvaluationTests
do
    runtestscenario $evaltest &
    # try to run more in parallel, remembering the parallised shuffle
    # This does not paralalise tests within a particular test scenario
    # as this would be dangerous given there is file reuse
    while true;
    do
        j=`jobs -p`
	aj=(${j// / })
	if [ ${#aj[@]} -lt $Parallel ]
	then
	    break
	fi
	sleep 2
    done
done

wait

echo "______________________________________________________________________"
echo "Simulations Finished"
datestr=`date`
echo "          $datestr"
echo "______________________________________________________________________"
echo
