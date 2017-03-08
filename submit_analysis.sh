#!/bin/sh
#
# Name:
#$ -N ROOT_ANALYSIS_OF_JEWEL_HEPMC
#
# Run through bash shell
#$ -S /bin/bash

#$ -cwd

# Load environment variables for aliroot.
export LHAPATH=/home/staff/0348082/JEWEL/LHAPDF/share/lhapdf/
export PATH=/home/staff/0348082/Python-2.7.11:/home/staff/0348082/FRANKENJEWEL/VISHNU/inst_blup/bin:$PATH
export LD_LIBRARY_PATH=/home/staff/0348082/Python-2.7.11/build/lib.linux-x86_64-2.7:/home/staff/0348082/JEWEL/LHAPDF/lib:/home/staff/0348082/FRANKENJEWEL/VISHNU/inst_blup/lib:$LD_LIBRARY_PATH

export ROOTSYS=/scratch/software/root/v5-34-30/inst/
export PATH=$PATH:$ROOTSYS/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$ROOTSYS/lib

# Run the task.
/home/staff/0348082/JEWEL/analyze/analyze_hepmc_jets example.hepmc example.root
