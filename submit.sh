#!/bin/sh
#
# Name:
#$ -N FRANKENJEWEL
#
# Run through bash shell
#$ -S /bin/bash

#$ -cwd

# Load environment variables for aliroot.
export LHAPATH=/home/staff/0348082/JEWEL/LHAPDF/share/lhapdf/
export PATH=/home/staff/0348082/Python-2.7.11:/home/staff/0348082/FRANKENJEWEL/VISHNU/inst_blup/bin:$PATH
export LD_LIBRARY_PATH=/home/staff/0348082/Python-2.7.11/build/lib.linux-x86_64-2.7:/home/staff/0348082/JEWEL/LHAPDF/lib:/home/staff/0348082/FRANKENJEWEL/VISHNU/inst_blup/lib:$LD_LIBRARY_PATH


# Run the task.
/scratch2/data/0348082/FJ_TEST/jewel-2.0.2-vishnu params-example.dat
