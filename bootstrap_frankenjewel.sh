#       launcher script
#
# this script creates
#       - folder structure to store output of frankenjewel jobs
#       - parameter file 
# usage
#       - type './bootstrap_frankenjewel.sh i j
#       with i and j as integers, upper and lower bound of value k which 
#       is used as name for output directory and random seed
#
#       other params can just be added by hand, for now i'm too lazy 
#       to add them as arguments (and they hardly ever change)
#

if [ ! -f vishnu_events.dat ]; then
        echo " > vishnu_events.dat not found, aborting"
        exit
fi

for((i=$1;i<$2;i++)) 
do
    mkdir seed$i
    cd seed$i
    # create medium parameters file
    # additional arguments can be passed simply by adding echo statements here
    echo "TI 0.40" >> medium-params.dat
    echo "CENTRMIN 30." >> medium-params.dat
    echo "CENTRMAX 40." >> medium-params.dat

    cp ../vishnu_events.dat .
    echo "LOGFILE frankenjewel_report.log" > frankenparams.dat
    echo "HEPMCFILE frankenjewel_events.hepmc" >> frankenparams.dat
    echo "NEVENT 1000" >> frankenparams.dat
    echo "NJOB $i" >> frankenparams.dat
    echo "PTMIN 5." >> frankenparams.dat
    echo "PTMAX 250." >> frankenparams.dat
    echo "ETAMAX 4." >> frankenparams.dat


    # write the auto launch script - farm specific

    # prepare running through bash, priority, name in the queue
    echo "#!/bin/bash" >> fj_autoscript_dont_change.sh    
    echo "#$ -N FJ_AUTOLAUNCHER" >> fj_autoscript_dont_change.sh    
    echo "#$ -q medium*" >> fj_autoscript_dont_change.sh    
    echo "#$ -S /bin/bash" >> fj_autoscript_dont_change.sh    
    echo "#$ -cwd" >> fj_autoscript_dont_change.sh    

    # prepare shell
    echo "module load gsl/1.15" >> fj_autoscript_dont_change.sh    
    echo "module load numpy/1.11.0-python2.7.3" >> fj_autoscript_dont_change.sh    
    echo "export LHAPATH=/data/rhip/alice/rbertens/JEWEL/LHAPDF/inst_5_8_9/share/lhapdf" >> fj_autoscript_dont_change.sh    
    echo "export LD_LIBRARY_PATH=/data/rhip/alice/rbertens/JEWEL/LHAPDF/inst_5_8_9/lib:/data/rhip/alice/rbertens/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/lib:$LD_LIBRARY_PATH" >> fj_autoscript_dont_change.sh    
    echo "export PATH=/data/rhip/alice/rbertens/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/bin:$PATH" >> fj_autoscript_dont_change.sh    
    echo "/data/rhip/alice/rbertens/JEWEL/FRANKENJEWEL/source/jewel-2.0.2-vishnu frankenparams.dat" >> fj_autoscript_dont_change.sh    

    # change permissions
    chmod +x fj_autoscript_dont_change.sh

    # launch the autolauncher
    qsub fj_autoscript_dont_change.sh
    cd ..    
done
