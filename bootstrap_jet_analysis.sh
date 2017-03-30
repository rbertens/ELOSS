#       launcher script for the jet analysis on hepMC files
#       outputs root files that have to be merged 
#       'by hand' once all jobs are finished
#
# this script creates
#       - folder structure to store output of frankenjewel jobs
# usage
#       - type './bootstrap_jet_analysis.sh i j
#       with i and j as integers, upper and lower bound of value k which 
#       is used as name for output directory and random seed
#
#       other params can just be added by hand, for now i'm too lazy 
#       to add them as arguments (and they hardly ever change)
#

for((i=$1;i<$2;i++)) 
do
    cd seed$i
    # prepare running through bash, priority, name in the queue
    echo "#!/bin/bash" >> jet_analysis_autoscript_dont_change.sh    
    echo "#$ -N FJ_AUTOLAUNCHER" >> jet_analysis_autoscript_dont_change.sh    
    echo "#$ -q medium*" >> jet_analysis_autoscript_dont_change.sh    
    echo "#$ -S /bin/bash" >> jet_analysis_autoscript_dont_change.sh    
    echo "#$ -cwd" >> jet_analysis_autoscript_dont_change.sh    

    echo "export FASTJET=/data/rhip/alice/rbertens/fastjet-3.2.1_INSTALL" >> jet_analysis_autoscript_dont_change.sh    
    echo "export PATH=/data/rhip/alice/rbertens/root/bin:/data/rhip/alice/rbertens/fastjet-3.2.1_INSTALL/bin:/data/rhip/alice/rbertens/htop-2.0.1/install/bin:/data/rhip/alice/rbertens/HepMC_BUILD/install/bin:/data/rhip/alice/rbertens/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/bin:$PATH" >> jet_analysis_autoscript_dont_change.sh    
    echo "export LD_LIBRARY_PATH=/data/rhip/alice/rbertens/root/lib:/data/rhip/alice/rbertens/fastjet-3.2.1_INSTALL/lib:/data/rhip/alice/rbertens/HepMC_BUILD/install/lib:/data/rhip/alice/rbertens/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/lib:$LD_LIBRARY_PATH" >> jet_analysis_autoscript_dont_change.sh    

    echo "/data/rhip/alice/rbertens/analyze/analyze_hepmc_jets frankenjewel_events.hepmc frankenjewel.root" >> jet_analysis_autoscript_dont_change.sh
    # change permissions
    chmod +x jet_analysis_autoscript_dont_change.sh

    # launch the autolauncher
    qsub jet_analysis_autoscript_dont_change.sh
    cd ..    
done
