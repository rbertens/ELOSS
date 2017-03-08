# small script to generate parameter file
# and launch jobs
# this is important, since otherwise jewel will always use the same random seed

for((i=0;i<1;i++)) 
do
    mkdir dirname$i
    cd dirname$i
    cp ../medium-params.dat .
    cp ../vishnu_events.dat .
    cp ../submit.sh .

    echo "LOGFILE example.log" > params-example.dat
    echo "HEPMCFILE example.hepmc" >> params-example.dat
    echo "NEVENT 1000" >> params-example.dat
    echo "NJOB $i" >> params-example.dat
    echo "PTMIN 5." >> params-example.dat
    echo "PTMAX 250." >> params-example.dat
    echo "ETAMAX 4." >> params-example.dat
    
    chmod +x submit.sh
    qsub submit.sh
    cd ..    
done
