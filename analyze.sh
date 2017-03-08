for (( i=1; i < 5000; i++ ))
do
    cd dirname$i;
    echo " going into "    
    pwd;
    echo " and launching your epic job " 
    cp ../submit_analysis.sh .
    qsub submit_analysis.sh
    cd ..
done
