while IFS=, read -r col1 col2
do
    trash=$(./latc_x86_64 "./tests/programs/$col1.lat")
    if [ $? = 0 ]; then
        echo $col1: "COMPILED"
        cat ./tests/outputs/$col2 > tmp1
        ./tests/programs/$col1 > tmp2

        diff tmp1 tmp2
        aredifferent=$?
        echo "XXX"
        echo "XXX"
        echo "XXX"
        echo $aredifferent
        echo "XXX"
        echo "XXX"
        echo "XXX"

        is=$(./tests/programs/$col1)
        if [ $aredifferent -eq 1 ]; then
          echo $col1 ": OK"
        else
          echo $col1 ": FAIL"
          echo "is:"
          cat tmp2
          echo "should be:"
          cat tmp1
        fi
    else
        echo $col1: "COMPILATION ERROR"
    fi
done < tests/expected.csv
