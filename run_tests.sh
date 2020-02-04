while IFS=, read -r col1 col2
do
    trash=$(./latc_x86_64 "./tests/programs/$col1.lat")
    if [ $? = 0 ]; then
        echo $col1: "COMPILED"
        should_be=$col2
        is=$(./tests/programs/$col1)
        if [ $should_be = $is ]; then
          echo $col1 ": OK"
        else
          echo $col1 ": FAIL"
          echo "is:"
          echo $is
          echo "should be:"
          echo $should_be
        fi
    else
        echo $col1: "COMPILATION ERROR"
    fi
done < tests/expected.csv
