#!/bin/bash
RED="\033[0;31m"
GREEN="\033[0;32m"
GRAY="\033[0;37m"
NC="\033[0m"
OK=1

echo "-------------------------------------------------------------------------------------------------------"
echo "-----------------------------RUNNING GOOD"
echo "-------------------------------------------------------------------------------------------------------"

for el in `ls lattests/good/*.lat good/*.lat`; do
    printf "${GRAY}Running $el${NC}:\n"
    ./latte $el; ret=$?;
    if [ "0" == $ret ]
    then
        printf "${GREEN}OK!${NC}\n"
    else
        printf "${RED}BAD, should run normally${NC}\n"
        OK=0
    fi
done;

printf "\n\n\n\n"

echo "-------------------------------------------------------------------------------------------------------"
echo "-----------------------------RUNNING BAD"
echo "-------------------------------------------------------------------------------------------------------"

for el in `ls lattests/bad/*.lat bad/*.lat`; do
    printf "${GRAY}Running $el, expect an error:${NC}\n"
    ./latte $el; ret=$?;
    if [ "1" == $ret ]
    then
        printf "${GREEN}OK!${NC}\n"
    else
        printf "${RED}BAD, should found an error${NC}\n"
        OK=0
    fi
done;

printf "\n\n\n\n"

if [ "$OK" == "1" ]
then
    printf "${GREEN}SUCCESS!${NC}\n"
else
    printf "${RED}THERE WERE SOME ERRORS!${NC}\n"
fi

