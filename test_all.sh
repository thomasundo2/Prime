#!/bin/bash
# time limit on operations
ulimit -t 30
logfile=tests.log
rm -rf $logfile
error=0
exitcode=0

IsError() {
    if [ $error -eq 0 ] ; then
    echo "FAILED"
    error=1
    fi
    # print out what we failed
    echo " $1"
}

Difference() {
    echo diff -b -q $1 $2 ">" $logfile 1>&2
    diff -b "$1" "$2" > "$1.diff" 2>&1 || {
    IsError "Difference in $1"
    }
}

# Run a command retaining error code
Run() {
    echo $* 1>&2
    eval $* || {
        IsError "$1 Failed (cmd: $*)"
        return 1
    }
}


Test() {
    error=0
    # extracting filename seen here: https://stackoverflow.com/questions/965053/extract-filename-and-extension-in-bash?page=1&tab=votes#tab-top
    filename=$(basename -- "$1")
    filename="${filename%.*}"

    echo -n "Test: $filename "
    # newline between tests
    echo 1>&2
    echo "#### Testing $1 ####" 1>&2

    # Run the various compilation parts
    Run "./prime.native" "$1" ">" "$filename.ll" &&
    Run "llc" "-relocation-model=pic" "$filename.ll" ">" "$filename.s" &&
    Run "cc" "-o" "$filename.exe" "$filename.s" "gmpfunc.o" "structs.o" "input.o" "-lgmp" &&
    Run "./$filename.exe" > "$filename.test" &&
    Difference $filename.test ./tests/$filename.out

    if [ $error -eq 0 ] ; then
        echo "OK"
        echo "#### Success" 1>&2
    else
        echo "#### FAIL" 1>&2
        exitcode=$error
    fi
}

RunFail() {
    echo $* 1>&2
    # Use short circuit && operator
    eval $* && {
        IsError "failed: $* did not show error"
        return 1
    }
    return 0
}

TestFail() {
    error=0
    # extracting filename seen here: https://stackoverflow.com/questions/965053/extract-filename-and-extension-in-bash?page=1&tab=votes#tab-top
    filename=$(basename -- "$1")
    filename="${filename%.*}"

    echo -n "Test: $filename "
    # newline between tests
    echo 1>&2
    echo "#### Testing $1 ####" 1>&2

    # This is a fila case so should not get past the compiler
    RunFail "./prime.native" "<" $1 "2>" "$filename.test" ">>" $logfile &&
    Difference $filename.test ./tests/$filename.out

    if [ $error -eq 0 ] ; then
        echo "OK"
        echo "#### Success" 1>&2
    else
        echo "#### FAIL" 1>&2
        exitcode=$error
    fi
}

# make sure C files ready
# Compile/link in gmpfunc file
cc -c gmpfunc.c
cc -c structs.c
cc -c input.c

# Run test_hello.pr
# check if specific files to test
if [ $# -ge 1 ]
then
    # provided specific files to test
    files=$@
else
    files="tests/*.pr"
fi

# run positive tests for now
for file in $files
do
    if [[ $file != *fail*.pr ]] ;
    then
        Test $file 2>> $logfile
    else
        TestFail $file 2>> $logfile
    fi
done

# clean up ()
# rm -rf *.exe *.test *.ll *.s

# print out so we can see return at the end
cat $logfile
exit $exitcode
