#!/bin/sh
# time limit on operations
ulimit -t 30
logfile=tests.log
rm -rf $logfile
error=0
exitcode=0

IsError() {
    if [ $error -eq 0 ] ; then
    echo "Fail"
    error=1
    fi
}

Difference() {
    echo diff -b -q $1 $2 > $logfile 1>&2
    diff -b -q "$1" "$2" > "$logfile" 2>&1 || {
    IsError "Difference in $1"
    }
}

# Run a command retaining error code
Run() {
    echo $* 1>&2
    eval $* || {
        IsError "$1 Failed"
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
    Run "cc" "-o" "$filename.exe" "$filename.s" &&
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

# Run test_hello.pr
# check if specific files to test
if [ $# -ge 1 ]
then
    # provided specific files to test
    files=$@
else
    files="tests/*.pr"
fi

# run tests
for file in $files
do
    Test $file 2>> $logfile
done

# Test tests/test_hello.pr 2>> $logfile

# clean up ()
# rm -rf *.exe *.test *.ll *.s

# print out so we can see return at the end
cat $logfile
exit $exitcode
