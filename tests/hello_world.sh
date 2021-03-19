#!/bin/sh
echo 'Hello World!'

# We will later us this file to check that our code returns hello world
Run() {
    echo "#### Testing $1 ####"
    ../prime.native test_hello.pr > test_hello.ll &&
    if ./test_hello.exe | grep -q "Hello World"; then
        echo "Success"
        return 0
    else
        return 1
    fi
}

# Run test_hello.pr