# Prime
A programming language designed for cryptography with built in support for large integers and elliptic curves. Developed for [Professor Edwards'](http://www.cs.columbia.edu/~sedwards/) Programming Languages and Translators Class (COMS W4115) Spring '21 at Columbia Univesity. Prime's compiler is primarily written in OCaml and uses LLVM to generate three address code.

[Prime Final Report](http://www.cs.columbia.edu/~sedwards/classes/2021/4115-spring/reports/PRIME.pdf)

## Team Members and Roles:
- Nikhil Mehta - *Language Guru*
- Alex Liebeskind - *Project Manager*
- Thomas Tran - *Architect*
- Pedro B. T. Santos - *Tester*

# Setup
Run in Professor Edwards' [MicroC](http://www.cs.columbia.edu/~sedwards/classes/2021/4115-spring/microc.tar.gz) Docker container. (Requires downloading Professor Edwards' MicroC source and installing Docker)
```
docker run --rm -it -v `pwd`:/home/microc -w=/home/microc columbiasedwards/plt
```
Run make to install the C GMP library, build prime.native, and run test suite.
```
make
```
To execte a single .pr file filename.pr use `test_file.sh`. Don't include  `.pr` in the filename. Make sure to run make at the start of the docker session to install the C GMP library.
```
./test_file.sh filepath/filename
```
For more, read the language tutoral and language reference manual in our [Final Report](http://www.cs.columbia.edu/~sedwards/classes/2021/4115-spring/reports/PRIME.pdf)

# Development Stages

## Final Stage
`curve`s and `pt`s implmeneted along with elliptic curve arithmetic and operations. Support for functions and added return types.

## Rounding Out Ints and Lints Stage
Entire suite of operators for `int`s and `lint`s implemented, control flow.

## Lint Stage
At this point, we have implemented basic arithmetic operator for integers and large intergers. Assignment operators are functional for intergers, large integers, and strings.

## Hello World Stage
At this point, we have decided to limit our program to printing integers and strings. 
Return statements are also included.

### Developments:
Since the last milestone, we have: 
- Implemented reduced versions of lexer through to top-level using MicroC as a reference guide to structure.
- Left structures in place (commented) to iteratively add more features.
- Added Continuous integration for testing through CircleCI integration.
- Started regression and (semi)unit test suites

## Hello World:
The hello world program (or first Prime program) prints the integer 0. Files for compiling and running test_hello are included in /tests.
