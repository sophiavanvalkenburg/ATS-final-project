
datatypes and functions relating to nfa are in finite_am.[s|d]ats
datatypes and functions relating to pattern matching are in pattern.[s|d]ats
tests for continuation style pattern matching are in test_cont.dats
tests for nfa style pattern matching are in test_nfa.dats

to compile both tests, type
$ make test
or testc and testn for continuation and nfa, respectively

to run the tests, type
$ ./testc
and
$ ./testn

since I didn't have time to write the string parser, patterns
must be written in the pattern datatype form (defined in pattern.sats)
Simply add a test like so:

in the main method of the testing file, type
val p = <pattern>
where p is the name of the pattern and <pattern> is the expression to match.
then add a println! statement with accept_nfa(p, <test>) or accept_cont(p, <test>)
depending on which testing file you are in, where <test> is the input string to test.
then compile and run the program as above.
