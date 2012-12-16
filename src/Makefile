# makefile for final project

tcp: pattern.dats ; atscc -tc $^
tcf: finite_am.dats ; atscc -tc $^

tc: tcp tcf

testc: finite_am.sats finite_am.dats pattern.sats pattern.dats test_cont.dats ; atscc -o testc $^
testn: finite_am.sats finite_am.dats pattern.sats pattern.dats test_nfa.dats ; atscc -o testn $^
test: testc testn

clean: ; rm *.c testc testn
