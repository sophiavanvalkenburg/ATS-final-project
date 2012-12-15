# makefile for final project

testp: pattern.dats ; atscc -tc $^
testf: finite_am.dats ; atscc -tc $^

test: testp testf

pattern: finite_am.sats finite_am.dats pattern.dats ; atscc -o pattern $^
