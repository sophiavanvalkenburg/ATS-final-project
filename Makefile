# makefile for final project

testp: pattern.dats ; atscc -tc $^
testd: dfa.dats ; atscc -tc $^

test: testp testd

pattern: dfa.sats dfa.dats pattern.dats ; atscc -o pattern $^
