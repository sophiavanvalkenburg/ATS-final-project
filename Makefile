# makefile for final project

test: pattern.dats ; atscc -tc $^
pattern: pattern.dats ; atscc -o pattern $^
