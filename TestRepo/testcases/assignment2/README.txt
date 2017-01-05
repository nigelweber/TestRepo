=====INFORMATION:=====
This Haskell repository was created for Assignment 2 of CPSC449 L01, University of Calgary. Given an input file containing a series of constraints for a machine-task scheduling problem, it reports the solution with the fewest penalties if one exists, or returns information on a formatting error within the file.
Authors: Daniel Armstrong, Nathan Harms, Nigel Weber, Winston Chan, and Danny Yu.
Date: November 8, 2013

=====FILES INCLUDED:=====
main.hs
Parser.hs
Printer.hs
Solver.hs
Utils.hs
main *executable*

=====TO COMPILE:=====
Make sure all repository files have been unpacked into the same directory, and a command line is open in that directory
ghc main.hs

=====TO RUN WITH COMMAND LINE:=====
Open command line in the folder containing the compiled file
<path>/main *arg1* *arg2*
	arg1: Input file: exact specifications listed below
	arg2: Output file: consists of the solution after solving, or an error message that was encountered during the attempt
	Make sure that if the input file and output file are not within the same folder as the command line prompt, they are given relative filepaths.

=====TO RUN WITH HUGS:=====
Open hugs within the folder containing the unpacked repository
:l <path>main.hs
:main "*arg1*" "*arg2*"


=====PROPER INPUT FILE FORMATTING:=====
Name:
*name*

forced partial assignment:
(*mach1*,*task1*)
...
(*machn*,*taskn*)

forbidden machine:
(*mach'1*,*task'1*)
...
(*mach'm*,*task'm*)
 
too-near tasks:
(*task"11*,*task"12*)
...
(*task"k1*,*task"k2*)

machine penalties:
*p11* *p12* *p13* *p14* *p15* *p16* *p17* *p18*
*p21* *p22* *p23* *p24* *p25* *p26* *p27* *p28*
*p31* *p32* *p33* *p34* *p35* *p36* *p37* *p38*
*p41* *p42* *p43* *p44* *p45* *p46* *p47* *p48*
*p51* *p52* *p53* *p54* *p55* *p56* *p57* *p58*
*p61* *p62* *p63* *p64* *p65* *p66* *p67* *p68*
*p71* *p72* *p73* *p74* *p75* *p76* *p77* *p78*
*p81* *p82* *p83* *p84* *p85* *p86* *p87* *p88*

too-near penalities
(*task+11*,*task+12*,*p1*)
...
(*task+x1*,*task+x2*,*px*)


=====OUTPUT FILE ERRORS:=====
"machine penalty error": The "machine penalties:" section contains either too many or too few lines of penalties, or one line does not contain the proper number of penalties.

"invalid task": One or more of the tasks expressed during the "too-near penalities" section is not a valid task (member of [ABCDEFGH])

"invalid penalty": One or more of the penalties expressed in "machine penalties:" and "too-near penalities" sections has not been expressed as a valid (natural) number.

"invalid machine/task": One or more of the hard constraints ("forced partial assignment:", "forbidden machine:" and "too-near tasks:") has an invalid machine or task.

"Error while parsing input file": A problem exists with the formatting of the file itself.