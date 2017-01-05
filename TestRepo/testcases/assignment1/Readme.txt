Assigment 1 CPSC449 T01 Group4

Compile: javac Main.java 

Execute: java Main <input file> <output file>
The input file should be located in the same directory of the .java and .class files.
The output file is created in the same location.
If a file already exists, the output file will be overwritten.
 

Files contained in the file:
	InvalidMachineTaskException.java
	Invalid PenaltyExeception.java
	MachinePenaltyException.java
	Main.java
	Node.java
	NoValidSolutionException.java
	ParseException.java
	Parser.java
	PartialAssignmentErrorException.java
	Solver.java

This program calculates the minimum penalty for tasks assigned to machines (1-8) and
returns the order in which tasks will result in minimum penalty.

The file takes an input text file with the following format:
***********************************************
Name:
*Name of file*

forced partial assignment:
*(machine,task)*

forbidden machine:
*(machine,task)*

too-near tasks:
*(task,task)*

machine penalties: 
8 x 8 array seperated by spaces
x x x x x x x x
x x x x x x x x
x x x x x x x x
x x x x x x x x
x x x x x x x x
x x x x x x x x
x x x x x x x x
x x x x x x x x

too-near penalities
*(task,task,penalty value)*
**********************************************