///////////////////////////////////////////////////////////////////////////////////////////

Project 3 Submission by 
Sree Pardha Saradhi Gunnam (UFID : 70304545) (pardhug@ufl.edu)
Nitin Agrahara Ravikumar (UFID : 73714398) (arnitin@ufl.edu)

///////////////////////////////////////////////////////////////////////////////////////////

*--------------------------*
|                          |
|Project name : project3   |
|class name   : COP5615	   |
|                          |
*--------------------------*

Normal scala compilation.

Working :
	Detailed anaylsis provided in the report.


Execution : 

The following command line arguments are expected :

numNodes numRequests numFailures

where numFailures is the number of nodes you want to kill during execution.

example

numNodes numRequests
example :
	>scalac project3bonus.scala
	>scala project3bonus 1024 60 400

OUTPUT :
	The ouptput will be a number of log messages indicating the events occuring.
	The final line will contain the the average number of hops that was calculated for that run.

	sample :
	Done!!!!Average number of hops for passing 10 messages is :5.1