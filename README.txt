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
	We tested the complete functionality of Join and Routing aspects of the Patry algorithm as mentioned.
	We have successfully tested the network for 4096 Nodes after which the algorithm takes a large amount of time to give 	results.


Execution : 

the following command line arguments are expected :

numNodes numRequests
example :
	>scalac project3.scala
	>scala project3 1024 60

OUTPUT :
	The ouptput will be a number of log messages indicating the events occuring.
	The final line will contain the the average number of hops that was calculated for that run.

	sample :

	Done!!!!Average number of hops for passing 10 messages for each node is :4.45703125