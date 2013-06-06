import scala.actors.Actor
import scala.actors.Actor._
import scala.math
import scala.actors.remote.Node
import scala.actors.TIMEOUT
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer


class PastryNodebonus(InstigatorFellow:Instigatorbonus,binarysize:Int) extends Actor {
  val reactWithinTime:Int = 1000 // 1 Second
  var myID:Int = 0;
  var myPeerID:Int = 0;
  var isPeer:Int = 0;
  var lesserLeaf = new ArrayBuffer[String]
  var greaterLeaf= new ArrayBuffer[String]
  var connection_list= new ArrayBuffer[Int]
 // var binarysize:Int = 7
  type Row = ArrayBuffer[Int]
  type conn_type = ArrayBuffer[PastryNodebonus]
  var myRowOfStrings = new Row
  val RoutingTable = new Array[Row](binarysize)
  var ForwardLeaf = new ArrayBuffer[Int]
  var RearLeaf = new ArrayBuffer[Int]
  var totNumPeers:Int = 127
  var myBinary:String =""
  var maxLeaf = -1
  var minLeaf = -1
  var currPeers:Int = 0
  var conn = new ArrayBuffer[PastryNodebonus]
 // var full_buff : Row

  var i=0
  while (i<binarysize){
    RoutingTable(i) = new ArrayBuffer[Int]
    i += 1
  }

  def binaryfy(x: Int): String = {
   var binarycode = Integer.toBinaryString(x)
          if(binarycode.length()<binarysize) {
            var diff = binarysize - binarycode.length();
            while(diff >0){
              binarycode = '0' + binarycode;
              diff -= 1;
            }
          }
   
        return binarycode
  }

  def flipModify(binarycode: String,loc:Int): String = {
        var length = binarycode.length()
        var modBinary:String ="";
        var locFlag = 0
        var inc = 0
        while(inc<length){
                if(loc == inc + 1){
                        if(binarycode(inc) == '1'){
                                modBinary = modBinary + '0'
                        } else {
                                modBinary = modBinary + '1'
                        }
                locFlag = 1
                } else {
                        if(locFlag == 0){
                                modBinary = modBinary + binarycode(inc)
                        } else {
                                modBinary = modBinary + '0'
                        }
                }

                inc += 1
        }
        return modBinary
  }

  def returnMatchingBits(binarycodeOne: String,binarycodeTwo: String):
Int = { //Assuming strings are of equal length
    var i:Int = 0
    var length:Int = binarycodeOne.length()
    while(i<length){
      try{
      if(binarycodeOne(i) == binarycodeTwo(i)){
        i += 1;
      } else {
        return i
      }
      } catch{
         case e: Exception =>
//println("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$4 " + binarycodeOne + "  " + binarycodeTwo)
      }
    }
    return i
  }

  def checkifWithinLeafs(target:Int) : Int = {
    var nearest = 1000000000;
 //   println("Why Herefdsfsdf   : MaxLeaf : " + maxLeaf + " MinLeaf : " + minLeaf + " Target: " + target + "  ForwardLeaf : " + ForwardLeaf)

         i = 0
         var diff = 1000
          while(i<4) { // num of leaf nodes
            if(ForwardLeaf(i) == target){
              return ForwardLeaf(i);
            } else {
                if(math.abs(target - ForwardLeaf(i)) < diff){
                        nearest = (i)
                        diff = math.abs(target - ForwardLeaf(i))
                }
            }
            i += 1
          }
         if (diff >= 2) {
           return -999
         } else {
           return ForwardLeaf(nearest)
         }

    return -999
  }

        def act(){
                 loop {
                        react{//reactWithin(reactWithinTime){
                          case "exit" =>
                            //println(myPeerID +" received exit")
                            exit()
                         case TIMEOUT =>
                         //  println("Now Sending Message")

                         case x :(ArrayBuffer[PastryNodebonus],Int) =>
                           myID = x._2
                           conn = x._1
                         case x: (Int,ArrayBuffer[Int],String) =>
                           //println(myID+"received rand array buffer and peer id")
                           myPeerID = x._1
                           connection_list = x._2
                           isPeer   = 1
                            myBinary = binaryfy(x._1)
                          /*  println("Received ID:" + x._1  + "=" + myBinary + " " +
                              "Reverse=" + Integer.parseInt(myBinary,2) +
                              " Flipped " + flipModify(myBinary,3))*/
                         case x: (Int,ArrayBuffer[Int],String,Int,Int) =>// receiving Peer ID

                          if(x._3 == "Join1"){ //do routine joins
                             currPeers = x._4
                           //  println("received Join1 for node "+x._1)
                            /* Routing Table */
                             var curBit = 0
                             while(curBit<binarysize){
                                 RoutingTable(/*curBit*/0) +=
Integer.parseInt(flipModify(myBinary,(curBit + 1)),2)
                                 curBit += 1
                             }
                             /* Routing Table End */

                             /* Leaf Build */
                              totNumPeers = 7           // setting the total number of peers to be 8, so old join code works fine.
                                  if (myPeerID + 1 > totNumPeers - 1) {
                                          ForwardLeaf += 0;
                                          ForwardLeaf += 1;
                                  } else if(myPeerID + 2 > totNumPeers -1){
                                          ForwardLeaf += (myPeerID + 1);
                                          ForwardLeaf += 0;
                                  } else {
                                          ForwardLeaf += (myPeerID + 1);
                                          ForwardLeaf += (myPeerID + 2);
                                          maxLeaf = myPeerID + 2;
                                  }

                                  if (myPeerID - 1 < 0) {
                                          ForwardLeaf += totNumPeers - 2;
                                          ForwardLeaf += totNumPeers - 1;
                                  } else if(myPeerID - 2 < 0){
                                          ForwardLeaf += (myPeerID - 1);
                                          ForwardLeaf += totNumPeers -1;
                                  } else {
                                          ForwardLeaf += (myPeerID - 1);
                                          ForwardLeaf += (myPeerID - 2);
                                          minLeaf = myPeerID - 2;
                                  }

                                  /* ForwardLeaf += (myPeerID + 1);
                                RearLeaf += (myPeerID + 2);*/

                                  /* Leaf Build  End*/


                          // BUILD BINARY (4DIGIT)
                                  sender ! "ready"
                                  InstigatorFellow ! "acknowledged"
                          } else { // Joining node by node after default 8 nodes.(join2)
                                   //send everyone in the current peer list to update their leafset and routing table accordingly.
                                   currPeers = x._4
                                /* Routing Table */
                                   //println(myPeerID+ " : Join2 for the node "+x._5)
                                   if( x._1 == x._5){// For join2 do this only if this is the new node getting added to network.
                                        //   println("received Join2 for node "+x._1)
                                           var curBit = 0
                                           while(curBit<binarysize){
                                                   RoutingTable(/*curBit*/0) += Integer.parseInt(flipModify(myBinary,(curBit + 1)),2)
                                           curBit += 1
                                           }
                                   }
                             /* Routing Table End */

                             /* Leaf Build */
                              totNumPeers = currPeers           // setting the total number of peers to be CurrPeers, so old join code works fine.
                              ForwardLeaf.clear
                              if (myPeerID + 1 > totNumPeers - 1) {
                                          ForwardLeaf += 0;
                                          ForwardLeaf += 1;
                                  } else if(myPeerID + 2 > totNumPeers -1){
                                          ForwardLeaf += (myPeerID + 1);
                                          ForwardLeaf += 0;
                                  } else {
                                          ForwardLeaf += (myPeerID + 1);
                                          ForwardLeaf += (myPeerID + 2);
                                          maxLeaf = myPeerID + 2;
                                  }

                                  if (myPeerID - 1 < 0) {
                                          ForwardLeaf += totNumPeers - 2;
                                          ForwardLeaf += totNumPeers - 1;
                                  } else if(myPeerID - 2 < 0){
                                          ForwardLeaf += (myPeerID - 1);
                                          ForwardLeaf += totNumPeers -1;
                                  } else {
                                          ForwardLeaf += (myPeerID - 1);
                                          ForwardLeaf += (myPeerID - 2);
                                          minLeaf = myPeerID - 2;
                                  }

                                  if( x._1 == x._5){
                                    sender ! "ready"
                                    InstigatorFellow ! "acknowledged"
                                  }
                          }

                         case x : (Int,Int,String,String,String,Int) =>
                          // println("recevied message passing")
                                if(x._3  == "MessagePassing"){
                                //  println("I (" + myBinary + ") got the start Q")
                                  /* Check Leaf Nodes*/
                                  if (x._1 == myPeerID) {
                                          println("Reached destination from "+ x._6 + " in "+x._2 +" hops")
                                          InstigatorFellow ! (x._6,x._2)
                                  } else {
                                        var nextPeer = checkifWithinLeafs(x._1)
                                        if (nextPeer != -999){ // not in leaf

                                                //      println("CAme Hereeefdsf " + nextPeer)
                                                        /* Send message to node at nodePeer loc */
                                                        //println("Forwarding to " + nextPeer)
                                                        //conn(connection_list(nextPeer)) ! (x._1,x._2 + 1 ,"MessagePassing","MessagePassing",x._5)
                                        				
                                        		while(conn(connection_list(nextPeer)).getState == scala.actors.Actor.State.Terminated){
                                        				  println(myPeerID +" : found a dead node: "+ nextPeer +" in the path. So rerouting.Destination is: "+ x._1)
                                        				  if(nextPeer == currPeers){
                                        				    nextPeer = 0
                                        				  } else{
                                        				    //println("####" + nextPeer)
                                        				    nextPeer = nextPeer +1
                                        				    //println("$$$$$" + nextPeer)
                                        				  }
                                        				}
                                        			//println("final sendinggg "+ nextPeer)
                                                println(myPeerID+ " " + (binaryfy(myPeerID))+" finally sending it to "+ nextPeer + ": " + (binaryfy(nextPeer))+ " ,dest " + x._1 + " : "+(binaryfy(x._1)) )      
                                        		conn(connection_list(nextPeer)) ! (x._1,x._2 + 1,"MessagePassing","abcd","abcd",x._6)

                                        } else { 
                                          /* check routing table */

                                          var len = RoutingTable(0).size
                                          var i=0
                                          var flag =0
                                          var curr = (binaryfy(myPeerID))
                                       //  while(i<len && (returnMatchingBits(curr,binaryfy(RoutingTable(0)(i))) < x._2)) {
                                        //      i +=1
                                         // }
                                           i = returnMatchingBits(curr,binaryfy(x._1))
                                          	if (i >= len) {
                                          		println(curr + " " + binaryfy(x._1)+ " "+ i + " " + len)
                                                //println("Didn't " + RoutingTable(0)) // probably here out logic og join
                                                  println(myPeerID+ " got stuck")
                                                } else {
                                                  nextPeer = RoutingTable(0)(i)
                                                  while(conn(connection_list(nextPeer)).getState == scala.actors.Actor.State.Terminated){
                                        				  println(myPeerID+ " : found a dead node : "+ nextPeer +" in the path in routing. So rerouting.")
                                        				  if(nextPeer == currPeers){
                                        				     nextPeer = 0
                                        				  } else{
                                        				    nextPeer = nextPeer + 1
                                        				  }
                                        				}
                                                  println(myPeerID+ " " + (binaryfy(myPeerID))+" finally sending it to "+ nextPeer + ": " + (binaryfy(nextPeer))+ " ,dest " + x._1 + " : "+(binaryfy(x._1)) )
                                                   conn(connection_list(nextPeer)) ! (x._1,x._2 + 1,"MessagePassing","abcd","abcd",x._6)
                                                }
                                        }
                                  }
                                }//case ending

                        }//react ending
                 }
         }
}

object project3bonus{
  def main(args: Array[String]) : Unit = {
    var numNodes:Double = 1024
    var numRequests:Int = 10
    var numkills:Int = 200
    if(args.length >1){
      println("Assuming arg0 is numNodes, arg1 is numRequests and arg2 is numkills")
     numNodes = args(0).toInt
     numRequests = args(1).toInt
     numkills = args(2).toInt

    }
    
    var x: Double = 0
    x = math.log(numNodes)/math.log(2)
    x = x.ceil
    numNodes = math.pow(2, x)
   // println("new numNodes is " + numNodes)

   var NetworkBuilderObj = new NetworkBuilderbonus(numNodes.toInt,numRequests,numkills)
   NetworkBuilderObj.start()
   NetworkBuilderObj ! "ready"
  }
}


class Instigatorbonus(numPeers:Int,full_buff:ArrayBuffer[Int],conn:ArrayBuffer[PastryNodebonus],numRequests:Int,numkills:Int)
extends Actor{
  var readyPeers:Int = 0
  var totNumPeers:Int = numPeers
  var i:Int = 0
  var sum = Array[Int](numPeers+1)
  var sum1 :Double = 0
  var r = new scala.util.Random
    var tempRand1 = r.nextInt(totNumPeers);
  var tempRand2 = r.nextInt(totNumPeers);
  var totalmessages:Int = numRequests*(numPeers)
  var rxmessages:Int = 0
  var j:Int = 0
  sum = Array.fill(numPeers+1)(0)
  var count:Double = 0

    def act(){
        loop {
                react{
                  /*    case x: ArrayBuffer[PastryNodebonus] =>
                        conn = x
                    */
                        case "acknowledged" =>
                                readyPeers += 1
                                if(readyPeers == totNumPeers) {
                                        //ready to send message
                                        println("Network Ready");
                                        //Tell Node
                                        j = 0
                                        //while(j<numRequests){
                                        		while(j<numkills){//kill randomly 10 nodes
                                        			tempRand1 = r.nextInt(totNumPeers-1)
                                        			println("killing node number : " + tempRand1)
                                        			conn(full_buff(tempRand1))! "exit"
                                        			j +=1
                                        		}
                                        		Thread.sleep(5000)
                                                i = 0
                                              while(i<numRequests){
                                                tempRand1 = r.nextInt(totNumPeers-1)
                                                tempRand2 = r.nextInt(totNumPeers-1)
                                                //while(i<totNumPeers){//nodes already killed.
                                                	while((conn(full_buff(tempRand1)).getState == scala.actors.Actor.State.Terminated)){
                                                	  tempRand1 = r.nextInt(totNumPeers-1)
                                                	}
                                        			
                                                	while((conn(full_buff(tempRand2)).getState == scala.actors.Actor.State.Terminated) || (tempRand1 == tempRand2)){
                                                	  tempRand2 = r.nextInt(totNumPeers-1)
                                                	}
                                                	//found two guys who are not dead
                       //                         	if((conn(full_buff(i)).getState != scala.actors.Actor.State.Terminated) && (conn(full_buff(tempRand1)).getState != scala.actors.Actor.State.Terminated))
                         //                       	{
                                                        //println(full_buff(i) + " sending messages to " + tempRand)
                                                        //conn(full_buff(i)) !(full_buff(tempRand),1,"MessagePassing","MessagePassing",full_buff(i))
                                                        println("source " +tempRand2 + " dest " + tempRand1)
                                                		conn(full_buff(tempRand2)) !(tempRand1,1,"MessagePassing","norm1","abcd",i)
                                                        //tempRand1 = r.nextInt(totNumPeers-1)
                                                        i += 1
                                                	
                                               }
                                                j +=1
                                        //}
                                }
                        case x:(Int,Int) =>
                          //    println("Instigator received "+x._1 + "  " + x._2 + " "+rxmessages + " " + totalmessages+ " "+ i)
                                count = count + x._2
                                rxmessages += 1

                                if(rxmessages == numRequests){
                                
                                 // println("$$$$$ get the program closed and exit..." + count)
                                  println("Done!!!!Average number of hops for passing "+ numRequests +" messages is :"+ count/numRequests)
                                  j= 0
                                  while(j<totNumPeers){
                                    conn(full_buff(j)) ! "exit"
                                    j += 1
                                  }
                                  exit()

                                }
                }
                }
        }
}


class NetworkBuilderbonus(numNodes1:Int,numRequests:Int,numkills:Int) extends Actor {
        //val numNodes = 127
        var numNodes = numNodes1-1
        var numPeers = numNodes  //Change number of bits from here
        var k:Int = numNodes-1     // for intitalizing our array  always -2
        val binarysize = (math.log(numPeers + 1)/math.log(2)).toInt
        var currPeers:Int = 0
        println("The size " + binarysize)
    var list = new ArrayBuffer[Int]()
    var full_buff = new ArrayBuffer[Int]()
    var m:Int = 0
    for(i <- 0 to k){
      list += i
    }


  //  var numNodesCount:Int = 126
    var r = new scala.util.Random
    var tempRand = r.nextInt(k);

    var range = 0 to (k);
        while(k >= 0){
                if (k != 0){
                        tempRand = r.nextInt(k)
                } else {
                        tempRand = 0
                }
                full_buff += list(tempRand)
                list.remove(tempRand)
                k -= 1
    }
    println(full_buff)
    // full_buff now has list of random nos
    var conn = new ArrayBuffer[PastryNodebonus](numNodes)

     // val numRequests = 10
      k = 0

       val instigator = new Instigatorbonus(numPeers,full_buff,conn,numRequests,numkills)
       instigator.start()

      // initialize all the connections (actors)
      while(k<numNodes){
         var newNode:PastryNodebonus = null
         newNode = new PastryNodebonus(instigator,binarysize)
         newNode.start()
         conn += newNode
         k += 1
      }
      instigator ! conn
      k = 0
      while(k<numNodes){
           conn(k)!(conn,k) //sending node Id
           k += 1
      }
      k =0
   //  self ! "ready"
   //  println("sending_to "+ k)
    /* Now we have random numbers*/
      while(k<numNodes){
        conn(full_buff(k)) ! (k,full_buff,"sending peer id and full rand array")
        k +=1
      }

      k=0
  def act(){
	  loop{
	    react{
	      case "ready" =>
                if (k<8) {//initialize 8 actors normally// each actor will return here after initialize so that next one can initialize
                    //println("Normal Node Bringup "+ k)
                    conn(full_buff(k)) ! (k,full_buff,"Join1",currPeers,currPeers) // we need to change this message to join so that apart from the calculations it is doing noe, it also does join.
                    k += 1
                    currPeers += 1
                } else if (k<numNodes) { //now do the join// each actor will return here after initialize so that next one can initialize
                    tempRand = r.nextInt(k-1)
                    //println("Node joining id :  "+ k)
                    //send the new currPeers to all nodes so far in peer network and ask them to update routing and leaf tables.
                    m = 0
                    while(m<=currPeers){
                            conn(full_buff(m)) ! (m,full_buff,"Join2",currPeers,k) // we need to change this message to join so that apart from the calculations it is doing noe, it also does join.
                            m +=1
                    }
                    k += 1
                    currPeers += 1
                }
                if(k == numNodes){
                	exit()
                }
	     	}
	   	}
	}
}
