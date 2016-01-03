package sryza

import scala.util.Random

object Node {
  val NAMES = Array(
    "Min Max: Node Warrior",
    "Nody DeAraujo",
    "bb-7",
    "Backender Wiggin",
    "Node to Joy",
    "Node Blooded",
    "HP Lovecraft",
    "Boxie",
    "Edward Noden",
    "Baron Harnoden",
    "Colossus of Nodes"
  )

  def randName(rand: Random): String = {
    NAMES(rand.nextInt(NAMES.length))
  }
}

class Node(
    val name: String,
    val cores: Int,
    val memory: Int,
    val birthDate: Long,
    val imageId: Int) {

  var x: Int = 0
  var y: Int = 0
  var facingLeft: Boolean = true
  var power: Int = 0
  var healthy: Boolean = true
  var lastMating: Long = 0

  def width: Int = NodesPanel.NODE_WIDTH
  def height: Int = NodesPanel.NODE_HEIGHT
}
