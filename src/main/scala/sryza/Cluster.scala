package sryza

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Cluster {
  val MILLIS_PER_MINUTE = 1000.0 * 60

  val QUERIES_PER_MINUTE = 2000
  val QUERIES_PER_MILLI = QUERIES_PER_MINUTE / MILLIS_PER_MINUTE

  val MATINGS_PER_MINUTE = 10
  val MATINGS_PER_MILLI = MATINGS_PER_MINUTE / MILLIS_PER_MINUTE
  val MIN_MATING_INTERVAL = MILLIS_PER_MINUTE * 2
  val MAX_MATING_DISTANCE = 120

  val SICKNESSES_PER_MINUTE = .5
  val SICKNESSES_PER_MILLI = SICKNESSES_PER_MINUTE / MILLIS_PER_MINUTE

  val UPGRADE_COSTS = Array(30, 50, 80, 100, 100, 100, 100, 100)

  val QUERY_QUEUE_TIME = 3750
}

class Cluster {
  import Cluster._

  val rand = new Random
  val nodes = new ArrayBuffer[Node]

  // Capabilities and resources
  var coin: Int = 20
  var versionNumber: Int = 0
  var features: Set[String] = Set()

  // Current query
  var curQuery: Option[Query] = None
  var curQueryQueueTime: Option[Long] = None
  var curQueryStartTime: Option[Long] = None
  var canExecuteCurrentQuery: Boolean = false
  var queryExecuting: Boolean = false

  // Stats
  var completedQueries: Int = 0
  var missedQueries: Int = 0

  // Derived attributes
  def totalCores: Int = nodes.filter(_.healthy).foldLeft(0)(_ + _.cores)
  def totalMemory: Int = nodes.filter(_.healthy).foldLeft(0)(_ + _.memory)
  def upgradeCost: Int = UPGRADE_COSTS(versionNumber)
  def versionName: String = s"CDH 5.$versionNumber"

  // Initialize game with some nodes
  val initNode1 = new Node(Node.randName(rand), 4, 10, System.currentTimeMillis, 0)
  initNode1.x = NodesPanel.WIDTH / 2
  initNode1.y = NodesPanel.HEIGHT / 2
  val initNode2 = new Node(Node.randName(rand), 5, 8, System.currentTimeMillis, 1)
  initNode2.x = NodesPanel.WIDTH / 2
  initNode2.y = NodesPanel.HEIGHT / 2
  nodes += initNode1
  nodes += initNode2

  // Timer state
  var lastTick: Long = -1

  def gameStep(): Unit = {
    val now = System.currentTimeMillis
    if (lastTick == -1) {
      lastTick = now
      return
    }

    // Some nodes deteriorate in health
    val sickProb = (now - lastTick) * SICKNESSES_PER_MILLI
    nodes.foreach { node =>
      if (rand.nextDouble() < sickProb) {
        node.healthy = false
      }
    }

    // Nodes move
    nodes.foreach { node =>
      val xSign = if (node.healthy) rand.nextInt(3) - 1 else 0
      val ySign = if (node.healthy) rand.nextInt(3) - 1 else 0
      val magnitude = 10
      node.x = node.x + magnitude * xSign
      node.y = node.y + magnitude * ySign
      if (xSign < 0) {
        node.facingLeft = true
      } else if (xSign > 0) {
        node.facingLeft = false
      }

      node.x = math.min(node.x, NodesPanel.WIDTH - node.width / 2)
      node.x = math.max(node.x, node.width / 2)
      node.y = math.min(node.y, NodesPanel.HEIGHT - node.height / 2)
      node.y = math.max(node.y, node.height / 2)
    }

    // Nodes possibly mate
    val mateProb = (now - lastTick) * MATINGS_PER_MILLI
    if (rand.nextDouble() < mateProb) {
      val len = nodes.length
      for (i <- 0 until len; j <- i + 1 until len) {
        val node1 = nodes(i)
        val node2 = nodes(j)
        if (node1.healthy && node2.healthy &&
            math.abs(node1.x - node2.x) < MAX_MATING_DISTANCE &&
            math.abs(node1.y - node2.y) < MAX_MATING_DISTANCE &&
            now - MIN_MATING_INTERVAL > node1.lastMating &&
            now - MIN_MATING_INTERVAL > node2.lastMating) {
          node1.lastMating = now
          node2.lastMating = now

          val newNode = mate(node1, node2, now)
          nodes += newNode
        }
      }
    }

    // Query comes in with some probability
    val queryProb = (now - lastTick) * QUERIES_PER_MILLI
    if (curQuery.isEmpty && rand.nextDouble() < queryProb) {
      // Queue new query
      val query = randQuery()
      curQuery = Some(query)
      curQueryQueueTime = Some(now)
      canExecuteCurrentQuery = query.cores <= totalCores && query.memory <= totalMemory &&
        query.features.forall(features.contains(_))
    } else if (queryExecuting && now - curQuery.get.duration > curQueryStartTime.get) {
      // Complete query
      coin += curQuery.get.value
      completedQueries += 1
      clearQuery()
    } else if (!queryExecuting && curQuery.nonEmpty
        && now - QUERY_QUEUE_TIME > curQueryQueueTime.get) {
      // Discard unused query
      missedQueries += 1
      clearQuery()
    }

    lastTick = now
  }

  def clearQuery(): Unit = {
    curQuery = None
    queryExecuting = false
    canExecuteCurrentQuery = false
    curQueryQueueTime = None
    curQueryStartTime = None
  }

  def executeQuery(): Unit = {
    assert(curQuery.nonEmpty)
    assert(canExecuteCurrentQuery)
    queryExecuting = true
    curQueryStartTime = Some(System.currentTimeMillis)
  }

  def randQuery(): Query = {
    import Query.ALL_FEATURES
    val sql = "SELECT * FROM orders"
    def randResource(cap: Int): Int = {
      math.max(2, (cap * .75 + rand.nextGaussian() * cap * 5 / 16).toInt)
    }

    val cores = randResource(totalCores)
    val memory = randResource(totalMemory)
    val duration = 2500 * (rand.nextInt(5) + 1)
    val features = ALL_FEATURES.filter(x => rand.nextDouble() < 1.0 / ALL_FEATURES.length)
    new Query(sql, cores, memory, duration, features)
  }

  def mate(mom: Node, dad: Node, now: Long): Node = {
    val cores = math.max(1,
      math.round((mom.cores + dad.cores) / 2.0 + rand.nextDouble() * 3.0 - 1.5)).toInt
    val memory = math.max(1,
      math.round((mom.memory + dad.memory) / 2.0 + rand.nextDouble() * 5.0 - 2.5)).toInt
    val node = new Node(Node.randName(rand), cores, memory, now, rand.nextInt(3))
    node.x = (mom.x + dad.x) / 2
    node.y = (mom.y + dad.y) / 2
    node
  }

  /**
   * Returns the set of features learned in the upgrade.
   */
  def upgrade(): Set[String] = {
    import Query.ALL_FEATURES
    coin -= upgradeCost
    versionNumber += 1
    if (features.size < ALL_FEATURES.length) {
      val denom = ALL_FEATURES.length - features.size
      val newFeatures = ALL_FEATURES.filter(x => rand.nextDouble() < 1.0 / denom)
        .filter(!features.contains(_)).toSet
      features ++= newFeatures
      newFeatures
    } else {
      Set.empty
    }
  }
}