package sryza

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Font, Color, GridLayout, BorderLayout}
import javax.swing.border.{LineBorder, BevelBorder}
import javax.swing._

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

object Colors {
  val CPU_COLOR = Color.pink
  val MEMORY_COLOR = Color.orange
}

class RootPanel(cluster: Cluster) extends JPanel {
  setLayout(new BorderLayout)
  val nodesPanel = new NodesPanel(cluster)
  add(nodesPanel, BorderLayout.CENTER)

  val statsPanel = new StatsPanel()
  val statsWrapperPanel = new WrapperPanel(statsPanel)
  statsWrapperPanel.setBorder(new BevelBorder(BevelBorder.RAISED))
  add(statsWrapperPanel, BorderLayout.SOUTH)

  val queryPanel = new QueryPanel(cluster)
  val queryPanelWrapper = new WrapperPanel(queryPanel)
  queryPanelWrapper.setBorder(new BevelBorder(BevelBorder.RAISED))
  add(queryPanelWrapper, BorderLayout.NORTH)

  val nodeStatsPanel = new NodeStatsPanel(cluster)
  add(nodeStatsPanel, BorderLayout.WEST)
}

class StatsPanel extends JPanel {
  val gridLayout = new GridLayout(4, 4)
  setLayout(gridLayout)
  gridLayout.setVgap(2)
  gridLayout.setHgap(2)

  private def valueLabel(labelStr: String, color: Color = Color.lightGray): JLabel = {
    val labelLabel = new JLabel(s"<html><b>$labelStr </b></html>")
    labelLabel.setHorizontalAlignment(SwingConstants.RIGHT)
    add(labelLabel)
    val label = new JLabel("")
    label.setBackground(color)
    label.setOpaque(true)
    label.setHorizontalAlignment(SwingConstants.CENTER)
    label.setBorder(new LineBorder(Color.black, 1))
    add(label)
    label
  }

  val versionLabel = valueLabel("Version:")
  val featuresLabel = valueLabel("Features:")
  val nodesLabel = valueLabel("Nodes:")
  val coinLabel = valueLabel("Coin:", Color.yellow)
  val coresLabel = valueLabel("Cluster Cores:", Colors.CPU_COLOR)
  val memoryLabel = valueLabel("Cluster Memory:", Colors.MEMORY_COLOR)
  val completedQueries = valueLabel("Completed Queries:")
  val failedQueries = valueLabel("Missed Queries:")

  def updateStats(cluster: Cluster): Unit = {
    versionLabel.setText(cluster.versionName)
    featuresLabel.setText(cluster.features.mkString(", "))
    nodesLabel.setText(cluster.nodes.length.toString)
    coinLabel.setText(cluster.coin.toString)
    coresLabel.setText(cluster.totalCores.toString)
    memoryLabel.setText(cluster.totalMemory + " GB")
    completedQueries.setText(cluster.completedQueries.toString)
    failedQueries.setText(cluster.missedQueries.toString)
  }
}

class QueryPanel(cluster: Cluster) extends JPanel {
  setLayout(new GridLayout(4, 1))

  val queryPanel = new JPanel(new GridLayout(1, 2))
  add(queryPanel)
  val requirementsPanel = new JPanel(new GridLayout(1, 6))
  requirementsPanel.getLayout.asInstanceOf[GridLayout].setHgap(2)
  add(requirementsPanel)

  val label = new JLabel("<html><b>Query:</b></html>")
  label.setHorizontalAlignment(SwingConstants.RIGHT)
  queryPanel.add(label)

  val textField = new JTextField()
  textField.setEditable(false)
  textField.setFont(new Font("Default", Font.PLAIN, 16))
  queryPanel.add(textField)

  def valueLabel(labelStr: String, parentPanel: JPanel, color: Color = Color.lightGray): JLabel = {
    val labelLabel = new JLabel(s"<html><b>$labelStr </b></html>")
    labelLabel.setHorizontalAlignment(SwingConstants.RIGHT)
    parentPanel.add(labelLabel)
    val label = new JLabel("")
    label.setBackground(color)
    label.setOpaque(true)
    label.setHorizontalAlignment(SwingConstants.CENTER)
    label.setBorder(new LineBorder(Color.black, 1))
    parentPanel.add(label)
    label
  }

  val coresLabel = valueLabel("Required Cores:", requirementsPanel, Colors.CPU_COLOR)
  val memoryLabel = valueLabel("Required Memory:", requirementsPanel, Colors.MEMORY_COLOR)
  val featuresLabel = valueLabel("Required Features:", requirementsPanel)

  val progressBar = new JProgressBar()
  add(progressBar)

  val executeButton = new JButton("Execute Query")
  executeButton.setEnabled(false)
  executeButton.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      cluster.executeQuery()
    }
  })
  add(executeButton)

  var curQuery: Option[Query] = None

  def updateQuery(): Unit = {
    import Color._
    val query = cluster.curQuery
    if (query != curQuery) {
      curQuery = query
      textField.setText(query.map(_.sql).getOrElse(""))
      coresLabel.setText(query.map(_.cores.toString).getOrElse(""))
      coresLabel.setForeground(
        if (query.map(_.cores > cluster.totalCores).getOrElse(false)) red else black)
      memoryLabel.setText(query.map(_.memory + " GB").getOrElse(""))
      memoryLabel.setForeground(
        if (query.map(_.memory > cluster.totalMemory).getOrElse(false)) red else black)
      featuresLabel.setText(query.map(_.features.mkString(",")).getOrElse(""))
      featuresLabel.setForeground(
        if (query.map(!_.features.forall(cluster.features.contains(_))).getOrElse(false))
          red else black)
      progressBar.setMinimum(0)
      progressBar.setMaximum(query.map(_.duration).getOrElse(1))
      executeButton.setEnabled(cluster.canExecuteCurrentQuery && !cluster.queryExecuting)
    }

    progressBar.setValue(cluster.curQueryStartTime
      .map(s => (System.currentTimeMillis - s).toInt).getOrElse(0))
  }
}

class NodeStatsPanel(cluster: Cluster) extends JPanel {
  setBorder(new BevelBorder(BevelBorder.RAISED))

  val innerPanel = new JPanel(new BorderLayout)
  val label = new JLabel("<html><b>Node Profile</html></b>")
  label.setHorizontalAlignment(SwingConstants.CENTER)
  innerPanel.add(label, BorderLayout.NORTH)
  val gridPanel = new JPanel(new GridLayout(6, 2))
  gridPanel.getLayout.asInstanceOf[GridLayout].setVgap(2)
  gridPanel.setBorder(new LineBorder(Color.black, 1))

  private def valueLabel(labelStr: String, color: Color = Color.lightGray): JLabel = {
    val labelLabel = new JLabel(labelStr)
    labelLabel.setHorizontalAlignment(SwingConstants.RIGHT)
    gridPanel.add(labelLabel)
    val label = new JLabel(" " * 35)
    label.setBackground(color)
    label.setOpaque(true)
    label.setHorizontalAlignment(SwingConstants.CENTER)
    gridPanel.add(label)
    label
  }
  val nameLabel = valueLabel("Name:")
  val coresLabel = valueLabel("Cores:", Colors.CPU_COLOR)
  val memoryLabel = valueLabel("Memory:", Colors.MEMORY_COLOR)
  val birthDateLabel = valueLabel("Birth Date:")
  val lastMatedLabel = valueLabel("Last Mate Date:")
  val healthLabel = valueLabel("Health:")

  innerPanel.add(gridPanel, BorderLayout.CENTER)

  val repairButton = new JButton("Repair (10 Coin)")
  repairButton.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      if (cluster.coin >= 10) {
        selectedNode.get.healthy = true
        cluster.coin -= 10
      } else {
        JOptionPane.showMessageDialog(NodeStatsPanel.this, "Not enough coin to repair!")
      }
    }
  })
  val upgradeClusterButton = new JButton(s"Upgrade Cluster (${cluster.upgradeCost} Coin)")
  upgradeClusterButton.addActionListener(new ActionListener() {
    override def actionPerformed(e: ActionEvent): Unit = {
      if (cluster.coin >= cluster.upgradeCost) {
        val newFeatures = cluster.upgrade()
        upgradeClusterButton.setText(s"Upgrade Cluster (${cluster.upgradeCost} Coin)")
        JOptionPane.showMessageDialog(null,
          s"Your cluster is now running ${cluster.versionName}!\n\n" +
          s"It has learned the feature(s) ${newFeatures.mkString(",")}.")
      } else {
        JOptionPane.showMessageDialog(NodeStatsPanel.this, "Not enough coin to upgrade!")
      }
    }
  })

  val buttonPanel = new JPanel(new GridLayout(2, 1))
  buttonPanel.add(repairButton)
  buttonPanel.add(upgradeClusterButton)

  innerPanel.add(buttonPanel, BorderLayout.SOUTH)
  add(innerPanel)

  val format = DateTimeFormat.forPattern("hh:mm a")
  var selectedNode: Option[Node] = None

  def updateSelectedNode(node: Option[Node]): Unit = {
    selectedNode = node
    nameLabel.setText(node.map(_.name).getOrElse(""))
    coresLabel.setText(node.map(_.cores.toString).getOrElse(""))
    memoryLabel.setText(node.map(_.memory + " GB").getOrElse(""))
    birthDateLabel.setText(node.map(x => new DateTime(x.birthDate).toString(format)).getOrElse(""))
    lastMatedLabel.setText(node.filter(_.lastMating > 0).map(
      x => new DateTime(x.lastMating).toString(format)).getOrElse(""))
    healthLabel.setText(node.map(x => if (x.healthy) "Good" else "Poor").getOrElse(""))
    repairButton.setEnabled(node.map(!_.healthy).getOrElse(false))
  }
}

class WrapperPanel(panel: JComponent) extends JPanel {
  add(panel)
}