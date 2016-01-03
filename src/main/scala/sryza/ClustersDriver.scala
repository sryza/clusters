package sryza

import java.awt.Cursor
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._

object ClustersDriver {
  def main(args: Array[String]): Unit = {
    val splashIcon = new ImageIcon(getClass.getResource("/splash.png"))
    JOptionPane.showMessageDialog(null, "", "Clusters", JOptionPane.INFORMATION_MESSAGE, splashIcon)

    val cluster = new Cluster()
    val rootPanel = new RootPanel(cluster)

    val menuBar = new JMenuBar()
    val menu = new JMenu("Help")
    menuBar.add(menu)
    val menuItem = new JMenuItem("About")
    menuItem.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        JOptionPane.showMessageDialog(null,
          "Clusters is an artificial life computer program.  Gameplay focuses on raising alien\n" +
          "creatures known as Nodes.  Nodes mate, pass on their genes, get sick, execute\n" +
          "queries.")
      }
    })
    menu.add(menuItem)

    val timer = new Timer(50, new ActionListener() {
      override def actionPerformed(event: ActionEvent) = {
        cluster.gameStep()
        rootPanel.nodesPanel.repaint()
        rootPanel.statsPanel.updateStats(cluster)
        rootPanel.nodeStatsPanel.updateSelectedNode(rootPanel.nodesPanel.selectedNode)
        rootPanel.queryPanel.updateQuery()
      }
    })
    timer.start()

    val frame = new JFrame()
    frame.setJMenuBar(menuBar)
    frame.setTitle("Clusters")
    frame.setCursor(Cursor.HAND_CURSOR)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setContentPane(rootPanel)
    frame.pack()
    frame.setVisible(true)
  }
}
