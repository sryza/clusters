package sryza

import java.awt.event._
import java.awt.geom.AffineTransform
import java.awt.image.{AffineTransformOp, BufferedImage}
import java.awt._
import javax.imageio.ImageIO
import javax.swing.JPanel
import java.io.File

class NodesPanel(val cluster: Cluster) extends JPanel {
  import NodesPanel._
  setPreferredSize(new Dimension(WIDTH, HEIGHT))

  val nodeImagesLeft = (1 to 3).map(x =>
    transparentImage(ImageIO.read(getClass.getResource(s"/node$x.jpeg"))))

  val sickImage = transparentImage(ImageIO.read(getClass.getResource("/sick.jpeg")))
  val backgroundImage = ImageIO.read(getClass.getResource("/background.png"))

  def scaleImage(image: BufferedImage, width: Int, height: Int): BufferedImage = {
    val after = new BufferedImage(image.getWidth, image.getHeight,
      BufferedImage.TYPE_INT_ARGB)
    val at = new AffineTransform()
    at.scale(width.toDouble / image.getWidth, height.toDouble / image.getHeight)
    val scaleOp = new AffineTransformOp(at, AffineTransformOp.TYPE_BILINEAR)
    scaleOp.filter(image, after)
  }

  def transparentImage(image: BufferedImage): BufferedImage = {
    val imageClone = new BufferedImage(image.getWidth(), image.getHeight(),
      BufferedImage.TYPE_INT_ARGB)
    val imageCloneG = imageClone.createGraphics()
    imageCloneG.drawImage(image, 0, 0, null)
    imageCloneG.setComposite(AlphaComposite.getInstance(AlphaComposite.DST_IN, 0.5f))
    val width = image.getWidth()
    val height = image.getHeight()
    val raster = imageClone.getRaster()

    val background = imageClone.getRGB(0, 0)
    for (x <- 0 until width; y <- 0 until height) {
      val rgb = imageClone.getRGB(x, y)
      val r = rgb
      if (rgb == background) {
        val transparent = rgb & 0x00ffffff
        imageClone.setRGB(x, y, transparent)
      }
    }
    imageClone
  }

  override def paint(g: Graphics): Unit = {
    import Color._
    val graphics = g.asInstanceOf[Graphics2D]

    graphics.drawImage(backgroundImage, 0, 0, null)

    cluster.nodes.foreach { node =>
      val nodeImage = if (!node.healthy) {
        sickImage
      } else {
        nodeImagesLeft(node.imageId)
      }
      val flip = !node.facingLeft && node.healthy
      val width = if (flip) -node.width else node.width
      val x = node.x + (if (flip) node.width / 2 else -node.width / 2)
      graphics.drawImage(nodeImage, x, node.y - node.height / 2, width,
        node.height, null)

      if (cluster.queryExecuting && node.healthy) {
        graphics.setColor(orange)
        graphics.setStroke(new BasicStroke(3))
        graphics.drawRect(node.x - node.width / 2, node.y - node.height / 2, node.width,
          node.height)
      }

      if (selectedNode.exists(node == _)) {
        graphics.setColor(green)
        graphics.setStroke(new BasicStroke(5))
        graphics.drawRect(node.x - node.width / 2, node.y - node.height / 2, node.width,
          node.height)
      }
    }
  }

  var draggedNode: Option[Node] = None
  var selectedNode: Option[Node] = None

  addMouseListener(new MouseAdapter() {
    def findNode(e: MouseEvent): Option[Node] = {
      val x = e.getX
      val y = e.getY
      cluster.nodes.find { node =>
        x < node.x + node.width / 2 && x > node.x - node.width / 2 &&
        y < node.y + node.height / 2 && y > node.y - node.height / 2
      }
    }

    override def mouseClicked(e: MouseEvent): Unit = {
      selectedNode = findNode(e)
    }

    override def mousePressed(e: MouseEvent): Unit = {
      draggedNode = findNode(e)
    }

    override def mouseReleased(e: MouseEvent): Unit = {
      draggedNode = None
    }
  })

  addMouseMotionListener(new MouseMotionAdapter {
    override def mouseDragged(e: MouseEvent): Unit = {
      draggedNode.foreach { node =>
        node.x = e.getX
        node.y = e.getY
      }
    }
  })
}

object NodesPanel {
  val NODE_WIDTH = 100
  val NODE_HEIGHT = 85
  val WIDTH = 829
  val HEIGHT = 579
}