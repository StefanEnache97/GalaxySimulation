package barneshut

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{BorderLayout, GridLayout}
import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing._

object BarnesHut {

  val model = new SimulationModel

  var simulator: Simulator = _

  def initialize(parallelismLevel: Int, pattern: String, nbodies: Int, dist: String): Unit = {
    model.initialize(parallelismLevel, pattern, nbodies, dist)
    model.timeStats.clear()
    simulator = new Simulator(model.taskSupport, model.timeStats)
  }

  class BarnesHutFrame extends JFrame("Barnes-Hut") {
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    setSize(1024, 600)
    setLayout(new BorderLayout)

    val rightpanel = new JPanel
    rightpanel.setBorder(BorderFactory.createEtchedBorder(border.EtchedBorder.LOWERED))
    rightpanel.setLayout(new BorderLayout)
    add(rightpanel, BorderLayout.EAST)

    val controls = new JPanel
    controls.setLayout(new GridLayout(0, 2))
    rightpanel.add(controls, BorderLayout.NORTH)

    val parallelismLabel = new JLabel("Parallelism")
    controls.add(parallelismLabel)


    val items = (1 to Runtime.getRuntime.availableProcessors).map(_.toString).toArray
    val parcombo = new JComboBox[String](items)
    parcombo.setSelectedIndex(items.length - 1)
    parcombo.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) = {
        initialize(getParallelism, getPattern, getTotalBodies, getDistribution)
        canvas.repaint()
      }
    })
    controls.add(parcombo)




    val bodiesLabel = new JLabel("Total bodies")
    controls.add(bodiesLabel)

    val bodiesSpinner = new JSpinner(new SpinnerNumberModel(5000, 32, 1000000, 1000))
    bodiesSpinner.addChangeListener(new ChangeListener {
      def stateChanged(e: ChangeEvent) = {
        if (frame != null) {
          initialize(getParallelism, getPattern, getTotalBodies, getDistribution)
          canvas.repaint()
        }
      }
    })
    controls.add(bodiesSpinner)


    val paternsLabel = new JLabel("Galaxy Pattern")
    controls.add(paternsLabel)

    val patterns = Array("one-galaxy", "two-galaxies")
    val patcombo = new JComboBox[String](patterns)
    patcombo.setSelectedIndex(0)
    patcombo.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        initialize(getParallelism, getPattern, getTotalBodies, getDistribution)
        canvas.repaint()
      }
    })
    controls.add(patcombo)


    val distLabel = new JLabel("Star Distribution")
    controls.add(distLabel)

    val distributions = Array("uniform", "gaussian")
    val distcombo = new JComboBox[String](distributions)
    distcombo.setSelectedIndex(0)
    distcombo.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        initialize(getParallelism, getPattern, getTotalBodies, getDistribution)
        canvas.repaint()
      }
    })
    controls.add(distcombo)




    val stepbutton = new JButton("Step")
    stepbutton.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        stepThroughSimulation()
      }
    })
    controls.add(stepbutton)

    val startButton = new JToggleButton("Start/Pause")
    val startTimer = new javax.swing.Timer(0, new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        stepThroughSimulation()
      }
    })
    startButton.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        if (startButton.isSelected) startTimer.start()
        else startTimer.stop()
      }
    })
    controls.add(startButton)

    val quadcheckbox = new JToggleButton("Show quad")
    quadcheckbox.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        model.shouldRenderQuad = quadcheckbox.isSelected
        repaint()
      }
    })
    controls.add(quadcheckbox)

    val clearButton = new JButton("Restart")
    clearButton.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        initialize(getParallelism, getPattern, getTotalBodies, getDistribution)
      }
    })
    controls.add(clearButton)

    val info = new JTextArea("   ")
    info.setBorder(BorderFactory.createLoweredBevelBorder)
    rightpanel.add(info, BorderLayout.SOUTH)

    val canvas = new SimulationCanvas(model)
    add(canvas, BorderLayout.CENTER)
    setVisible(true)

    def updateInformationBox(): Unit = {
      val text = model.timeStats.toString
      frame.info.setText("--- Statistics: ---\n" + text)
    }

    def stepThroughSimulation(): Unit = {
      SwingUtilities.invokeLater(new Runnable {
        def run() = {
          val (bodies, quad) = simulator.step(model.bodies)

          model.bodies = bodies
          model.quad = quad
          updateInformationBox()
          repaint()
        }
      })
    }

    def getPattern: String = {
      val selidx = patcombo.getSelectedIndex
      patcombo.getItemAt(selidx)
    }

    def getParallelism = {
      val selidx = parcombo.getSelectedIndex
      parcombo.getItemAt(selidx).toInt
    }

    def getDistribution: String = {
      val selidx = distcombo.getSelectedIndex
      distcombo.getItemAt(selidx)
    }

    def getTotalBodies = bodiesSpinner.getValue.asInstanceOf[Int]

    initialize(getParallelism, getPattern, getTotalBodies, getDistribution)
  }

  try {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
  } catch {
    case _: Exception => println("Cannot set look and feel, using the default one.")
  }

  val frame = new BarnesHutFrame

  //  def main(args: Array[String]): Unit = {
  //    frame.repaint()
  //  }


}
