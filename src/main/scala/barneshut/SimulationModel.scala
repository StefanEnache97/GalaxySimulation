package barneshut

import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import scala.annotation.tailrec
import scala.collection.parallel.{TaskSupport, defaultTaskSupport}
import scala.util.Random
import scala.{collection => coll}

class SimulationModel {

  var screen = new Boundaries

  var bodies: coll.Seq[Body] = Nil

  var quad: Quad = Empty(screen.centerX, screen.centerY, Float.MaxValue)

  var shouldRenderQuad = false

  var timeStats = new TimeStatistics

  var taskSupport: TaskSupport = defaultTaskSupport


  def initialize(parallelismLevel: Int, pattern: String, totalBodies: Int, dist: String): Unit = {
    taskSupport = new collection.parallel.ForkJoinTaskSupport(
      new java.util.concurrent.ForkJoinPool(parallelismLevel))

    dist match {
      case "uniform" => {
        pattern match {
          case "two-galaxies" => init2Galaxies(totalBodies, uniformDistribution)
          case "one-galaxy" => init1Galaxy(totalBodies, uniformDistribution)
          case _ => sys.error(s"no such initial pattern: $pattern")
        }
      }
      case "gaussian" =>  {
        pattern match {
          case "two-galaxies" => init2Galaxies(totalBodies, gaussianDistribution)
          case "one-galaxy" => init1Galaxy(totalBodies, gaussianDistribution)
          case _ => sys.error(s"no such initial pattern: $pattern")
        }
      }
      case _ => sys.error(s"no such distribution: $dist")
    }


  }


    def uniformDistribution(maxradius: Float, random: Random): Float = {
      25 + maxradius * random.nextFloat
    }

    def gaussianDistribution(maxradius: Float, random: Random): Float = {
      def gaussRandom(): Float = {
        val u = 2*random.nextFloat() - 1
        val v = 2*random.nextFloat() - 1
        val r = u*u + v*v
        if(r == 0 || r >= 1) return gaussRandom()
        u*math.sqrt(-2*math.log(r)/r).toFloat
      }

      def sampleNormalDistribution(mean: Float, std_dev: Float): Float ={
        mean + gaussRandom()*std_dev
      }

      def oneTailNormalDistribution(mean: Float = 0, std_dev: Float = maxradius, range: Float = maxradius): Float = {
        math.abs(sampleNormalDistribution(mean, std_dev) % (range - 25)) + 25
      }

      oneTailNormalDistribution()

    }


    def galaxy(from: Int, num: Int, bodyArray: Array[Body], distribution: (Float, Random) => Float, maxradius: Float, cx: Float, cy: Float, sx: Float, sy: Float): Unit = {

        val random = new scala.util.Random(213L)

        val totalM = 1.5f * num
        val blackHoleM = 1.0f * num
        val cubmaxradius = maxradius * maxradius * maxradius
        for (i <- from until (from + num)) {
          val b = if (i == from) {
            new Body(blackHoleM, cx, cy, sx, sy)
          } else {
            val angle = random.nextFloat * 2 * math.Pi
            val radius = distribution(maxradius, random)
            val starx = cx + radius * math.sin(angle).toFloat
            val stary = cy + radius * math.cos(angle).toFloat
            val speed = math.sqrt(gee * blackHoleM / radius + gee * totalM * radius * radius / cubmaxradius)
            val starspeedx = sx + (speed * math.sin(angle + math.Pi / 2)).toFloat
            val starspeedy = sy + (speed * math.cos(angle + math.Pi / 2)).toFloat
            val starmass = 1.0f + 1.0f * random.nextFloat
            new Body(starmass, starx, stary, starspeedx, starspeedy)
          }
          bodyArray(i) = b
        }
    }


  def init1Galaxy(totalBodies: Int, distribution: (Float, Random) => Float = uniformDistribution): Unit = {

    val bodyArray = new Array[Body](totalBodies)

    galaxy(0, bodyArray.length, bodyArray, distribution, 300.0f, -1250.0f, -600.0f, 0.0f, 0.0f)

    bodies = bodyArray.toSeq

    // compute center and boundaries
    screen = new Boundaries
    screen.minX = -2200.0f
    screen.minY = -1600.0f
    screen.maxX = 350.0f
    screen.maxY = 350.0f
  }


  def init2Galaxies(totalBodies: Int, distribution: (Float, Random) => Float = uniformDistribution): Unit = {
    val bodyArray = new Array[Body](totalBodies)

    galaxy(0, bodyArray.length / 8, bodyArray, distribution, 300.0f, 0.0f, 0.0f, 0.0f, 0.0f)
    galaxy(bodyArray.length / 8, bodyArray.length / 8 * 7, bodyArray, distribution, 350.0f, -1800.0f, -1200.0f, 0.0f, 0.0f)

    bodies = bodyArray.toSeq

    // compute center and boundaries
    screen = new Boundaries
    screen.minX = -2200.0f
    screen.minY = -1600.0f
    screen.maxX = 350.0f
    screen.maxY = 350.0f
  }

}
