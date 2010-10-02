package net.lichtd.ray.output


import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import net.lichtd.ray.scene._
import java.util.concurrent.{Executors,TimeUnit}
import net.lichtd.ray.math._
import net.lichtd.ray.shapes.Shape

class Screen(_viewPoint: ViewPoint, _ambientLight: Color, val width: Int, val height: Int, val aspectRatio: Double)
        extends Scene(_viewPoint, _ambientLight) {
  def this(viewPoint: ViewPoint, ambientLight: Color, width: Int, height: Int) = this (viewPoint, ambientLight, width, height, 1.33)

  var subPixelResolution : Int = 1      // number of rays cast per pixel per dimension (uniformly distributed)
  var edgeDetection : Boolean = false;  // use subPixelResolution only at edges

  final val deltaCol : Vector = viewPoint.right / (width.toDouble / viewPoint.viewPlaneDistance / aspectRatio)
  final val deltaRow : Vector = viewPoint.up / (height.toDouble / viewPoint.viewPlaneDistance)
  final val data = new Array[Array[Int]](height, width)
  final val reporter = new ProgressReporter(height, 60)

  def getRay(x: Double, y: Double) : Ray =
                new Ray(viewPoint.origin,
                  (viewPoint.lookingAt
                    + (deltaCol * (-width / 2 + x))
                    + (deltaRow * (-height / 2 + y))
                  ) - viewPoint.origin
                )

  class LineRenderer(val imageWriter: ImageWriter, val line: Int) extends Runnable {
    def run = {
      val w = width
      val pixels = new Array[Int](w)
      var x = 0
      var last: LightResult = null
      var inRecalc = 0 // in recalculation after edge detection?
      while (x < w) {
        var pixel : Color = Color.BLACK
        var dy = 0
        // Subpixel resolution of 1 means one sample in the center,
        // 2 means 2x2 samples starting in the top left corner, excluding the bottom right border, and so on
        val res =
          if (edgeDetection && inRecalc <= 0 && x > 0) {
            // cast primary ray first
            val primary = cast(getRay(
                x.toDouble - 0.5,
                line - 0.5
            ))
          if (last != null && primary.shape == last.shape && primary.visibleLightSources == last.visibleLightSources) {
              // use match color, assume we're still painting the same shape
              pixel = primary.color
              0
            } else {
              x -= Math.min(x, 3)
              inRecalc = 5   // don't do edge detection for 2 pixels
              subPixelResolution
            }
          } else {
            subPixelResolution
          }
        val xDouble = x.toDouble
        if (res > 0) {
          val steps = res
          while (dy < res) {
            val y = dy.toDouble / steps
            var dx = 0
            while (dx < res) {
              val primary = cast(getRay(
                  xDouble - 0.5 + dx.toDouble / steps,
                  line - 0.5 + y
              ))
              pixel += primary.color / (res * res)
              last = primary  // store last ray for edge detection
              dx += 1
            }
            dy += 1
          }
        }
        pixels(x) = imageWriter.toInternal(pixel)
        x += 1
        inRecalc -= 1
      }
      put(line, pixels)
      //println("Rendered line " + line)
    }
  }

  def render[T <: ImageWriter](imageWriter: T): RenderedScene[T] = {

    // setup scene
    beforeRender()

    val nThreads = Runtime.getRuntime.availableProcessors
    //println("Rendering in " + nThreads + " threads")
    val pool = Executors.newFixedThreadPool(nThreads)
    reporter.reset
    var y = 0
    while (y < height) {
      pool.execute(new LineRenderer(imageWriter, y))
      y += 1
    }
    pool.shutdown
    pool.awaitTermination(java.lang.Long.MAX_VALUE, TimeUnit.MILLISECONDS)

    printStats("%-20s")

    // TODO: clone data?
    return new RenderedScene(imageWriter, data)
  }

  def put(y: Int, line: Array[Int]) = data.synchronized {
    data(y) = line
    reporter.onLineCompleted
  }

  class ProgressReporter(val totalLines: Int, val secondsBetweenProgressReport: Int) {
    var completed : Int = 0
    var start = System.currentTimeMillis
    var lastInfoTimestamp: Long = start

    def reset(): Unit = this.synchronized {
      completed = 0
      start = System.currentTimeMillis
      lastInfoTimestamp = start
    }

    def onLineCompleted() = this.synchronized {
      completed += 1
      if (System.currentTimeMillis - lastInfoTimestamp > secondsBetweenProgressReport * 1000) {
        lastInfoTimestamp = System.currentTimeMillis
        val remaining = ((totalLines - completed) * ((lastInfoTimestamp - start).toDouble / completed)).toInt 
        println("Rendered " + completed + "/" + totalLines + " lines (" + (remaining / 1000 / 60) + " minutes remaining)")
      }
    }
  }
}
