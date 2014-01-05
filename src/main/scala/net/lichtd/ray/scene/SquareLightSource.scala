package net.lichtd.ray.scene

import net.lichtd.ray.maths.{Color, Vector}
import java.util.concurrent.ConcurrentHashMap

class SquareLightSource(val upperLeftCorner: Vector, val right: Vector, val down: Vector, color: Color) extends LightSource(color) {
  require(right * down == 0, "Square light source: right and down vectors are not normal")

  // for a single ray, return the center of the lightsource
  private val singleTracedOrigin = Array(upperLeftCorner + right * 0.5 + down * 0.5)
  private val cachedOrigins = new ConcurrentHashMap[Int, Array[Array[Vector]]]
  private val cachedOriginCounts = new ConcurrentHashMap[Int, Int]
  private val jitteredRays = 25   // number of jittered rays for MonteCarlo sampling

  private sealed case class DistributedOrigin(origins: Array[Vector], rightStep: Vector, downStep: Vector)

  def getOrigins(from: Vector, rays: Int) : Array[Vector] = {
    if (rays <= 1) {
      return singleTracedOrigin
    }
    if (!cachedOrigins.containsKey(rays)) {
      // TODO: return sample orthogonal to the view vector (from - center of LS)
      val steps = Math.round(Math.sqrt(rays))
      val origins : Array[Vector] = Array.ofDim(steps.toInt * steps.toInt)
      var y = 0
      var idx = 0
      while (y < steps.toInt) {
        var x = 0
        while (x < steps.toInt) {
          origins(idx) = upperLeftCorner + right * (x.toDouble / steps) + down * (y.toDouble / steps)
          idx += 1
          x += 1
        }
        y += 1
      }
      val orig = DistributedOrigin(origins, right / steps, down / steps)
      val jitteredOrigins = new Array[Array[Vector]](jitteredRays)
      // add MonteCarlo distribution (jitter)
      var i = 0
      while (i < jitteredRays) {
        jitteredOrigins(i) = orig.origins.map(_ + orig.rightStep * Math.random + orig.downStep * Math.random)
        i += 1
      }
      cachedOrigins.putIfAbsent(rays, jitteredOrigins)
    }
    cachedOrigins.get(rays)(Math.max((Math.random * jitteredRays).asInstanceOf[Int], jitteredRays - 1))
  }


  override def getOriginCount(rays: Int) = {
    if (!cachedOriginCounts.containsKey(rays)) {
      cachedOriginCounts.put(rays, getOrigins(Vector.ORIGIN, rays).length)
    }
    cachedOriginCounts.get(rays)
  }
}