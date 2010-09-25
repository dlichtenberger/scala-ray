package net.lichtd.ray.scene

import net.lichtd.ray.math.{Color, Vector}
import java.util.concurrent.{ConcurrentHashMap}

class SquareLightSource(val upperLeftCorner: Vector, val right: Vector, val down: Vector, color: Color) extends LightSource(color) {
  require(right * down == 0, "Square light source: right and down vectors are not normal")

  // for a single ray, return the center of the lightsource
  private val singleTracedOrigin = List(upperLeftCorner + right * 0.5 + down * 0.5)
  private val cachedOrigins = new ConcurrentHashMap[Int, Array[List[Vector]]]
  private val cachedOriginCounts = new ConcurrentHashMap[Int, Int]
  private val jitteredRays = 25   // number of jittered rays for MonteCarlo sampling

  private sealed case class DistributedOrigin(val origins: List[Vector], val rightStep: Vector, val downStep: Vector)

  def getOrigins(from: Vector, rays: Int) : List[Vector] = {
    if (rays <= 1) {
      return singleTracedOrigin
    }
    if (!cachedOrigins.containsKey(rays)) {
      // TODO: return sample orthogonal to the view vector (from - center of LS)
      val steps = Math.round(Math.sqrt(rays))
      var origins = List[Vector]()
      var y = 0
      while (y < steps.toInt) {
        var x = 0
        while (x < steps.toInt) {
          origins ::= upperLeftCorner + right * (x.toDouble / steps) + down * (y.toDouble / steps)
          x += 1
        }
        y += 1
      }
      val orig = DistributedOrigin(origins, right / steps, down / steps)
      val jitteredOrigins = new Array[List[Vector]](jitteredRays)
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
      cachedOriginCounts.put(rays, getOrigins(Vector.ORIGIN, rays).size)
    }
    cachedOriginCounts.get(rays)
  }
}