package net.lichtd.ray.scene

import net.lichtd.ray.maths.Color
import net.lichtd.ray.maths.Vector
import util.Random

class SphericalLightSource(val origin: Vector, size: Double, color: Color)
        extends LightSource(color) with CachingLightSource {

  override def createCacheEntry(from: Vector, rays: Int) : Array[Array[Vector]] = {
    if (rays == 1) {
      return Array(Array(origin))
    }
    //val viewVector = (from - origin).normalize   // view vector
    // TODO: create (rays) samples inside a circle that is normal to (viewVector)

    // return sets of rays origin points for montecarlo sampling
    val samples: Int = 250
    val result : Array[Array[Vector]] = Array.ofDim(samples, rays)
    for (i <- 0 until samples) {
      val origins: Array[Vector] = Array.ofDim(rays)
      val rnd = new Random
      for (j <- 0 until rays) {
        origins(j) = new Vector(rnd.nextDouble(), rnd.nextDouble(), rnd.nextDouble()) * size
      }
      result(i) = origins
    }
    result
  }


  override def getOriginCount(rays: Int) = rays
}