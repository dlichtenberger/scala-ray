package net.lichtd.ray.scene

import net.lichtd.ray.math.Color
import net.lichtd.ray.math.Vector
import util.Random

class SphericalLightSource(val origin: Vector, size: Double, color: Color)
        extends LightSource(color) with CachingLightSource {

  override def createCacheEntry(from: Vector, rays: Int) : Array[List[Vector]] = {
    if (rays == 1) {
      return Array(List(origin))
    }
    //val viewVector = (from - origin).normalize   // view vector
    // TODO: create (rays) samples inside a circle that is normal to (viewVector)

    // return sets of rays origin points for montecarlo sampling
    var result = List[List[Vector]]()
    for (i <- 1 to 250) {
      var origins = List[Vector]()
      val rnd = new Random
      for (i <- 1 to rays) {
        origins ::= origin + (new Vector(rnd.nextDouble, rnd.nextDouble, rnd.nextDouble) * size)
      }
      result ::= origins
    }
    return result.toArray
  }


  override def getOriginCount(rays: Int) = rays
}