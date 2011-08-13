package net.lichtd.ray.scene

import net.lichtd.ray.math.Color
import net.lichtd.ray.math.Vector
import java.util.concurrent.ConcurrentHashMap

abstract class LightSource(val color: Color) {
  /**
   * Return all points of the lightsource with at most
   * {@code rays} points, when looking from position {@code from}.
   */
  def getOrigins(from: Vector, rays: Int) : Array[Vector]

  /**
   * Optimized method that returns the number of origins for the given ray count.
   */
  def getOriginCount(rays: Int) : Int = getOrigins(Vector.ORIGIN, rays).length
}

trait CachingLightSource extends LightSource {
  private val cachedOrigins = new ConcurrentHashMap[Int, Array[Array[Vector]]]
  private val cachedOriginCounts = new ConcurrentHashMap[Int, Int]
  
  override def getOrigins(from: Vector, rays: Int) : Array[Vector] = {
    if (!cachedOrigins.containsKey(rays)) {
      // TODO: don't ignore from for cache key
      cachedOrigins.putIfAbsent(rays, createCacheEntry(from, rays))
    }
    val set = cachedOrigins.get(rays)
    set((Math.random * set.size).asInstanceOf[Int])
  }

  /** Create one (or more, if adding jitter noise) sets of origins for the given number of rays */
  def createCacheEntry(from: Vector, rays: Int) : Array[Array[Vector]]

}
