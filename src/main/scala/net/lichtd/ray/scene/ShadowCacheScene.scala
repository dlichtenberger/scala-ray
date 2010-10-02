package net.lichtd.ray.scene


import net.lichtd.ray.math.Vector
import net.lichtd.ray.shapes.{Shape, Sphere}
import java.lang.ThreadLocal
import scala.collection.mutable;

/**
 * Extends Scene with a shadow cache
 *
 * @author Daniel Lichtenberger
 * @version $Rev$
 */
trait ShadowCacheScene extends Scene {
  private val PROP_ENABLED = "shadow-cache"
  

  class CachedBlockFinder(wrapped: BlockFinder) extends WrappedBlockFinder(wrapped) {
    override def getBlockingObject(target: Shape, point: Vector, lsOrigin: Vector) : Option[Shape] = {
      CachedBlockFinder.lastBlock.get.get(lsOrigin) match {
        case Some(obj) =>
          val vector = point - lsOrigin
          val ray = new Ray(lsOrigin, vector)
          recordSecondaryIntersections(1)
          obj.intersectionPoint(ray) match {
            case Some(inter)  => 
              // check if the intersection is before or after our target object
              if ((inter - lsOrigin).length < vector.length) {
                return Some(obj)
              }
            case None     => // use normal check
          }

        case None   => // not found
      }
      super.getBlockingObject(target, point, lsOrigin) match {
        case Some(shape) =>
          CachedBlockFinder.lastBlock.get.put(lsOrigin, shape)
          Some(shape)
        case None =>
          CachedBlockFinder.lastBlock.get.remove(lsOrigin)
          None
      }
    }
  }

  object CachedBlockFinder {
    // record last block info across pixel/subpixel boundaries
    val lastBlock : ThreadLocal[mutable.Map[Vector, Shape]] = new ThreadLocal[mutable.Map[Vector, Shape]]() {
      override def initialValue = mutable.Map[Vector, Shape]()
    }
  }

  private lazy val enabled = if (System.getProperty(PROP_ENABLED) != null) {
    println("Shadow cache enabled")
    true
  } else false

  override def createBlockFinder(ray: Ray, target: Shape, intersection: Intersection, intersectionToken: Any) : BlockFinder =
    if (enabled)
      new CachedBlockFinder(super.createBlockFinder(ray, target, intersection, intersectionToken))
    else
      super.createBlockFinder(ray, target, intersection, intersectionToken)

  
}
