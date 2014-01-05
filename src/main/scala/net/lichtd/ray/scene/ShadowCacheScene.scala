package net.lichtd.ray.scene


import net.lichtd.ray.maths.Vector
import net.lichtd.ray.shapes.Shape
import net.lichtd.ray.Utils

/**
 * Extends Scene with a shadow cache
 *
 * @author Daniel Lichtenberger
 * @version $Rev$
 */
trait ShadowCacheScene extends Scene {
  class CachedBlockFinder(wrapped: BlockFinder)
          extends BlockFinder(wrapped.ray, wrapped.target, wrapped.intersection, wrapped.token) {
    var lastBlock: Shape = null

    override def getBlockingObject(target: Shape, point: Vector, lsOrigin: Vector) : Option[Shape]
    = super.getBlockingObject(target, point, lsOrigin) match {
      case Some(shape) =>
        lastBlock = shape
        Some(shape)
      case None =>
        None
    }


    override protected def getBlockingCandidates(ray: Ray) = {
      val result = super.getBlockingCandidates(ray)
      if (lastBlock == null) {
        result
      } else {
        Utils.arrayPrepend(lastBlock, result)
      }
    }
  }


  override def createBlockFinder(ray: Ray, target: Shape, intersection: Intersection, intersectionToken: Any) =
    new CachedBlockFinder(super.createBlockFinder(ray, target, intersection, intersectionToken))
  
}