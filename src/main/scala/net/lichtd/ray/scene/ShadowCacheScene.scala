package net.lichtd.ray.scene


import net.lichtd.ray.math.Vector
import net.lichtd.ray.shapes.{Shape, Sphere}

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
      if (lastBlock == null) {
        super.getBlockingCandidates(ray)
      } else {
        var candidates = super.getBlockingCandidates(ray)
        lastBlock :: candidates // try last blocking object first
      }
    }
  }


  override def createBlockFinder(ray: Ray, target: Shape, intersection: Intersection, intersectionToken: Any) =
    new CachedBlockFinder(super.createBlockFinder(ray, target, intersection, intersectionToken))
  
}