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
  private val PROP_ENABLED = "shadow-cache"

  class CachedBlockFinder(wrapped: BlockFinder) extends WrappedBlockFinder(wrapped) {
    var lastBlock: Shape = null

    override def getBlockingObject(target: Shape, point: Vector, lsOrigin: Vector) : Option[Shape]
    = super.getBlockingObject(target, point, lsOrigin) match {
      case Some(shape) =>
        lastBlock = shape
        Some(shape)
      case None =>
        None
    }


    override def getBlockingCandidates(ray: Ray) : List[Seq[Shape]] = {
      if (lastBlock == null) {
        super.getBlockingCandidates(ray)
      } else {
        var candidates = super.getBlockingCandidates(ray)
        Seq(lastBlock) :: candidates // try last blocking object first
      }
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
