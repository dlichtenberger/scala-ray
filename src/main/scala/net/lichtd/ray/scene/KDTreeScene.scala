package net.lichtd.ray.scene

import net.lichtd.ray.math.Vector
import net.lichtd.ray.shapes.{Shape, Sphere}

trait KDTreeScene extends Scene {
  private val PROP_ENABLED = "kd-tree"
  var kdtree : KDTree = null

  protected override def isOrderedTraversal = true

  class BSPBlockFinder(wrapped: BlockFinder) extends WrappedBlockFinder(wrapped) {

    override def getBlockingCandidates(ray: Ray): List[Seq[Shape]] = {
      // walk KD tree
      var sorted : List[Seq[Shape]] = List()
      val tsplit = (kdtree.coord - ray.origin.coord(kdtree.axis)) / ray.direction.coord(kdtree.axis)
      for (node <- createNodePath(ray, kdtree, 0, 9999)) {
        if (node.leaf) {
          sorted ::= node.shapes
        }
      }
      //println("Checking " + sorted.flatten.size + " shapes (" + kdtree.shapes.size + " total)")
      sorted.reverse
    }

    private def createNodePath(ray: Ray, node: KDTree, tmin: Double, tmax: Double) : List[KDTree] = {
      if (node.leaf) {
        return List(node)
      }
      val axis = node.axis
      val (near, far) = {
        if (ray.direction.coord(axis) > 0) {
          (node.left, node.right)
        } else {
          (node.right, node.left)
        }
      }
      if (ray.direction.coord(axis) != 0) {
        val tsplit = (node.coord - ray.origin.coord(axis)) / ray.direction.coord(axis)
        if (tsplit > tmax) {
          //println("Checking only near (tsplit=" + tsplit + ", minc=" + node.minCoord + ")")
          return node :: createNodePath(ray, near, tmin, tmax)
        } else if (tsplit < tmin) {
          //println("Checking only far")
          return node :: createNodePath(ray, far, tmin, tmax)
        } else {
          //println("Checking both")
          return node :: (createNodePath(ray, near, tmin, tsplit) ++ createNodePath(ray, far, tsplit, tmax))
        }
      } else {
        return node :: (createNodePath(ray, near, tmin, tmax) ++ createNodePath(ray, far, tmin, tmax))
      }
    }
    
  }

  private lazy val enabled = if (System.getProperty(PROP_ENABLED) == null) false else {
    println("KD Tree enabled")
    true
  }

  override def beforeRender() : Unit = {
    if (enabled) {
      val start = System.currentTimeMillis
      kdtree = KDTree.create(objects)
      println("kd-tree creation took " + (System.currentTimeMillis - start) + "ms")
    }
  }

  override def createBlockFinder(ray: Ray, target: Shape, intersection: Intersection, intersectionToken: Any) : BlockFinder =
    if (enabled)
      new BSPBlockFinder(super.createBlockFinder(ray, target, intersection, intersectionToken))
    else
      super.createBlockFinder(ray, target, intersection, intersectionToken)
  
}

object KDTree {
  private val SIZE_CUTOFF = 3    // no split below this number of primitives in a tree
  private val DEPTH_CUTOFF = 10  // no split below this cutoff value

  def create(shapes: Seq[Shape]) : KDTree = create(shapes, 0, 0) 

  def create(shapes: Seq[Shape], axis: Int, depth: Int) : KDTree = {
    if (shapes.size <= SIZE_CUTOFF || depth > DEPTH_CUTOFF) {
      // TODO: is 0 really good for coord/minCoord/maxCoord here?
      return new KDTree(axis, depth, 0, 0, 0, null, null, shapes)
    }
    // get "median" value
    var extremes : Seq[(Double, Double)] = shapes.map(_.getExtremes(axis))
    var leftExtremes = extremes.map(_._1).toList.sort((e1, e2) => (e1 < e2))
    //var rightExtremes = extremes.map(_._2).sort
    val split = leftExtremes(leftExtremes.size / 2 + (leftExtremes.size % 2))

    // find split position
    var left : List[Shape] = List()
    var right : List[Shape] = List()
    for (shape <- shapes) { 
      val check = shape.intersectsAxis(axis, split)
      if (check <= 0) {
        left ::= shape
      }
      if (check >= 0) {
        right ::= shape
      }
    }
    println("Split @ depth " + depth + ", axis=" + axis + ",val=" + split + " (left: " + left.size + ", right: " + right.size + ")")
    return new KDTree(
      axis, depth, split, leftExtremes(0), leftExtremes(leftExtremes.size - 1),
      create(left, (axis + 1) % 3, depth + 1), 
      create(right, (axis + 1) % 3, depth + 1), 
      shapes
    )
  }
}

class KDTree(val axis: Int, val depth: Int, val coord: Double, val minCoord: Double, val maxCoord: Double,
             val left: KDTree, val right: KDTree, val shapes: Seq[Shape]) {

  def leaf = left == null || right == null

  lazy val globalMin : Double = Math.min(coord, if (leaf) coord else Math.min(left.globalMin, right.globalMin))
  lazy val globalMax : Double = Math.max(coord, if (leaf) coord else Math.max(left.globalMax, right.globalMax))

  override def toString : String = 
    return "KDTree(axis:" + axis + "@" + coord + ",d:" + depth + ",size: " + shapes.size + ",left: " +
            left + ",right: " + right + ")"
}

