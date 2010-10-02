package net.lichtd.ray.scene

import net.lichtd.ray.math.Vector
import net.lichtd.ray.shapes.{Shape, Sphere}
import scala.collection.JavaConversions._
import java.util.ArrayList

trait KDTreeScene extends Scene {
  private val PROP_ENABLED = "kd-tree"
  var kdtree : KDTree = null

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

  override def walkScene[T](from: Shape, ray: Ray, 
                            interFunc: (Shape, Ray) => Option[T], 
                            func: (Shape, T) => Boolean) : Unit = {
    if (!enabled) {
      super.walkScene(from, ray, interFunc, func)
    } else {
      walkNodes(from, ray, kdtree, 0, 9999, interFunc, func)
    }
  }

  private def walkNodes[T](from: Shape, ray: Ray, node: KDTree, tmin: Double, tmax: Double,
                        interFunc: (Shape, Ray) => Option[T],
                        func: (Shape, T) => Boolean) : Boolean = {
    if (node.leaf) {
      val it = node.shapes.elements
      var done = false;
      var checks = 0
      while (it.hasNext) {
        val obj = it.next
        if (obj != from) {
          checks += 1
          interFunc(obj, ray) match {
            case Some(inter: T)    => done |= func(obj, inter)
            case None => // continue
          }
        }
      }
      recordIntersections(from == null, checks)
      return done
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
        return walkNodes(from, ray, near, tmin, tmax, interFunc, func)
      } else if (tsplit < tmin) {
        //println("Checking only far")
        return walkNodes(from, ray, far, tmin, tmax, interFunc, func)
      } else {
        //println("Checking both")
        return walkNodes(from, ray, near, tmin, tsplit, interFunc, func) || walkNodes(from, ray, far, tsplit, tmax, interFunc, func)
      }
    } else {
      return walkNodes(from, ray, near, tmin, tmax, interFunc, func) || walkNodes(from, ray, far, tmin, tmax, interFunc, func)
    }
  }

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

