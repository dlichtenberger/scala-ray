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
                            func: (Shape, T) => Boolean,
                            interExtractor: T => Vector) : Unit = {
    if (!enabled) {
      super.walkScene(from, ray, interFunc, func, interExtractor)
    } else {
      walkNodes(from, ray, kdtree, 0, 9999, ray.direction.toArray, ray.origin.toArray, interFunc, func, interExtractor, 0, 0)
    }
  }

  private def walkNodes[T](from: Shape, ray: Ray, node: KDTree, tmin: Double, tmax: Double,
                        direction: Array[Double],  // unwrapped from ray for faster access
                        origin: Array[Double],     // unwrapped from ray for faster access
                        interFunc: (Shape, Ray) => Option[T], // calculates the intersection
                        callback: (Shape, T) => Boolean,      // process an intersection and returns true to abort
                                                              // processing after this tree node
                        interExtractor: T => Vector,          // extract the intersection point from a T
                        splitPos: Double,         // the split position and axis of the parent node
                        splitAxis: Int) : Boolean = {
    if (node.leaf) {
      var done = false;
      var checks = 0
      var i = 0
      while (i < node.shapes.length) {
        val obj = node.shapes(i)
        if (obj != from) {
          checks += 1
          interFunc(obj, ray) match {
            case Some(inter: T)    =>
              val result = callback(obj, inter)
              if (result) {
                // check if we can stop processing here - only if the intersection was on "our side" of the bounding box,
                // to fix the edge case where a ray hits an object spanning both nodes of a tree
                // first in the near node, and then does not check if it is occluded by an object in the far node
                val diraxis = direction(splitAxis % 3)
                val interPos = interExtractor(inter).toArray(splitAxis % 3)
                if ((diraxis > 0 && interPos <= splitPos) || (diraxis < 0 && interPos >= splitPos)) {
                  done |= result
                }
              }
            case None => // continue
          }
        }
        i += 1
      }
      recordIntersections(from == null, checks)
      return done
    }
    val axis = node.axis
    var near: KDTree = null
    var far: KDTree = null
    val diraxis : Double = direction(axis % 3)
    val origaxis : Double = origin(axis % 3)
    if (diraxis > 0) {
      near = node.left
      far = node.right
    } else {
      near = node.right
      far = node.left
    }
    if (diraxis != 0) {
      val tsplit = (node.coord - origaxis) / diraxis
      if (tsplit > tmax) {
        //println("Checking only near (tsplit=" + tsplit + ", minc=" + node.minCoord + ")")
        return walkNodes(from, ray, near, tmin, tmax, direction, origin, interFunc, callback, interExtractor, node.coord, node.axis)
      } else if (tsplit < tmin) {
        //println("Checking only far")
        return walkNodes(from, ray, far, tmin, tmax, direction, origin, interFunc, callback, interExtractor, node.coord, node.axis)
      } else {
        //println("Checking both")
        return walkNodes(from, ray, near, tmin, tsplit, direction, origin, interFunc, callback, interExtractor, node.coord, node.axis) ||
               walkNodes(from, ray, far, tsplit, tmax, direction, origin, interFunc, callback, interExtractor, node.coord, node.axis)
      }
    } else {
      return walkNodes(from, ray, near, tmin, tmax, direction, origin, interFunc, callback, interExtractor, node.coord, node.axis) ||
             walkNodes(from, ray, far, tmin, tmax, direction, origin, interFunc, callback, interExtractor, node.coord, node.axis)
    }
  }

}

object KDTree {
  private val SIZE_CUTOFF = 5    // no split below this number of primitives in a tree
  private val DEPTH_CUTOFF = 5  // no split below this cutoff value

  def create(shapes: Seq[Shape]) : KDTree = create(shapes, 0, 0) 

  def create(shapes: Seq[Shape], axis: Int, depth: Int) : KDTree = {
    if (shapes.size <= SIZE_CUTOFF || depth > DEPTH_CUTOFF) {
      // TODO: is 0 really good for coord/minCoord/maxCoord here?
      return new KDTree(axis, depth, 0, 0, 0, null, null, shapes.toArray)
    }
    // get "median" value
    var extremes : Seq[(Double, Double)] = shapes.map(_.getExtremes(axis))
    var leftExtremes = extremes.map(_._1).toList.sortWith((e1, e2) => (e1 < e2))
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
      shapes.toArray
    )
  }
}

class KDTree(val axis: Int, val depth: Int, val coord: Double, val minCoord: Double, val maxCoord: Double,
             val left: KDTree, val right: KDTree, val shapes: Array[Shape]) {

  val leaf = (left == null || right == null)

  lazy val globalMin : Double = scala.math.min(coord, if (leaf) coord else scala.math.min(left.globalMin, right.globalMin))
  lazy val globalMax : Double = scala.math.max(coord, if (leaf) coord else scala.math.max(left.globalMax, right.globalMax))

  override def toString : String = 
    return "KDTree(axis:" + axis + "@" + coord + ",d:" + depth + ",size: " + shapes.size + ",left: " +
            left + ",right: " + right + ")"
}

