package net.lichtd.ray.shapes


import _root_.scala.None
import net.lichtd.ray.maths.{NumberUtils, Vector}
import net.lichtd.ray.scene.{Intersection, Ray}

class Plane(val normal: Vector, val distance: Double) extends Shape {

  override def intersect(ray: Ray) : Option[Intersection] = intersectionPoint(ray) match {
    case Some(inter) => Some(new Intersection(inter, normal))
    case None        => None
  }

  def intersectionPoint(ray: Ray) = {
    // taken from http://blogs.warwick.ac.uk/nickforrington/entry/raytracing_intersection_with
    val num = -distance - (ray.origin * normal)
    val denom = ray.direction * normal
    if (NumberUtils.equal(num, 0.0) || NumberUtils.equal(denom, 0.0)) {
      None
    } else {
      val t = num / denom
      if (t < 0)
        None
      else
        Some(ray.origin + ray.direction * t)
    }
  }
}