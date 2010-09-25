package net.lichtd.ray.shapes

import net.lichtd.ray.math.{Vector2D, Vector}
import net.lichtd.ray.scene.{Intersection, Ray}

class Sphere(val origin: Vector, val radius: Double) extends Shape {
  val r2 = radius * radius
  //val origin3d = new Vector3d(origin.x, origin.y, origin.z)

  def intersect(ray: Ray) : Option[Intersection]
  = intersectionPoint(ray) match {
    case Some(point: Vector) =>
      Some(new Intersection(point, (point - origin).normalize))
    case None =>  None
  }


  def intersectionPoint(ray: Ray) : Option[Vector] = {
    // algebraic algorithm taken from http://www.devmaster.net/wiki/Ray-sphere_intersection
    val dest: Vector = ray.origin - origin
    val b: Double = dest * ray.direction
    val c: Double = (dest * dest) - r2
    val d: Double = b*b - c
    /*val dest = new Vector3d
    dest.sub(ray.origin3d, origin3d)
    val b = dest dot ray.direction3d
    val c = (dest dot dest) - r2
    val d = b*b - c*/
    if (d > 0) {
      val sqrt = Math.sqrt(d)
      val t = (-b - sqrt)
      // this seems to be necessary for intersections from inside the sphere, see http://www.devmaster.net/wiki/Talk:Ray-sphere_intersection
      // this leads to false positives
      //val delta = if (t > 0) t else (-b + sqrt)
      return if (t > 0) Some(ray.origin + (ray.direction * t)) else None
      } else {
      return None
      }
    }

  // SphericalSurfaceMapper implementation
  def getSurfaceCoordinates(surfacePoint: Vector, north: Vector, equator: Vector) : Vector2D = {
    // convert to (u, v) coordinates using spherical coordinates
    // taken from http://www.cs.unc.edu/~rademach/xroads-RT/RTarticle.html
    val vp = (surfacePoint - origin).normalize

    val phi = Math.acos(-(north * vp))
    val v = phi / Math.Pi

    val theta = (Math.acos((vp * equator) / Math.sin(phi))) / (2 * Math.Pi)
    val u = if ((north cross equator) * vp > 0) theta else 1 - theta

    // (u, v) represent the position in a [0, 1] square
    new Vector2D(u, v)
  }
}
