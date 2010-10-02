package net.lichtd


import net.lichtd.ray.math.{Color, Vector}
import net.lichtd.ray.surface.SurfaceShader
import net.lichtd.ray.shapes.{Shape, Sphere}

import net.lichtd.ray.scene.{Intersection, Ray}
import org.junit._
import Assert._
import net.lichtd.ray._

@Test
class ShapesTest {
  // ray from inside sphere
  @Test @Ignore def sphereInnerIntersect = assertIntersect(new Sphere(Vector.ORIGIN, 1),
                    new Ray(Vector.ORIGIN, new Vector(1, 1, 0)),
                    new Vector(scala.math.sqrt(.5), scala.math.sqrt(.5), 0))

  // ray from the outside
  @Test def sphereExtIntersect = assertIntersect(new Sphere(Vector.ORIGIN, 1),
                    new Ray(new Vector(-10, 0, 0), new Vector(1, 0, 0)),
                    new Vector(-1, 0, 0))

  private def assertIntersect(shape: Shape, ray: Ray, expectedIntersection: Vector) = shape.intersect(ray) match {
    case Some(inter: Intersection) =>
      assert(inter.origin == expectedIntersection, "Unexpected intersection point: " + inter.origin)
    case None =>
      assert (false, "Ray intersection expected for " + shape + " and " + ray)
  }
}
