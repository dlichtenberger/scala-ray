package net.lichtd.ray.shapes


import net.lichtd.ray.math.{Color, Vector}
import net.lichtd.ray.scene.{Intersection, Ray}

import net.lichtd.ray.surface.SurfaceShader

abstract class Shape {
  var surface: SurfaceShader = null

  def intersect(ray: Ray) : Option[Intersection]

  def intersectionPoint(ray: Ray) : Option[Vector]
  
}
