package net.lichtd.ray.shapes


import net.lichtd.ray.math.{Color, Vector}
import net.lichtd.ray.scene.{Intersection, Ray}

import net.lichtd.ray.surface.SurfaceShader

abstract class Shape {
  var surface: SurfaceShader = null

  def intersect(ray: Ray) : Option[Intersection]

  def intersectionPoint(ray: Ray) : Option[Vector]

  /**
   * @param axis    the axis to be checked(x=0, y=1, z=2)
   * @param coord   the coordinate value on this axis
   * @return    -1 if the shape lies completely to the "left" of the given axis
   *             0 if the shape intersects the axis
   *             1 if the shape lies completely to the "right" of the given axis
   */
  def intersectsAxis(axis: Int, coord: Double) : Int

  /**
   * @param axis    the axis to be checked(x=0, y=1, z=2)
   * @return        the (left, right) extremes of the object on the given axis
   */
  def getExtremes(axis: Int) : (Double, Double)
  
}
