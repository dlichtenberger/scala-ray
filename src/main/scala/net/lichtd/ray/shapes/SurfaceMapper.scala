package net.lichtd.ray.shapes

import net.lichtd.ray.maths.{Vector2D, Vector}

trait SurfaceMapper extends Shape {
  def getSurfaceCoordinates(surfacePoint: Vector) : Vector2D
}
