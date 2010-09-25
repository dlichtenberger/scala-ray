package net.lichtd.ray.shapes

import net.lichtd.ray.math.{Vector2D, Vector}

trait SurfaceMapper extends Shape {
  def getSurfaceCoordinates(surfacePoint: Vector) : Vector2D
}
