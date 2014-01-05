package net.lichtd.ray.shapes

import net.lichtd.ray.maths.{Vector2D, Vector}

trait SphericalSurfaceMapper extends SurfaceMapper {
  val north : Vector
  val equator : Vector

  override def getSurfaceCoordinates(surfacePoint: Vector) : Vector2D =
    getSurfaceCoordinates(surfacePoint, north, equator)

  def getSurfaceCoordinates(surfacePoint: Vector, north: Vector, equator: Vector) : Vector2D
}