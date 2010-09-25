package net.lichtd.ray.scene

import net.lichtd.ray.math.Color
import net.lichtd.ray.math.Vector

class PointLightSource(val origin: Vector, color: Color) extends LightSource(color) {
  private val origins = List(origin)

  def getOrigins(from: Vector, resolution: Int) : List[Vector] = origins

  override def getOriginCount(rays: Int) = 1
}