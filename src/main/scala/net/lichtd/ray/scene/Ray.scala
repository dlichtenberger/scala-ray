package net.lichtd.ray.scene

import net.lichtd.ray.math.Vector

class Ray(val origin: Vector, _direction: Vector, val iteration: Int) {
  val direction = _direction.normalize

  def this(origin: Vector, _direction: Vector) = this(origin, _direction, 0)
  override def toString = "Ray[from=" + origin + ", dir=" + direction + "]"
}
