package net.lichtd.ray.scene

import net.lichtd.ray.maths.Vector

class ViewPoint(val origin: Vector, _direction: Vector, _up: Vector) {
  val direction = _direction.normalize

  // calculate up
  private val viewPlanePoint = origin + direction // a point on the viewplane
  val up = if (Math.abs(direction * _up) < 0.01)
    _up.normalize // already normal to view direction
  else  // project using direction 
    (_up.normalize + direction * Math.abs(direction * _up)).normalize

  val right = (up cross direction).normalize
  
  def this(origin: Vector) = this(origin, new Vector(0, 0, 1), new Vector(0, 1, 0))

  val viewPlaneDistance = direction.length
  val lookingAt = origin + direction
}

