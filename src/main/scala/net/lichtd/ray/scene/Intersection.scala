package net.lichtd.ray.scene

import net.lichtd.ray.maths.Vector

class Intersection(val origin: Vector, val surfaceNormal: Vector) {
  def reflect(in: Vector) : Vector =
    in - surfaceNormal * ((surfaceNormal * in) * 2)
  
  override def toString = "Intersection[" + origin + ",N=" + surfaceNormal + "]"
}
