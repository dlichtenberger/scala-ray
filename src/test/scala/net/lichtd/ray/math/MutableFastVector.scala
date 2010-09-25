package net.lichtd.ray.math

import javax.vecmath.Vector3d

final class MutableFastVector(val v: Vector3d) {

  def this(x: Double, y: Double, z: Double) = this(new Vector3d(x, y, z))

  def +(v2: FastVector) = {
    v.add(v2.v)
    this
  }

  def +(v2: Vector3d) = {
    v.add(v2)
    this
  }


  override def toString = v.toString

}
