package net.lichtd.ray.math

import javax.vecmath.Vector3d

final class FastVector(val v: Vector3d) {
  
  def this(x: Double, y: Double, z: Double) = this(new Vector3d(x, y, z))

  def +(v2: FastVector) = {
    val result = new Vector3d()
    result.add(v, v2.v)
    new FastVector(result)
  }

  def +(v2: Vector3d) = {
    val result = new Vector3d()
    result.add(v, v2)
    new FastVector(result)
  }


  override def toString = v.toString

}
