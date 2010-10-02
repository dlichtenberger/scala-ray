package net.lichtd.ray.math

final class MutableVector(val v: Vector) {
  import NumberUtils.equal
  private var x = v.x
  private var y = v.y
  private var z = v.z

  def toVector = new Vector(x, y, z)

  def length = scala.math.sqrt(x*x + y*y + z*z)
  def normalized = equal(length, 1.0)

  // basic ops
  def -(v: Vector) = { x -= v.x; y -= v.y; z -= v.z; this }
  def +(v: Vector) = { x += v.x; y += v.y; z += v.z; this }
  def *(amount: Double) = { x *= amount; y *= amount; z *= amount; this }
  def *(v: Vector) : Double = x * v.x + y * v.y + z * v.z
  def /(amount: Double) = { x /= amount; y /= amount; z /= amount; this }

  def unary_-() = { x = -x; y = -y; z = -z; this } 

  def cross(v: Vector) = {
    x = y*v.z - z*v.y;
    y = z*v.x - x*v.z;
    z = x*v.y - y*v.x;
    this
  }

  def normalize() : MutableVector = {
    this./(length);
    this
  }


  // TODO hashCode

  override def toString = String.format(java.util.Locale.ENGLISH,
    "MutableVector(%.2f,%.2f,%.2f)", Array[java.lang.Double](x, y, z) : _*)
}

