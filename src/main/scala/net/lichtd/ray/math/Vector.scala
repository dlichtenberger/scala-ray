package net.lichtd.ray.math

final class Vector(val x: Double, val y: Double, val z: Double, val forceNormalized: Boolean) {
  import NumberUtils.equal

  def this(x: Double, y: Double, z: Double) = this(x, y, z, false)
  
  lazy val length = if (forceNormalized) 1 else Math.sqrt(x*x + y*y + z*z)
  lazy val normalized = forceNormalized || equal(length, 1.0)

  // basic ops
  def -(v: Vector) = new Vector(x - v.x, y - v.y, z - v.z)
  def +(v: Vector) = new Vector(x + v.x, y + v.y, z + v.z)
  def *(amount: Double) = new Vector(x * amount, y * amount, z * amount)
  def *(v: Vector) : Double = x * v.x + y * v.y + z * v.z
  def /(amount: Double) = new Vector(x / amount, y / amount, z / amount)

  def unary_-() = new Vector(-x, -y, -z)

  def cross(v: Vector) = new Vector(y*v.z - z*v.y, z*v.x - x*v.z, x*v.y - y*v.x)

  def rotateAround(_axis: Vector, theta: Double) = {
    assert(normalized)
    val axis = _axis.normalize
    // taken from http://www.gamedev.net/community/forums/topic.asp?topic_id=198844
    // (corrected parentheses before cos() on the first expression) 
    ((this - (axis * (axis * this))) * Math.cos(theta) +
    ((this cross axis) * Math.sin(theta)) +
    (axis * (axis * this))).normalize
  }

  def reflectAround(n: Vector) = n * (n * this * 2) - this

  def normalize() : Vector = normalized match {
    case true  => this
    case false => new Vector(x / length, y / length, z / length, true)
  }

  override def equals(other: Any) : Boolean = other match {
    case v: Vector =>   equal(x, v.x) && equal(y, v.y) && equal(z, v.z)
    case _ =>           false
  }

  // TODO hashCode

  override def toString = String.format(java.util.Locale.ENGLISH,
    "(%.2f,%.2f,%.2f)", Array[java.lang.Double](x, y, z) : _*)
}

object Vector {
  val ORIGIN = new Vector(0, 0, 0)
}