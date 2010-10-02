package net.lichtd.ray.math

final class Vector2D(val x: Double, val y: Double) {
  import NumberUtils.equal

  lazy val length = scala.math.sqrt(x*x + y*y)
  lazy val normalized = equal(length, 1.0)

  // TODO: vector arithmetic

  def normalize() : Vector2D = normalized match {
    case true => this
    case false => new Vector2D(x/length, y/length)
  }

  override def equals(other: Any): Boolean = other match {
    case v: Vector2D =>
            v.getClass == getClass && equal(x, v.x) && equal(y, v.y)
    case _ => false
  }

  override def toString = String.format(java.util.Locale.ENGLISH,
    "(%.2f,%.2f)", Array[java.lang.Double](x, y) : _*)
}

