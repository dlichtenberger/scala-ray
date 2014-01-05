package net.lichtd.ray.maths

object NumberUtils {
  private val PREC = 0.00000001

  def equal(v1: Double, v2: Double) : Boolean = {
    val diff = v1 - v2
    diff < PREC && diff > -PREC
  }
}
