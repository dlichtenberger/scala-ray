package net.lichtd.ray.math

final class Color(_red: Double, _green: Double, _blue: Double) {
  import NumberUtils.equal

  val red = saturated(_red)
  val green = saturated(_green)
  val blue = saturated(_blue)

  private def saturated(color: Double): Double = if (color < 0) 0 else if (color > 1) 1 else color

  /**
   * Constructor for a grayscale value.
   */
  def this(intensity: Double) = this (intensity, intensity, intensity)

  def +(other: Color) = new Color(red + other.red, green + other.green, blue + other.blue)

  def -(other: Color) = new Color(red - other.red, green - other.green, blue - other.blue)

  def *(other: Color) = new Color(red * other.red, green * other.green, blue * other.blue)

  def *(scale: Double) = new Color(red * scale, green * scale, blue * scale)

  def /(scale: Double) = new Color(red / scale, green / scale, blue / scale)

  override def equals(other: Any): Boolean = other match {
    case c: Color => c.getClass == getClass && equal(red, c.red) && equal(green, c.green) && equal(blue, c.blue)
    case _ => false
  }


  // TODO hashCode
  override def toString = String.format(java.util.Locale.ENGLISH,
    "Color[%.2f,%.2f,%.2f]", Array[java.lang.Double](red, green, blue): _*)
}

object Color {
  val BLACK = new Color(0)
  val WHITE = new Color(1)

  def DColor(red: Double, green: Double, blue: Double): Color = new Color(red, green, blue)

  def DColor(intensity: Double): Color = new Color(intensity)

  /**
   * Factory method for an RGB color using integer values between 0 and 255.
   */
  def IColor(_red: Int, _green: Int, _blue: Int): Color =
    new Color(_red.toDouble / 256.0, _green.toDouble / 256.0, _blue.toDouble / 256.0)

  def RGBColor(rgb: Int) = IColor(
    ((rgb >> 16) & 0xFF),
    ((rgb >> 8) & 0xFF),
    (rgb & 0xFF)
    )
}