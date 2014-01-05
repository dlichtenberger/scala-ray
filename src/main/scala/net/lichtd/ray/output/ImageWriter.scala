package net.lichtd.ray.output

import net.lichtd.ray.maths.Color

abstract class ImageWriter {
  def toInternal(red: Int, green: Int, blue: Int): Int

  def write(fileName: String, data: Array[Array[Int]]): Unit

  def toInternal(color: Color): Int =
          toInternal((color.red * 255).asInstanceOf[Int], (color.green * 255).asInstanceOf[Int], (color.blue * 255).asInstanceOf[Int])
}
