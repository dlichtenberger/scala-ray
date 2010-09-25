package net.lichtd.ray.output

class RenderedScene[T <: ImageWriter](val imageWriter: T, val data: Array[Array[Int]]) {
  def toFile(fileName: String) = imageWriter.write(fileName, data)
}
