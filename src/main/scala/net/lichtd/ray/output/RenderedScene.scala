package net.lichtd.ray.output

class RenderedScene[T <: ImageWriter](val imageWriter: T, val data: Array[Array[Int]]) {
  def writeImage(fileName: String) {
    imageWriter.write(fileName, data)
  }
}
