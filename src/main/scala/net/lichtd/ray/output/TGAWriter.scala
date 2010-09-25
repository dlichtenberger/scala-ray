package net.lichtd.ray.output

import java.io.FileOutputStream
import java.nio.ByteBuffer

object TGAWriter extends ImageWriter {
  // convert to BGR in little endian, set alpha channel
  def toInternal(red: Int, green: Int, blue: Int) = (blue << 24) + (green << 16) + (red << 8) + 0xFF

  override def write(fileName: String, data: Array[Array[Int]]) = {
    val outStream = new FileOutputStream(fileName)
    val out = outStream.getChannel

    val header = new Array[Byte](18)
    val height = data.length
    val width = data(0).length
    header(2) = 2   // 24 bit uncompressed
    header(12) = width.toByte
    header(13) = (width >> 8).toByte
    header(14) = height.toByte
    header(15) = (height >> 8).toByte
    header(16) = 32

    val bb = ByteBuffer.allocate(data.length * data(0).length * 4 + header.length)
    bb.put(header)
    data.foreach(_.foreach(bb.putInt(_)))
    bb.flip
    out.write(bb)
    out.close
    outStream.close
  }
}
