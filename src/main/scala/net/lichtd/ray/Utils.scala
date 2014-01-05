package net.lichtd.ray

import scala.reflect.ClassTag

/**
 * @author Daniel Lichtenberger, 1/5/14.
 */
object Utils {

  def arrayPrepend[A : ClassTag](elem: A, array: Array[A]): Array[A] = {
    val result: Array[A] = Array.ofDim[A](array.length + 1)
    result(0) = elem
    System.arraycopy(array, 0, result, 1, array.length)
    result
  }
}
