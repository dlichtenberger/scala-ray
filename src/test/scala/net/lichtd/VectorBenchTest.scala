package net.lichtd

import net.lichtd.ray.maths.Vector
import net.lichtd.ray.maths.{FastVector, MutableVector, MutableFastVector}

import org.junit._
import javax.vecmath.Vector3d

@Test
class VectorBenchTest {

  @Test def basicVectorBench : Unit = {
    val COUNT = 1000000
    var v = new Vector(0, 0, 0)
    val v2 = new Vector(1, 1, 1)
    var i = 0
    var start = System.currentTimeMillis
    while (i < COUNT) {
      v = v + v2
      i += 1
    }
    println("Vector: 1M adds = " + (System.currentTimeMillis - start) + "ms, result=" + v)

    val mv = new MutableVector(new Vector(0, 0, 0))
    i = 0
    start = System.currentTimeMillis
    while (i < COUNT) {
      mv + v2;
      i += 1
    }
    println("MutableVector: 1M adds = " + (System.currentTimeMillis - start) + "ms, result=" + mv)

    var fv = new FastVector(0, 0, 0)
    val fv2 = new FastVector(1, 1, 1)
    i = 0
    start = System.currentTimeMillis
    while (i < COUNT) {
      fv = fv + fv2
      i += 1
    }
    println("FastVector+FastVector: 1M adds = " + (System.currentTimeMillis - start) + " ms, result=" + v)

    fv = new FastVector(0, 0, 0)
    val fv3d_2 = new Vector3d(1, 1, 1)
    i = 0
    start = System.currentTimeMillis
    while (i < COUNT) {
      fv = fv + fv3d_2
      i += 1
    }
    println("FastVector+Vector3d: 1M adds = " + (System.currentTimeMillis - start) + " ms, result=" + v)

    var mfv = new MutableFastVector(0, 0, 0)
    val mfv2 = new MutableFastVector(1, 1, 1)
    i = 0
    start = System.currentTimeMillis
    while (i < COUNT) {
      mfv += fv2
      i += 1
    }
    println("MutableFastVector: 1M adds = " + (System.currentTimeMillis - start) + " ms, result=" + v)


    val v3d = new Vector3d(0, 0, 0)
    val v3d_2 = new Vector3d(1, 1, 1)
    i = 0
    start = System.currentTimeMillis
    while (i < COUNT) {
      v3d.add(v3d_2)
      i += 1
    }
    println("Vector3d: 1M adds = " + (System.currentTimeMillis - start) + "ms, result=" + v3d)
  }

  @Test def intersectionCalcBench : Unit = {
    // pseudo-intersection code resembling the sphere intersection test
    val COUNT = 1000000

    var v = new Vector(0, 0, 0)
    var v2 = new Vector(1, 1, 1)
    var i = 0
    var start = System.currentTimeMillis
    var result = 0.0

    while (i < COUNT) {
      val dest = v + v2
      val b = dest * v2
      val c = dest * dest
      val d = b*b - c
      result += d
      i += 1
    }
    println("Vector: 1M ops = " + (System.currentTimeMillis - start) + "ms, result=" + result)

    i = 0
    result = 0.0
    var v3d = new Vector3d(0, 0, 0)
    var v3d_2 = new Vector3d(1, 1, 1)
    start = System.currentTimeMillis

    while (i < COUNT) {
      val dest = new Vector3d
      dest.add(v3d, v3d_2)
      val b = dest.dot(v3d_2)
      val c = dest.dot(dest)
      val d = b*b - c
      result += d
      i += 1
    }

    println("Vector3d: 1M ops = " + (System.currentTimeMillis - start) + "ms, result=" + result)
  }
}

object VectorBenchTest {
  @BeforeClass def warmup() {
    var v = new Vector(0, 0, 0)
    val v2 = new Vector(1, 1, 1)
    for (x <- 0 to 1000) {
      v + v2
      v * v2
      v * 10
    }
    val v3d = new Vector3d(0, 0, 0)
    val v3d_2 = new Vector3d(1, 1, 1)
    for (x <- 0 to 1000) {
      v3d.add(v3d_2)
      v3d.dot(v3d)
    }
  }
}
