package net.lichtd


import _root_.net.lichtd.ray.math.{Color, Vector}
import org.junit._
import Assert._
import ray.math.NumberUtils
@Test
class RayTest {

  @Test def vectorEq = assert (new Vector(1, 2, 3) ==  new Vector(1, 2, 3))
  @Test def vectorNeq = assert (new Vector(1, 2, 3) != new Vector(2, 4, 6))
  @Test def vectorNeg = assert (-new Vector(1, 2, 3) == new Vector(-1, -2, -3))
  @Test def vectorDot = assert (NumberUtils.equal(new Vector(1, 1, 0) * (new Vector(-1, 1, 0)), 0))
  @Test def vectorStretch = assert (new Vector(1, 1, 1) * 5 == new Vector(5, 5, 5))
  @Test def vectorAdd = assert(new Vector(1, 2, 3) + new Vector(3, 2, 1) == new Vector(4, 4, 4))
  @Test def vectorSub = assert(new Vector(2, 3, 5) - new Vector(3, 1, 5) == new Vector(-1, 2, 0))
  @Test def vectorCrush = assert(new Vector(1, 1, 1) / 2.0 == new Vector(0.5, 0.5, 0.5))

  @Test def normVector = {
    assert((new Vector(1, 0, 0) cross new Vector(0, 1, 0)) == new Vector(0, 0, 1)) 
  }

  @Test def colorAdd = assert(new Color(0.5, 0.5, 0.5) + new Color(1, 0, 0.3) == new Color(1, 0.5, 0.8))
  @Test def colorSub = assert(new Color(0.5, 0.5, 0.5) - new Color(1, 0.5, 0.1) == new Color(0, 0, 0.4))
  @Test def colorMul = assert(new Color(0.5, 1, 0.25) * new Color(1, 0.75, 0.5) == new Color(0.5, 0.75, 0.125))

  @Test def rayNormalize = assert(new Vector(20, 0, 0).normalize == new Vector(1, 0, 0),
    "Vector not normalized properly: " + new Vector(20, 0, 0).normalize)


}
