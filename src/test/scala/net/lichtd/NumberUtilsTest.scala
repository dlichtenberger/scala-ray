package net.lichtd

import org.junit._
import Assert._
import net.lichtd.ray.maths.NumberUtils

@Test
class NumberUtilsTest {
  @Test def testEq0 = assertEquals(NumberUtils.equal(0.0, 0.000000000000000001), true)
  @Test def testEqDiv = assertEquals(NumberUtils.equal(23.12345 / 23.12345, 1.0), true)
  @Test def testNeq = assertEquals(NumberUtils.equal(1.0, 1.01), false)
}
