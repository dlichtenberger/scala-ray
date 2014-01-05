package net.lichtd

import _root_.net.lichtd.ray.maths.Vector
import org.junit.Test
import ray.scene.ViewPoint

@Test
class ViewPointTest {
  @Test def viewpointUpVector = {
    val vp = new ViewPoint(Vector.ORIGIN,
              new Vector(0, 0, 1),
              new Vector(0, 1, 0)
    )
    assert(vp.up == new Vector(0, 1, 0), "Invalid up vector: " + vp.up)
  }

  @Test def viewpointUp45 = {
    val vp = new ViewPoint(Vector.ORIGIN,
              new Vector(0, -1, 1),   // look down with -45 degrees
              new Vector(0, 1, 0)
    )
    assert(vp.up == new Vector(0, 1, 1).normalize, "Invalid up vector: " + vp.up)
  }

  @Test def viewpointRightVector = {
    val vp = new ViewPoint(Vector.ORIGIN,
              new Vector(0, 0, 1),
              new Vector(0, 1, 0)
    )
    assert(vp.right == new Vector(1, 0, 0), "Invalid right vector: " + vp.right)
  }

  @Test def viewpointRight45 = {
    val vp = new ViewPoint(Vector.ORIGIN,
              new Vector(0, -1, 1),
              new Vector(0, 1, 0)
    )
    assert(vp.right == new Vector(1, 0, 0), "Invalid right vector: " + vp.right)
  }

  @Test def viewpointRight45_2 = {
    val vp = new ViewPoint(Vector.ORIGIN,
              new Vector(1, -1, 1),
              new Vector(0, 1, 0)
    )
    assert(vp.right == new Vector(1, 0, -1).normalize, "Invalid right vector: " + vp.right)
  }
}