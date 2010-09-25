package net.lichtd

import _root_.net.lichtd.ray.math.Color
import ray.scene.ViewPoint
import ray.output.Screen
import org.junit._
import Assert._
import ray._
import ray.math.Vector

@Test
class ScreenTest {
  @Test def sceneCoordinates = {
    val vp = new ViewPoint(Vector.ORIGIN)
    val scene = new Screen(vp, new Color(0.1), 640, 480)
    println(scene.deltaCol)
    println(scene.deltaRow)
    assertEquals(Vector.ORIGIN, scene.getRay(0, 0).origin)
    assertEquals(vp.direction, scene.getRay(320, 240).direction)
    assert(scene.getRay(0, 0).direction.x < scene.getRay(1, 0).direction.x, "World X coordinates should be ascending")
    assert(scene.getRay(0, 0).direction.y < scene.getRay(0, 1).direction.y, "World Y coordinates should be ascending")
    //println(scene.getRay(320, 0))
    //println(scene.getRay(320, 200))
  }
}