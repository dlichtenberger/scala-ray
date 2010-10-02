package net.lichtd

import org.junit._
import Assert._
import net.lichtd.ray.scene._
import net.lichtd.ray.shapes._
import net.lichtd.ray.math.Vector

@Test
class KDTreeTest {

  @Test def simpleTree : Unit = {
    val root = new Sphere(new Vector(0,0,0), 1.0)
    val tree = KDTree.create(List(root))
    assertEquals(List(root), tree.shapes)
  }

  @Test def simpleSplit : Unit = {
    var left = for (i <- 1 to 10) yield new Sphere(new Vector(-10, 0, i), 0.5)
    var right = for (i <- 1 to 10) yield new Sphere(new Vector(10, 0, i), 0.5)
    val shapes = left ++ right
    assertEquals(20, shapes.size)
    val tree = KDTree.create(shapes, 0, 0)  // split on X axis
    assertEquals(10, tree.left.shapes.size)
    assertEquals(10, tree.right.shapes.size)
    println(tree)
  }
  
}
