package net.lichtd.ray

import net.lichtd.ray.math.{Color, Vector}
import net.lichtd.ray.math.Color._
import scala.collection.mutable.HashMap
import scala.actors.Actor
import scala.actors.Actor._

import java.io.File
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import output.Screen
import scene._
import shapes.{Sphere, SphericalSurfaceMapper, Plane}
import surface.{TexturedSurface, CheckeredSurface, ColoredSurface}

object ExampleScenes {
    
  /**
   * First test scene: red sphere with four blue "knobs", a green planet, an a large checkered "ground" sphere
   */
  def test01_spheres(screenWidth: Int, screenHeight: Int) : Screen = {
    val screen = makeScreen(new ViewPoint(
      new Vector(0, 1.3, -2.0),    // from
      new Vector(0, -0.1, 1),    // direction
      new Vector(0, 1, 0)),      // up
      DColor(0.2),           // ambient light
      screenWidth, screenHeight)

    // black/white checkered sphere
    val sphere = new Sphere(new Vector(0, -500, 0), 500) with SphericalSurfaceMapper {
      val north = new Vector(0, 0, 1)
      val equator = new Vector(1, 0, 0)
    }
    val blackCheckers = new ColoredSurface(DColor(0), 50.0, 0.8)
    val whiteCheckers = new ColoredSurface(DColor(1), 50.0, 0.2)
    sphere.surface = new CheckeredSurface(sphere, whiteCheckers, blackCheckers, 1200)
    screen.addShape(sphere)

    // main red sphere
    val redSphere = new Sphere(new Vector(0, 1.5, 3), 1.0)
    screen.addShape(redSphere, new ColoredSurface(DColor(1, 0, 0), 10.0, 0.3))
    // four blue core elements
    List(-0.5, 0.5).foreach(x => List(-0.5, 0.5).foreach(y => {
            val blueSphere = new Sphere(new Vector(x, 1.5 + y, 2.75), 0.4)
            screen.addShape(blueSphere, new ColoredSurface(DColor(0, 0, 1), 10.0, 0.2))
    }))
    // a green planet providing some shadowing
    val greenSphere = new Sphere(new Vector(-0.8, 1.5, 1.5), 0.3)
    screen.addShape(greenSphere, new ColoredSurface(DColor(0, 1, 0), 10.0, 0))

    // two lightsources from the view position
    //screen.addLightSource(new PointLightSource(new Vector(-2, 2, 0), DColor(0.7)))
    screen.addLightSource(new PointLightSource(new Vector(0, 2, 0), DColor(0.5)))
    screen
  }

  def test02_earth(screenWidth: Int, screenHeight: Int, currentStep: Int, steps: Int) : Screen = {
    val fromX = 0.75 - 1.9 * scala.math.sin(currentStep * scala.math.Pi / steps / 3)
    val earthRotation = - scala.math.Pi / 2 / steps * currentStep
    val screen = makeScreen(new ViewPoint(
      new Vector(fromX * 2, 1.3, -(fromX + 0.67) * 18),    // from
      new Vector(-fromX / 4, -0.1, 1).normalize,    // direction 
      new Vector(0, 1, 0)),      // up
      DColor(0.2),           // ambient light
      screenWidth, screenHeight)
    
    // "earth"
    val sphere = new Sphere(new Vector(0, -200, 1000), 500) with SphericalSurfaceMapper {
      val north = new Vector(0, 1, 0.2).normalize
      val equator = new Vector(1, 0, 0).rotateAround(north, earthRotation)
    }
    //println("*** phi=" + earthRotation + ", EQUATOR = " + sphere.equator)
    sphere.surface = new TexturedSurface(sphere, 0, 20, loadImage("textures/Earth.jpg"))
//    sphere.surface = new TexturedSurface(sphere, 0, 20, loadImage("textures/land_ocean_ice_2048-1.png"))
//    sphere.surface = new TexturedSurface(sphere, 0, 20, loadImage("textures/earthmap1k.png"))
    screen.addShape(sphere)

    // main red sphere
    val redSphere = new Sphere(new Vector(0, 0, 3), 1.0)
    screen.addShape(redSphere, new ColoredSurface(DColor(1, 0, 0), 10.0, 0.3))

    // a green planet providing some shadowing
    val greenSphere = new Sphere(new Vector(-0.8, redSphere.origin.y - 0.2, 1.5), 0.3)
    screen.addShape(greenSphere, new ColoredSurface(DColor(0, 1, 0), 10.0, 0))
    // a circle of grey spheres

    val beltSize = 20 // 20 spheres
    val distance = redSphere.radius * 1.5
    var phi : Double = earthRotation  // rotatation rate linked to earth
    for (step <- 0 until beltSize) {
      val center = new Vector(scala.math.cos(phi) * distance, scala.math.sin(phi) * 0.3 + scala.math.cos(phi) * 0.5, scala.math.sin(phi) * distance)
      val asteroid = new Sphere(redSphere.origin + center, 0.1)
      screen.addShape(asteroid, new ColoredSurface(DColor(0.5), 20, 0))
      phi += 2 * scala.math.Pi / beltSize
    }

//    screen.addLightSource(new SquareLightSource(new Vector(-1, 1.5, -10), new Vector(1, 0, 0), new Vector(0, 1, 0), DColor(0.5)))
    screen.addLightSource(new SphericalLightSource(new Vector(-1, 1.5, -10), 1.0, DColor(0.5)))
    screen
  }

  def test03_plane_horizon(screenWidth: Int, screenHeight: Int) : Screen = {
    val screen = makeScreen(new ViewPoint(
      new Vector(0, 1.3, -2.0),    // from
      new Vector(0, 0, 1).normalize,    // direction
      new Vector(0, 1, 0)),      // up
      DColor(0.5),           // ambient light
      screenWidth, screenHeight)

    val plane = new Plane(new Vector(0, 1, 0), 0)
    screen.addShape(plane, new ColoredSurface(DColor(0, 1, 0), 20.0, 0.0))

    val sky = new Plane(new Vector(0, -1, 0), 500)
    screen.addShape(sky, new ColoredSurface(DColor(0, 0, 1), 20.0, 0.0))

    screen.addLightSource(new PointLightSource(new Vector(0, 5, 10), DColor(0.5)))
    
    screen
  }

  def test04_plane_pool(screenWidth: Int, screenHeight: Int, useTextures: Boolean) : Screen = {
    val screen = makeScreen(
      new ViewPoint(
        new Vector(0, 0.6, 0.2),            // from
        new Vector(0, -1, 1),         // direction
        new Vector(0, 1, 0)),
      DColor(0.0),
      screenWidth, screenHeight
    )

    val green = new Plane(new Vector(0, 1, 0), 0)
    screen.addShape(green, new ColoredSurface(IColor(0, 82, 0), 20.0, 0.0))

    val peak = new Vector(0, 0, 0.5)
    val ballRadius = 0.1
    val ballColors = List(
      IColor(238, 168, 19),
      IColor(135, 19, 30),
      IColor(51, 39, 103),
      IColor(18, 147, 91),
      IColor(27, 76, 213),
      IColor(238, 54, 42)
    )
    // enumerate ball numbers (1-8) from peak to bottem
    val ballOrder = List(9, 7, 12, 15, 8, 1, 6, 10, 3, 14, 11, 2, 13, 4, 5)
    var ballNum = 0
    val rotationRnd = new scala.util.Random(4)
    for (row <- 0 to 4) {
      for (col <- 0 to row) {
        val ball = new Sphere(
            new Vector(0, ballRadius, 0)  // place on green
                + peak                    // move to peak
                - new Vector(0, 0, -1) * ballRadius * row * 1.5 // move to row
                + new Vector(1, 0, 0) * ballRadius * (      // move to column
                1             // account for the ball itself - center ball around peak
                - (row + 1)   // account for total balls
                + col * 2     // goto column
              ), ballRadius) with SphericalSurfaceMapper {
                val north = new Vector(0, 1, 0).rotateAround(new Vector(1, 0, 0), (rotationRnd.nextDouble - 1) * scala.math.Pi / 4)
                val equator = new Vector(1, 0, 0).rotateAround(north, (rotationRnd.nextDouble + 1) * scala.math.Pi / 4)
              }
        if (useTextures) {
          // use ball textures from textures/poolball
          // (taken from http://www.robinwood.com/Catalog/FreeStuff/Textures/TexturePages/BallMaps.html)
          screen.addShape(ball, new TexturedSurface(ball, 0, 50.0,
            loadImage("textures/poolball/Ball" + ballOrder(ballNum) + ".jpg"))
          )
        } else {
          // use single colored ball
          val color = if (row == 2 && col == 1) DColor(0.1) else ballColors(ballNum % ballColors.size)
          screen.addShape(ball, new ColoredSurface(color, 50.0, 0.0))
        }
        ballNum += 1
        //screen.addLightSource(new PointLightSource(ball.origin + new Vector(0, 1, 0) - (ball.origin - peak) * 3, IColor(0.03)))
      }
    }

    // create quadratic light sources in the Y-plane
    screen.addLightSource(new SquareLightSource(new Vector(-2, 4.0, -4), new Vector(1, 0, 0), new Vector(0, 0, 1), DColor(0.7)))
    //screen.addLightSource(new SquareLightSource(new Vector(-2, 4.0, -2), 0.2, IColor(0.3)))
    screen.addLightSource(new SquareLightSource(new Vector(-8, 6.0, 0), new Vector(1, 0, 0), new Vector(0, 0, 1), DColor(0.4)))

    screen
  }

  /**
   * Render a monochrom grid of speres against a gray background
   */
  def test05_spheregrid(screenWidth: Int, screenHeight: Int) : Screen = {
    val screen = makeScreen(new ViewPoint(
      new Vector(0, 1.3, -2.0),    // from
      new Vector(0, -0.4, 1),    // direction
      new Vector(0, 1, 0)),      // up
      DColor(0.2),           // ambient light
      screenWidth, screenHeight)

    val gridSize = 10
    val gridStep = 0.7
    val center = gridSize * gridStep / 2
    for (x <- 0 to gridSize; z <- 0 to gridSize; y <- 0 to gridSize) {
      val sphere = new Sphere(new Vector(x * gridStep - center, y * gridStep - center, z * gridStep - center), 0.05)
      sphere.surface = new ColoredSurface(DColor(0.3), 0.0, 0.0)
      screen.addShape(sphere)
    }
/*
    val floor = new Plane(new Vector(0, 1, 0), 5.0)
    floor.surface = new ColoredSurface(DColor(0.25), 0.0, 0.0)
    screen.addShape(floor)
    val wall = new Plane(new Vector(0, 0, -1), 10.0)
    wall.surface = new ColoredSurface(DColor(0.15), 0.0, 0.0)
    screen.addShape(wall)
*/
    screen.addLightSource(new SquareLightSource(new Vector(-3, 1.5, -3), new Vector(1, 0, 0), new Vector(0, 1, 0), DColor(1.0)))
    screen
  }

  private def makeScreen(vp: ViewPoint, ambient: Color, width: Int, height: Int) : Screen = 
    new MyScreen(vp, ambient, width, height)
  
  // always 'compiled in', enabled/disabled at runtime
  private class MyScreen(vp: ViewPoint, ambient: Color, width: Int, height: Int) 
    extends Screen(vp, ambient, width, height)
    with ShadowCacheScene
    with KDTreeScene

  val cachedTextures = new HashMap[String, Array[Array[Int]]]()
  private def extractLine(bi: BufferedImage, line: Int) : Array[Int] = {
    val result = new Array[Int](bi.getWidth)
    bi.getRGB(0, line, bi.getWidth, 1, result, 0, 1)
    result
  }

  def loadImage(fn: String): Array[Array[Int]] = cachedTextures.get(fn) match {
  case Some(x) => x
  case None =>
      val bi = ImageIO.read(new File(fn))
      val start = System.currentTimeMillis

      // serialized version
      val result = new Range(0, bi.getHeight, 1).toArray.map((y: Int) => extractLine(bi, y))

      // parallelized version
    /*  val result = new Mapper(
        new Range(0, bi.getHeight, 1).toList,
        (y: Int) => extractLine(bi, y)
      ).pmap*/

      println("Texture processing: " + (System.currentTimeMillis() - start) + "ms")
      println("Loaded texture: " + bi.getWidth + "x" + bi.getHeight + " pixels")
      cachedTextures += (fn -> result)
      result
  }

  // Taken from http://debasishg.blogspot.com/2008/06/playing-around-with-parallel-maps-in.html
/*  class Mapper[A, B](l: List[A], f: A => B) {
    def pmap = {
      val buffer = new Array[B](l.length)
      val mappers =
        for(idx <- (0 until l.length).toList) yield {
          scala.actors.Futures.future {
            buffer(idx) = f(l(idx))
        }
      }
      for(mapper <- mappers) mapper()
      buffer
    }
  }*/

}
