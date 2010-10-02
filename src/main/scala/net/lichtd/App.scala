package net.lichtd

import java.io.File
import net.lichtd.ray._
import net.lichtd.ray.scene.{ViewPoint, LightSource}
import net.lichtd.ray.shapes.{Sphere, SphericalSurfaceMapper}
import net.lichtd.ray.surface.{CheckeredSurface, ColoredSurface}
import net.lichtd.ray.output.{Screen,TGAWriter}

import net.lichtd.ray.math.{Color, Vector}

object App {
  sealed abstract case class Quality() {
    def apply(screen: Screen)
  }

  case object PREVIEW extends Quality {
    override def apply(screen: Screen) = {
      screen.maxReflections = 1
      screen.lightSourceResolution = 1
      screen.subPixelResolution = 1
    }
  }

  case object LOW extends Quality {
    override def apply(screen: Screen) = {
      screen.maxReflections = 3
      screen.lightSourceResolution = 25
      screen.subPixelResolution = 2
      screen.edgeDetection = true
    }
  }

  case object NORMAL extends Quality {
    override def apply(screen: Screen) = {
      screen.maxReflections = 3
      screen.lightSourceResolution = 25
      screen.subPixelResolution = 2
    }
  }

  case object HIGH extends Quality {
    override def apply(screen: Screen) = {
      screen.maxReflections = 5
      screen.lightSourceResolution = 25
      screen.subPixelResolution = 4
    }
  }

  case object ULTRA extends Quality {
    override def apply(screen: Screen) = {
      screen.maxReflections = 9
      screen.lightSourceResolution = 64
      screen.subPixelResolution = 8
    }
  }

  case object TEST_LIGHTSOURCES extends Quality {
    override def apply(screen: Screen) = {
      screen.maxReflections = 3
      screen.lightSourceResolution = 500
      screen.lightSourceAbortRatio = 0.15
      screen.subPixelResolution = 2
    }
  }

  def main(args: Array[String])  {
    new File("out").mkdir()
    if (args.length == 0 || (args.length == 1 && (args(0) == "-h" || args(0) == "--help"))) {
      println("Usage: scala-ray scene-no [width] [height] [quality]")
      println(" scene-no : [1-5]")
      println(" quality  : [PREVIEW, LOW, NORMAL, HIGH, ULTRA]")
      println()
      exit(1)
    }

    if (System.getProperty("wait") != null) {
      println( "Waiting for keypress" ); System.in.read()
    }

    val scene = Integer.parseInt(args(0))
    val width = if (args.length < 2) 800 else Integer.parseInt(args(1))
    val height = if (args.length < 3) 600 else Integer.parseInt(args(2))
    val quality = {
        if (args.length < 4) {
          NORMAL
        } else {
          args(3) match {
            case "PREVIEW"  => PREVIEW
            case "LOW"      => LOW
            case "NORMAL"   => NORMAL
            case "HIGH"     => HIGH
            case "ULTRA"    => ULTRA
            case x          => println("Unknown quality " + x + ", using normal"); NORMAL
          }
        }
    }
    println("Processing scene " + scene + ", resolution: " + width + "x" + height + ", quality: " + quality)

    if (scene == 1) {
        process(ExampleScenes.test01_spheres(width, height), 0, quality)
    } else if (scene == 2) {
        process(ExampleScenes.test02_earth(width, height, 70, 100), 0, quality)
    } else if (scene == 3) {
      process(ExampleScenes.test03_plane_horizon(width, height), 0, quality)
    } else if (scene == 4) {
       process(ExampleScenes.test04_plane_pool(width, height, true), 0, quality)
    } else if (scene == 5) {
       process(ExampleScenes.test05_spheregrid(width, height), 0, quality) 
    } else {
        println("Unknown scene: " + scene)
        exit(1)
    }
  }

  def process(_screen: => Screen, iteration: Int, quality: Quality) = {
    val startInit = System.currentTimeMillis
    val screen = _screen
    quality.apply(screen)
    //screen.lightSourceResolution = 1
    val printInfo = (iteration % 10 == 0)
    if (printInfo) {
      println("Init Scene:        " + (System.currentTimeMillis - startInit) + "ms")
    }
    val startRender = System.currentTimeMillis
    val result = screen.render(TGAWriter)
    if (printInfo) {
      println("Rendering:         " + (System.currentTimeMillis - startRender) + "ms")
    }
    val startWrite = System.currentTimeMillis
    val filename = String.format("out/test_%05d.tga", iteration.asInstanceOf[Object])
    result.toFile(filename)
    if (printInfo) {
      println("Wrote " + filename)
      println("File output:       " + (System.currentTimeMillis - startWrite) + "ms")
    }
  }
}
