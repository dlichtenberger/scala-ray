package net.lichtd.ray.surface


import net.lichtd.ray.math._
import net.lichtd.ray.scene.Intersection
import net.lichtd.ray.shapes.{SurfaceMapper, SphericalSurfaceMapper}

class CheckeredSurface[T <: SurfaceMapper](
         val target: T,
         val white: SurfaceShader,
         val black: SurfaceShader,
         val n: Int) extends SurfaceShader(0.0, 0.0) {

  def getIntersectionToken(intersection: Intersection) : CheckeredSurface.IntersectionToken = {
    val targetSurface = getTargetSurface(intersection.origin)
    new CheckeredSurface.IntersectionToken(
      targetSurface,
      targetSurface.getIntersectionToken(intersection)
    )
  }

  def getIntersectionColor(token: Any, intersection: Intersection) = {
    val t = token.asInstanceOf[CheckeredSurface.IntersectionToken]
    t.targetSurface.getIntersectionColor(t.targetToken, intersection)
  }

  override def getAmbient(token: Any, intersection: Intersection) = {
    val t = token.asInstanceOf[CheckeredSurface.IntersectionToken]
    t.targetSurface.getAmbient(t.targetToken, intersection)
  }

  override def getReflection(token: Any, intersection: Intersection, reflections: => Color) = {
    val t = token.asInstanceOf[CheckeredSurface.IntersectionToken]
    t.targetSurface.getReflection(t.targetToken, intersection, reflections)
  }

  override def shade(token: Any, view: Vector, intersection: Intersection, light: Vector, lightColor: Color, ambientLight: Color) : Color = {
    val t = token.asInstanceOf[CheckeredSurface.IntersectionToken]
    t.targetSurface.shade(t.targetToken, view, intersection, light, lightColor, ambientLight)
  }

  def getTargetSurface(point: Vector) : SurfaceShader = {
    val coords = target.getSurfaceCoordinates(point)
    val x = Math.round(n * coords.x)
    val y = Math.round(n * coords.y)
    if ((x + y) % 2 == 0) white else black
  }
}

object CheckeredSurface {
  class IntersectionToken(val targetSurface: SurfaceShader, val targetToken: Any)
}