package net.lichtd.ray.surface


import net.lichtd.ray.maths._
import net.lichtd.ray.scene.Intersection

abstract class SurfaceShader(val reflectionCoeff: Double, val specularCoeff: Double) {

  // return a token that caches information about this intersection (e.g. surface coordinates)
  def getIntersectionToken(intersection: Intersection) : Any

  def getIntersectionColor(token: Any, intersection: Intersection): Color

  def getAmbient(token: Any, intersection: Intersection) : Color = getIntersectionColor(token, intersection)

  def getReflection(token: Any, intersection: Intersection, reflections: => Color) : Color = {
    if (reflectionCoeff > 0) reflections * reflectionCoeff else Color.BLACK
  }

  def shade(token: Any, view: Vector, intersection: Intersection, light: Vector, lightColor: Color, ambientLight: Color) : Color = {
    phongShaded(getIntersectionColor(token, intersection), view, intersection.surfaceNormal, light, lightColor)
  }

  protected def phongShaded(targetColor: Color, view: Vector, surfaceNormal: Vector, light: Vector, lightColor: Color) = {
    // diffuse
    val diffuse = targetColor * (light * surfaceNormal)
    if (specularCoeff > 0) {
      lightColor * (diffuse + (lightColor * Math.pow(Math.max(0, view * (light reflectAround surfaceNormal)), specularCoeff)))
    } else {
      lightColor * diffuse
    }
  }
}
