package net.lichtd.ray.surface

import net.lichtd.ray.math.{Color, Vector}
import net.lichtd.ray.scene.Intersection
import net.lichtd.ray.shapes.Shape

class ColoredSurface(val objectColor: Color, _specularCoeff: Double, _reflectionCoeff: Double)
        extends SurfaceShader(_reflectionCoeff, _specularCoeff) {

  def getIntersectionToken(intersection: Intersection) = null

  def getIntersectionColor(token: Any, intersection: Intersection) = objectColor

}
