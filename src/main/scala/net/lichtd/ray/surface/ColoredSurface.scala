package net.lichtd.ray.surface

import net.lichtd.ray.maths.Color
import net.lichtd.ray.scene.Intersection

class ColoredSurface(val objectColor: Color, _specularCoeff: Double, _reflectionCoeff: Double)
        extends SurfaceShader(_reflectionCoeff, _specularCoeff) {

  def getIntersectionToken(intersection: Intersection) = null

  def getIntersectionColor(token: Any, intersection: Intersection) = objectColor

}
