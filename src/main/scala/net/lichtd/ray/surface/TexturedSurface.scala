package net.lichtd.ray.surface

import net.lichtd.ray.maths.{Vector2D, Color}
import net.lichtd.ray.maths.Color.RGBColor
import net.lichtd.ray.scene.Intersection
import net.lichtd.ray.shapes.SurfaceMapper

class TexturedSurface[T <: SurfaceMapper](val target: T, _reflectionCoeff: Double, _specularCoeff: Double, val texture: Array[Array[Int]])
        extends SurfaceShader(_reflectionCoeff, _specularCoeff) {
  val height = texture.length
  val width = texture(0).length

  def getIntersectionToken(intersection: Intersection): Vector2D = target.getSurfaceCoordinates(intersection.origin)

  def getIntersectionColor(token: Any, intersection: Intersection) = {
    // bilinear filtering
    val coords = token.asInstanceOf[Vector2D] // (u,v)
    val hitX = width.asInstanceOf[Double] - Math.max(1.0, coords.x * width)
    val hitY = height.asInstanceOf[Double] - Math.max(1.0, coords.y * height)

    var x = hitX - 0.5
    var y = hitY - 0.5
    var dy = 0.0
    var result : Color = Color.BLACK
    while (dy <= 1) {
      val line = texture(Math.max(0, (y + dy).asInstanceOf[Int] % height))
      var dx = 0.0
      while (dx <= 1) {
        result += RGBColor(line(Math.max(0, (x + dx).asInstanceOf[Int] % width))) / 9
        dx += 0.5
      }
      dy += 0.5
    }
    result
  }

  // non-bilinear version:
  /*
   RGBColor(
   texture(height - Math.max(1, (coords.y * height).asInstanceOf[Int]))
   (width - Math.max(1, (coords.x * width).asInstanceOf[Int]))
   )
   */
}