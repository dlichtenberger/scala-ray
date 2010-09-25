package net.lichtd.ray.scene


import net.lichtd.ray.math._
import net.lichtd.ray.shapes.Shape
import net.lichtd.ray.surface.SurfaceShader

class Scene(val viewPoint: ViewPoint, val ambientLight: Color) {
  var lightSourceResolution = 16 // use a 16 rays per light source
  var lightSourceAbortRatio = 0.3   // percentage of total light source checks before aborting
                                    // when all checks return the same value
  var maxReflections = 2 // maximum number of times a ray might get reflected
  var objects = List[Shape]()
  var lightSources = List[LightSource]()

  def addShape(shape: Shape) = objects ::= shape

  def addShape(shape: Shape, surface: SurfaceShader) = {
    shape.surface = surface
    objects ::= shape
  }

  def addLightSource(source: LightSource) = lightSources ::= source

  def cast(ray: Ray): LightResult = cast(null, ray)

  def createBlockFinder(ray: Ray, target: Shape, intersection: Intersection, intersectionToken: Any)
      = new BlockFinder(ray, target, intersection, intersectionToken)

  def cast(from: Shape, ray: Ray): LightResult = (ray.iteration > maxReflections) match {
    case true => EmptyLightResult // stop recursion
    case false =>
      var nextObj: (Intersection, Shape, Double) = null
      val it = objects.elements
      while (it.hasNext) {
        val obj = it.next
        if (obj != from) {
          obj.intersect(ray) match {
            case Some(inter: Intersection) =>
              val distance = (inter.origin - ray.origin).length
              if (nextObj == null || distance < nextObj._3) {
                nextObj = (inter, obj, distance)
              }
            case None => // try next
          }
        }
      }

      if (nextObj != null) 
        calculateLighting(ray, nextObj._2, nextObj._1)
      else
        EmptyLightResult
  }

  case class LightResult(val color: Color, val shape: Option[Shape], val visibleLightSources: Int)
  object EmptyLightResult extends LightResult(Color.BLACK, None, 0)

  def calculateLighting(ray: Ray, target: Shape, intersection: Intersection): LightResult = {
    if (target.surface == null) {
      return EmptyLightResult
    }
    val token = target.surface.getIntersectionToken(intersection)
    lazy val reflectedRay = intersection.reflect(ray.direction)
    lazy val reflected: Color = cast(target, new Ray(intersection.origin, reflectedRay, ray.iteration + 1)).color
    // get base object color
    var color: Color =
    target.surface.getReflection(token, intersection, reflected) +
            (ambientLight * target.surface.getAmbient(token, intersection))
    // calculate lightsources
    val blockFinder = createBlockFinder(ray, target, intersection, token)
    val it = lightSources.elements
    while (it.hasNext) {
      color += blockFinder.shadeTarget(it.next)
    }
    LightResult(color, Some(target), blockFinder.visibleLightSources)
  }

  class BlockFinder(val ray: Ray, val target: Shape, val intersection: Intersection, val token: Any) {
    // number of fully visible lightsources
    var visibleLightSources = 0

    def shadeTarget(ls: LightSource) : Color = coarseShadeTarget(ls) match {
      case Some(x)  => x  // we assume that the lightsource is completely visible/occluded
      case None     =>
        // partially occluded/visible
        var result: Color = Color.BLACK
        // first do a coarser check to see if the light source is completely occluded/visible
        val iter = ls.getOrigins(intersection.origin, lightSourceResolution).elements
        while (iter.hasNext) {
          result += processLightOrigin(ls, iter.next)
        }
        result
    }

    private def coarseShadeTarget(ls: LightSource) : Option[Color] = {
      if (lightSourceAbortRatio >= 0.9) {
        // no need to do coarse preview
        return None
      }
      val origins = ls.getOrigins(intersection.origin, (lightSourceResolution.toDouble * lightSourceAbortRatio).asInstanceOf[Int])
      var lastBlock : Option[Boolean] = None
      val iterator = origins.elements
      while (iterator.hasNext) {
        var blocked : Boolean = getBlockingObject(target, intersection.origin, iterator.next) != None
        if (lastBlock == None) {
          lastBlock = Some(blocked)
        } else if (lastBlock.get != blocked) {
          // partially occluded
          return None
        }
      }
      lastBlock match {
        case Some(true) =>  
          Some(Color.BLACK) // blocked
          
        case _ =>
          visibleLightSources += 1
          // not blocked, shade surface
          Some(target.surface.shade(token,
                                    -ray.direction, intersection,
                                    (ls.getOrigins(intersection.origin, 1)(0) - intersection.origin).normalize,
                                    ls.color, ambientLight
            ))
      }
    }

    def processLightOrigin(ls: LightSource, origin: Vector) : Color = {
      getBlockingObject(target, intersection.origin, origin) match {
        case Some(shape) =>
          Color.BLACK   // blocked
        case None =>
          target.surface.shade(token,
            -ray.direction, intersection,
            (origin - intersection.origin).normalize, ls.color, ambientLight
            ) / ls.getOriginCount(lightSourceResolution)
      }
    }

    def getBlockingObject(target: Shape, point: Vector, lsOrigin: Vector): Option[Shape] = {
      val vector = point - lsOrigin
      val ray = new Ray(lsOrigin, vector)
      val it = getBlockingCandidates(ray).elements
      var result: Shape = null
      while (it.hasNext && result == null) {
        val obj = it.next
        if (!obj.eq(target)) {
          obj.intersectionPoint(ray) match {
            case Some(inter) =>
              // check if the intersection is before or after our target object
              if ((inter - lsOrigin).length < vector.length) {
                // blocked
                result = obj
              }
            case None =>
          // not blocked
          }
        }
      }
      if (result == null) None else Some(result)
    }

    /**
     * Returns the shapes that should be checked for a collision with the given ray.
     *
     * @param ray the ray to be casted
     * @param target the target that should be reached by the ray
     */
    protected def getBlockingCandidates(ray: Ray): List[Shape] = objects
  }


}
