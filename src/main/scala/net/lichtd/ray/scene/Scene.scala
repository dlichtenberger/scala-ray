package net.lichtd.ray.scene


import net.lichtd.ray.math._
import net.lichtd.ray.shapes.Shape
import net.lichtd.ray.surface.SurfaceShader
import java.util.concurrent.atomic.AtomicLong

class Scene(val viewPoint: ViewPoint, val ambientLight: Color) {
  val PROP_RECORDSTATS = "stats"    // record ray and trace counts

  var lightSourceResolution = 16 // use a 16 rays per light source
  var lightSourceAbortRatio = 0.3   // percentage of total light source checks before aborting
                                    // when all checks return the same value
  var maxReflections = 2 // maximum number of times a ray might get reflected
  var objects = List[Shape]()
  var lightSources = List[LightSource]()

  val statsEnabled = (System.getProperty(PROP_RECORDSTATS) != null)
  private val primaryRays = new AtomicLong
  private val secondaryRays = new AtomicLong
  private val primaryIntersections = new AtomicLong
  private val secondaryIntersections = new AtomicLong

  def addShape(shape: Shape) = objects ::= shape

  def addShape(shape: Shape, surface: SurfaceShader) = {
    shape.surface = surface
    objects ::= shape
  }

  def addLightSource(source: LightSource) = lightSources ::= source

  /** Called when scene is finished before rendering the first line */
  def beforeRender() : Unit = { }

  def printStats(labelFormat: String) : Unit = {
    if (!statsEnabled) {
      println("Render stats disabled (enable with -D" + PROP_RECORDSTATS + ")")
    } else {
      println("Render stats: ")
      println(labelFormat.format("Primary rays:") + primaryRays)
      println(labelFormat.format("Primary inters:") + primaryIntersections)
      println(labelFormat.format("Secondary rays:") + secondaryRays)
      println(labelFormat.format("Secondary inters:") + secondaryIntersections)
    }
  }

  def cast(ray: Ray): LightResult = cast(null, ray)

  def createBlockFinder(ray: Ray, target: Shape, intersection: Intersection, intersectionToken: Any)
      = new BlockFinder(ray, target, intersection, intersectionToken)
 
  protected def recordPrimaryRays(delta: Int) : Unit = if (statsEnabled) primaryRays.addAndGet(delta)
  protected def recordSecondaryRays(delta: Int) : Unit = if (statsEnabled) secondaryRays.addAndGet(delta)
  protected def recordPrimaryIntersections(delta: Int) : Unit = if (statsEnabled) primaryIntersections.addAndGet(delta)
  protected def recordSecondaryIntersections(delta: Int) : Unit = if (statsEnabled) secondaryIntersections.addAndGet(delta)
  protected def recordIntersections(primary: Boolean, delta: Int) : Unit = {
    if (statsEnabled) {
      if (primary) {
        recordPrimaryIntersections(delta)
      } else {
        recordSecondaryIntersections(delta)
      }
    }
  }


  /**
   * Walk the scene, calling func on every shape that may intersect the ray,
   * stopping when func returned true for the current group of objects.
   * (in the simple scene there is only one group, but e.g. in kd-trees one could
   * stop after hitting an object in a tree node)
   */
   def walkScene[T](from: Shape, ray: Ray, interFunc: (Shape, Ray) => Option[T], func: (Shape, T) => Boolean, interExtractor: T => Vector) {
    // unordered traversal across all shapes in the scene
    var checks = 0
    for (obj <- objects) {
      if (obj != from) {
        interFunc(obj, ray) match {
          case Some(inter: T)    => func(obj, inter) // check rest of objects
          case None              => // continue
        }
        checks += 1
      }
    }
    if (from == null) {
      recordPrimaryIntersections(checks)
    } else {
      recordSecondaryIntersections(checks)
    }
  }

  def cast(from: Shape, ray: Ray): LightResult = {
    (ray.iteration > maxReflections) match {
      case true => EmptyLightResult // stop recursion
      case false =>
        recordPrimaryRays(1)
        var nextObj: (Intersection, Shape, Double) = null
        walkScene(from, ray, _.intersect(_), (obj, inter: Intersection) => {
          val distance = (inter.origin - ray.origin).length
          if (nextObj == null || distance < nextObj._3) {
            nextObj = (inter, obj, distance)
            true
          } else {
            false
          }
        }, (inter: Intersection) => inter.origin)
        if (nextObj != null) 
          calculateLighting(ray, nextObj._2, nextObj._1)
        else
          EmptyLightResult
          //new LightResult(Color.DColor(0.01) * checks, None, 0)
    }
  }

  /** @return whether traversal is ordered, i.e. can stop at the first intersection */
  protected def isOrderedTraversal = false

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
    val it = lightSources.iterator
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
        val iter = ls.getOrigins(intersection.origin, lightSourceResolution).iterator
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
      val iterator = origins.iterator
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
      recordSecondaryRays(1)
      var result: Shape = null
      walkScene(target, ray, _.intersectionPoint(_), (shape, inter: Vector) => {
        // check if the intersection is before or after our target object
        if ((inter - lsOrigin).length < vector.length) {
          // blocked
          result = shape
          true
        } else {
          false
        }
      }, (inter: Vector) => inter)
      if (result == null) None else Some(result)
    }
  }

  /** Base wrapper class for extension to the Block Finder */
  abstract class WrappedBlockFinder(wrapped: BlockFinder) extends BlockFinder(
    wrapped.ray, wrapped.target, wrapped.intersection, wrapped.token
  )

}
