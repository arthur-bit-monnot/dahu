package dahu.geometry

import java.awt.geom
import java.awt.geom.{Line2D, Point2D}

import dahu.geometry.Shape.{Circle, Point, Polygon}

trait ShapeTC[T] {
  def center(t: T): (Double, Double) = (centerX(t), centerY(t))
  def centerX(t: T): Double
  def centerY(t: T): Double

  /** Distance to the point. the returned distance is <= 0 if the point is contained by the shape. */
  def distToPoint(t: T, x: Double, y: Double): Double
  def contains(t: T, x: Double, y: Double): Boolean
}

object ShapeTC {

  implicit object PointShape extends ShapeTC[Point] {
    override def centerX(t: Point): Double = t.x
    override def centerY(t: Point): Double = t.y
    override def distToPoint(t: Point, x: Double, y: Double): Double =
      geom.Point2D.distance(t.x, t.y, x, y)
    override def contains(t: Point, x: Double, y: Double): Boolean =
      t.x == x && t.y == y
  }

  implicit object CircleShape extends ShapeTC[Circle] {
    override def centerX(t: Circle): Double = t.x
    override def centerY(t: Circle): Double = t.y
    override def distToPoint(t: Circle, x: Double, y: Double): Double =
      geom.Point2D.distance(t.x, t.y, x, y) - t.radius
    override def contains(t: Circle, x: Double, y: Double): Boolean =
      distToPoint(t, x, y) <= 0.0
  }

  implicit object PolygonShape extends ShapeTC[Polygon] {
    override def centerX(t: Polygon): Double = ???
    override def centerY(t: Polygon): Double = ???
    override def distToPoint(t: Polygon, x: Double, y: Double): Double = {
      val n = t.npoints
      require(n > 0)
      var minDist = Double.PositiveInfinity
      var i = 0
      while(i < n) {
        val x1 = t.xpoints(i)
        val y1 = t.ypoints(i)
        val x2 = if(i == n - 1) t.xpoints(0) else t.xpoints(i + 1)
        val y2 = if(i == n - 1) t.ypoints(0) else t.ypoints(i + 1)
        val d = geom.Line2D.ptSegDist(x1, y1, x2, y2, x, y)
        minDist = math.min(minDist, d)
        i += 1
      }
      if(contains(t, x, y))
        -minDist
      else
        minDist
    }
    override def contains(t: Polygon, x: Double, y: Double): Boolean = {
      val outX = t.minX - 1.0
      val outY = 0.0
      var crossings = 0
      val n = t.npoints
      var i = 0
      while(i < n) {
        val x1 = t.xpoints(i)
        val y1 = t.ypoints(i)
        val x2 = if(i == n - 1) t.xpoints(0) else t.xpoints(i + 1)
        val y2 = if(i == n - 1) t.ypoints(0) else t.ypoints(i + 1)

        val isCrossing = Line2D.linesIntersect(outX, outY, x, y, x1, y1, x2, y2)
        if(isCrossing)
          crossings += 1

        i += 1
      }
      crossings % 2 == 1
    }
  }

}

sealed abstract class Shape {
  def distTo(x: Double, y: Double): Double
  def contains(x: Double, y: Double): Boolean
}

object Shape {

  case class Point(x: Double, y: Double)

  case class Circle(x: Double, y: Double, radius: Double)

  class Polygon(val xpoints: Array[Double], val ypoints: Array[Double]) {
    require(xpoints.length == ypoints.length)
    require(npoints > 1)
    def npoints: Int = xpoints.length

    def minX: Double = xpoints.min
    def maxX: Double = xpoints.max
    def minY: Double = xpoints.min
    def maxY: Double = ypoints.max

    override def toString: String = s"poly(${xpoints.zip(ypoints).mkString(", ")})"
  }

  def point(x: Double, y: Double): Shape = new ShapeImpl[Point](Point(x, y))

  def circle(x: Double, y: Double, radius: Double): Shape =
    new ShapeImpl[Circle](Circle(x, y, radius))

  def polygon(points: (Double, Double)*): Shape = {
    val xs = new Array[Double](points.size)
    val ys = new Array[Double](points.size)
    for(((x, y), i) <- points.zipWithIndex) {
      xs(i) = x
      ys(i) = y
    }

    new ShapeImpl[Polygon](new Polygon(xs, ys))
  }

  def rect(x: Double, y: Double, h: Double, w: Double): Shape =
    polygon((x, y), (x + h, y), (x + h, y + w), (x, y + w))

  final case class ShapeImpl[S](s: S)(implicit m: ShapeTC[S]) extends Shape {
    override def distTo(x: Double, y: Double): Double = m.distToPoint(s, x, y)
    override def contains(x: Double, y: Double): Boolean = m.contains(s, x, y)
  }
}
