package unit

import org.specs2.mutable._
import org.specs2.specification.Scope
import climadata.raster._

object DoubleRasterSpec extends Specification {
  "Double raster" should {
    "apply correct value" in new rasterScope {
      val res = raster.apply(0,1)

      (res must beAnInstanceOf[Double]) and (res must beEqualTo(2.0))
    }

    "be converted to an int raster" in new rasterScope {
      raster.toInt must beAnInstanceOf[Raster[Int]]
    }

    "sum to a byte raster" in new rasterScope {
      val byteRaster = Raster(Array(1,2,3,4).map(_.toByte), 2, 2)
      val res = raster + byteRaster

      (res must beAnInstanceOf[Raster[Double]]) and (res(1,1) must beEqualTo(8.0))
    }

    "subtrat to a short raster" in new rasterScope {
      val shortRaster = Raster(Array(1,1,1,1).map(_.toShort), 2, 2)
      val res = raster - shortRaster

      (res must beAnInstanceOf[Raster[Double]]) and (res(0,1) must beEqualTo(1.0))
    }

    "divide to a int raster" in new rasterScope {
      val intRaster = Raster(Array(2,2,2,2), 2, 2)
      val res = raster / intRaster

      (res must beAnInstanceOf[Raster[Double]]) and (res(1,1) must beEqualTo(2.0))
    }

    "mutiply to a float raster" in new rasterScope {
      val doubleRaster = Raster(Array(1,2,3,4).map(_.toFloat), 2, 2)
      val res = raster * doubleRaster

      (res must beAnInstanceOf[Raster[Double]]) and (res(1,1) must beEqualTo(16.0))
    }

    "divide to a double raster" in new rasterScope {
      val doubleRaster = Raster(Array(2,2,2,2).map(_.toDouble), 2, 2)
      val res = raster / doubleRaster

      (res must beAnInstanceOf[Raster[Double]]) and (res(0,0) must beEqualTo(0.5))
    }

    "sum to a byte constant" in new rasterScope {
      val res = raster + 2.toByte

      (res must beAnInstanceOf[Raster[Double]]) and (res(0,1) must beEqualTo(4.0))
    }

    "divide to a double constant" in new rasterScope {
      val res = raster / 2.0

      (res must beAnInstanceOf[Raster[Double]]) and (res(0,0) must beEqualTo(0.5))
    }
  }

  "Double constant" should {
    "sum to a byte raster" in new rasterScope {
      val res = const + raster.toByte

      (res must beAnInstanceOf[Raster[Double]]) and (res(0,1) must beEqualTo(4.0))
    }

    "subtract to a short raster" in new rasterScope {
      val res = const - raster.toShort

      (res must beAnInstanceOf[Raster[Double]]) and (res(0,0) must beEqualTo(1))
    }

    "divide to a int raster" in new rasterScope {
      val res = const / raster.toInt

      (res must beAnInstanceOf[Raster[Double]]) and (res(0,1) must beEqualTo(1))
    }

    "mutiply to a float raster" in new rasterScope {
      val res = const * raster.toFloat

      (res must beAnInstanceOf[Raster[Double]]) and (res(0,1) must beEqualTo(4.0))
    }

    "divide to a double raster" in new rasterScope {
      val res = const / raster.toDouble

      (res must beAnInstanceOf[Raster[Double]]) and (res(1,1) must beEqualTo(0.5))
    }
  }

  trait rasterScope extends Scope {
    lazy val raster = Raster(Array(1,2,3,4).map(_.toDouble), 2, 2)
    lazy val const = 2.0
  }
}