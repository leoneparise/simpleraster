package unit

import org.specs2.mutable._
import org.specs2.specification.Scope
import climadata.raster._

object IntRasterSpec extends Specification {
  "Int raster" should {
    "apply correct value" in new rasterScope {
      val res = raster(0,1)

      (res must beAnInstanceOf[Int]) and (res must beEqualTo(2))
    }

    "be converted to a float raster" in new rasterScope {
      raster.map(_.toFloat) must beAnInstanceOf[Raster[Float]]
    }

    "sum to a byte raster" in new rasterScope {
      val byteRaster = Raster(Array(1,2,3,4), 2, 2).map(_.toByte)
      val res = raster + byteRaster

      (res must beAnInstanceOf[Raster[Int]]) and (res(1,1) must beEqualTo(8))
    }

    "subtrat to a short raster" in new rasterScope {
      val shortRaster = Raster(Array(1,1,1,1), 2, 2).map(_.toShort)
      val res = raster - shortRaster

      (res must beAnInstanceOf[Raster[Int]]) and (res(0,1) must beEqualTo(1))
    }

    "divide to a int raster" in new rasterScope {
      val intRaster = Raster(Array(2,2,2,2), 2, 2)
      val res = raster / intRaster

      (res must beAnInstanceOf[Raster[Int]]) and (res(1,1) must beEqualTo(2))
    }

    "mutiply to a float raster" in new rasterScope {
      val doubleRaster = Raster(Array(1,2,3,4), 2, 2).map(_.toFloat)
      val res = raster * doubleRaster

      (res must beAnInstanceOf[Raster[Double]]) and (res(1,1) must beEqualTo(16.0))
    }

    "divide to a double raster" in new rasterScope {
      val doubleRaster = Raster(Array(2,2,2,2), 2, 2).map(_.toDouble)
      val res = raster / doubleRaster

      (res must beAnInstanceOf[Raster[Double]]) and (res(0,0) must beEqualTo(0.5))
    }

    "sum to a byte constant" in new rasterScope {
      val res = raster + 2.toByte

      (res must beAnInstanceOf[Raster[Int]]) and (res(0,1) must beEqualTo(4))
    }

    "modulus to a short constant" in new rasterScope {
      val res = raster % 2.toShort

      (res must beAnInstanceOf[Raster[Int]]) and (res(1,1) must beEqualTo(0))
    }

    "and to a int constant" in new rasterScope {
      val res = raster & 2

      (res must beAnInstanceOf[Raster[Int]]) and (res(1,0) must beEqualTo(2))
    }

    "or to a byte constant" in new rasterScope {
      val res = raster | 2.toByte

      (res must beAnInstanceOf[Raster[Int]]) and (res(1,0) must beEqualTo(3))
    }

    "divide to a double constant" in new rasterScope {
      val res = raster / 2.0

      (res must beAnInstanceOf[Raster[Double]]) and (res(0,0) must beEqualTo(0.5))
    }
  }

  "Int constant" should {
    "sum to a byte raster" in new rasterScope {
      val res = const + raster.map(_.toByte)

      (res must beAnInstanceOf[Raster[Int]]) and (res(0,1) must beEqualTo(4.0))
    }

    "subtract to a short raster" in new rasterScope {
      val res = const - raster.map(_.toShort)

      (res must beAnInstanceOf[Raster[Int]]) and (res(0,0) must beEqualTo(1))
    }

    "divide to a int raster" in new rasterScope {
      val res = const / raster.map(_.toInt)

      (res must beAnInstanceOf[Raster[Int]]) and (res(0,1) must beEqualTo(1))
    }

    "mutiply to a float raster" in new rasterScope {
      val res = const * raster.map(_.toFloat)

      (res must beAnInstanceOf[Raster[Float]]) and (res(0,1) must beEqualTo(4.0))
    }

    "divide to a double raster" in new rasterScope {
      val res = const / raster.map(_.toDouble)

      (res must beAnInstanceOf[Raster[Double]]) and (res(1,1) must beEqualTo(0.5))
    }
  }

  trait rasterScope extends Scope {
    lazy val raster = Raster(Array(1,2,3,4), 2, 2)
    lazy val const = 2
  }
}