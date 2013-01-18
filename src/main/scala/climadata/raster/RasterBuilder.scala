package climadata.raster

import scala.reflect.{ClassTag}
import scala.language.implicitConversions

object RasterUtils {
  def isNodata(v:Byte) = v == byteNodata
  def isNodata(v:Short) = v == shortNodata
  def isNodata(v:Int) = v == intNodata
  def isNodata(v:Float) = v.isNaN
  def isNodata(v:Double) = v.isNaN

  val byteNodata = Byte.MinValue
  val shortNodata = Short.MinValue
  val intNodata = Int.MinValue
  val floatNodata = Float.NaN
  val doubleNodata = Double.NaN
}

trait RasterBuilder[@specialized A, @specialized B] {
  def isNodata(data:A):Boolean
  def nodata:B
  def map(r:Raster[A], f:A => B):Raster[B]
}

import RasterUtils._

trait RasterBuilders {
  private def mapper[A:ClassTag, B:ClassTag](b: RasterBuilder[A, B], r:Raster[A])(f: A => B):Raster[B] =
    r match {
      case r:ConstantRaster[A] => Raster(f(r.const))
      case r:Raster[A] => Raster(r.data.map(d => if(b.isNodata(d)) b.nodata else f(d)), r.cols, r.rows)
    }

  implicit object B2B extends RasterBuilder[Byte, Byte]{
    def isNodata(data:Byte) = RasterUtils.isNodata(data)
    def nodata = byteNodata
    def map(r: Raster[Byte], f:Byte => Byte) = mapper(this, r)(f)
  }

  implicit object B2S extends RasterBuilder[Byte, Short]{
    def isNodata(data:Byte) = RasterUtils.isNodata(data)
    def nodata = shortNodata
    def map(r: Raster[Byte], f:Byte => Short) = mapper(this, r)(f)
  }

  implicit object B2I extends RasterBuilder[Byte, Int]{
    def isNodata(data:Byte) = RasterUtils.isNodata(data)
    def nodata = intNodata
    def map(r: Raster[Byte], f:Byte => Int) = mapper(this, r)(f)
  }

  implicit object B2F extends RasterBuilder[Byte, Float]{
    def isNodata(data:Byte) = RasterUtils.isNodata(data)
    def nodata = floatNodata
    def map(r: Raster[Byte], f:Byte => Float) = mapper(this, r)(f)
  }

  implicit object B2D extends RasterBuilder[Byte, Double]{
    def isNodata(data:Byte) = RasterUtils.isNodata(data)
    def nodata = doubleNodata
    def map(r: Raster[Byte], f:Byte => Double) = mapper(this, r)(f)
  }

  implicit object S2B extends RasterBuilder[Short, Byte]{
    def isNodata(data:Short) = RasterUtils.isNodata(data)
    def nodata = byteNodata
    def map(r: Raster[Short], f:Short => Byte) = mapper(this, r)(f)
  }

  implicit object S2S extends RasterBuilder[Short, Short]{
    def isNodata(data:Short) = RasterUtils.isNodata(data)
    def nodata = shortNodata
    def map(r: Raster[Short], f:Short => Short) = mapper(this, r)(f)
  }

  implicit object S2I extends RasterBuilder[Short, Int]{
    def isNodata(data:Short) = RasterUtils.isNodata(data)
    def nodata = intNodata
    def map(r: Raster[Short], f:Short => Int) = mapper(this, r)(f)
  }

  implicit object S2F extends RasterBuilder[Short, Float]{
    def isNodata(data:Short) = RasterUtils.isNodata(data)
    def nodata = floatNodata
    def map(r: Raster[Short], f:Short => Float) = mapper(this, r)(f)
  }

  implicit object S2D extends RasterBuilder[Short, Double]{
    def isNodata(data:Short) = RasterUtils.isNodata(data)
    def nodata = doubleNodata
    def map(r: Raster[Short], f:Short => Double) = mapper(this, r)(f)
  }

  implicit object I2B extends RasterBuilder[Int, Byte]{
    def isNodata(data:Int) = RasterUtils.isNodata(data)
    def nodata = byteNodata
    def map(r: Raster[Int], f:Int =>  Byte) = mapper(this, r)(f)
  }

  implicit object I2S extends RasterBuilder[Int, Short]{
    def isNodata(data:Int) = RasterUtils.isNodata(data)
    def nodata = shortNodata
    def map(r: Raster[Int], f:Int =>  Short) = mapper(this, r)(f)
  }

  implicit object I2I extends RasterBuilder[Int, Int]{
    def isNodata(data:Int) = RasterUtils.isNodata(data)
    def nodata = intNodata
    def map(r: Raster[Int], f:Int =>  Int) = mapper(this, r)(f)
  }

  implicit object I2F extends RasterBuilder[Int, Float]{
    def isNodata(data:Int) = RasterUtils.isNodata(data)
    def nodata = floatNodata
    def map(r: Raster[Int], f:Int =>  Float) = mapper(this, r)(f)
  }

  implicit object I2D extends RasterBuilder[Int, Double]{
    def isNodata(data:Int) = RasterUtils.isNodata(data)
    def nodata = doubleNodata
    def map(r: Raster[Int], f:Int =>  Double) = mapper(this, r)(f)
  }

  implicit object F2B extends RasterBuilder[Float, Byte]{
    def isNodata(data:Float) = RasterUtils.isNodata(data)
    def nodata = byteNodata
    def map(r: Raster[Float], f:Float => Byte) = mapper(this, r)(f)
  }

  implicit object F2S extends RasterBuilder[Float, Short]{
    def isNodata(data:Float) = RasterUtils.isNodata(data)
    def nodata = shortNodata
    def map(r: Raster[Float], f:Float => Short) = mapper(this, r)(f)
  }

  implicit object F2I extends RasterBuilder[Float, Int]{
    def isNodata(data:Float) = RasterUtils.isNodata(data)
    def nodata = intNodata
    def map(r: Raster[Float], f:Float => Int) = mapper(this, r)(f)
  }

  implicit object F2F extends RasterBuilder[Float, Float]{
    def isNodata(data:Float) = RasterUtils.isNodata(data)
    def nodata = floatNodata
    def map(r: Raster[Float], f:Float => Float) = mapper(this, r)(f)
  }

  implicit object F2D extends RasterBuilder[Float, Double]{
    def isNodata(data:Float) = RasterUtils.isNodata(data)
    def nodata = byteNodata
    def map(r: Raster[Float], f:Float => Double) = mapper(this, r)(f)
  }

  implicit object D2B extends RasterBuilder[Double, Byte]{
    def isNodata(data:Double) = RasterUtils.isNodata(data)
    def nodata = byteNodata
    def map(r: Raster[Double], f:Double => Byte) = mapper(this, r)(f)
  }

  implicit object D2S extends RasterBuilder[Double, Short]{
    def isNodata(data:Double) = RasterUtils.isNodata(data)
    def nodata = shortNodata
    def map(r: Raster[Double], f:Double => Short) = mapper(this, r)(f)
  }

  implicit object D2I extends RasterBuilder[Double, Int]{
    def isNodata(data:Double) = RasterUtils.isNodata(data)
    def nodata = intNodata
    def map(r: Raster[Double], f:Double => Int) = mapper(this, r)(f)
  }

  implicit object D2F extends RasterBuilder[Double, Float]{
    def isNodata(data:Double) = RasterUtils.isNodata(data)
    def nodata = floatNodata
    def map(r: Raster[Double], f:Double => Float) = mapper(this, r)(f)
  }

  implicit object D2D extends RasterBuilder[Double, Double]{
    def isNodata(data:Double) = RasterUtils.isNodata(data)
    def nodata = doubleNodata
    def map(r: Raster[Double], f:Double => Double) = mapper(this, r)(f)
  }
}

