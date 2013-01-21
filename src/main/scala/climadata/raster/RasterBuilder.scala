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
  implicit object B2B extends RasterBuilder[Byte, Byte]{
    def isNodata(data:Byte) = RasterUtils.isNodata(data)
    def nodata = byteNodata
    def map(r: Raster[Byte], f:Byte => Byte) = r match {
      case r:ConstantRaster[Byte] => Raster(f(r.const))
      case r:Raster[Byte] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object B2S extends RasterBuilder[Byte, Short]{
    def isNodata(data:Byte) = RasterUtils.isNodata(data)
    def nodata = shortNodata
    def map(r: Raster[Byte], f:Byte => Short) = r match {
      case r:ConstantRaster[Byte] => Raster(f(r.const))
      case r:Raster[Byte] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object B2I extends RasterBuilder[Byte, Int]{
    def isNodata(data:Byte) = RasterUtils.isNodata(data)
    def nodata = intNodata
    def map(r: Raster[Byte], f:Byte => Int) = r match {
      case r:ConstantRaster[Byte] => Raster(f(r.const))
      case r:Raster[Byte] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object B2F extends RasterBuilder[Byte, Float]{
    def isNodata(data:Byte) = RasterUtils.isNodata(data)
    def nodata = floatNodata
    def map(r: Raster[Byte], f:Byte => Float) = r match {
      case r:ConstantRaster[Byte] => Raster(f(r.const))
      case r:Raster[Byte] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object B2D extends RasterBuilder[Byte, Double]{
    def isNodata(data:Byte) = RasterUtils.isNodata(data)
    def nodata = doubleNodata
    def map(r: Raster[Byte], f:Byte => Double) = r match {
      case r:ConstantRaster[Byte] => Raster(f(r.const))
      case r:Raster[Byte] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object S2B extends RasterBuilder[Short, Byte]{
    def isNodata(data:Short) = RasterUtils.isNodata(data)
    def nodata = byteNodata
    def map(r: Raster[Short], f:Short => Byte) = r match {
      case r:ConstantRaster[Short] => Raster(f(r.const))
      case r:Raster[Short] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object S2S extends RasterBuilder[Short, Short]{
    def isNodata(data:Short) = RasterUtils.isNodata(data)
    def nodata = shortNodata
    def map(r: Raster[Short], f:Short => Short) = r match {
      case r:ConstantRaster[Short] => Raster(f(r.const))
      case r:Raster[Short] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object S2I extends RasterBuilder[Short, Int]{
    def isNodata(data:Short) = RasterUtils.isNodata(data)
    def nodata = intNodata
    def map(r: Raster[Short], f:Short => Int) = r match {
      case r:ConstantRaster[Short] => Raster(f(r.const))
      case r:Raster[Short] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object S2F extends RasterBuilder[Short, Float]{
    def isNodata(data:Short) = RasterUtils.isNodata(data)
    def nodata = floatNodata
    def map(r: Raster[Short], f:Short => Float) = r match {
      case r:ConstantRaster[Short] => Raster(f(r.const))
      case r:Raster[Short] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object S2D extends RasterBuilder[Short, Double]{
    def isNodata(data:Short) = RasterUtils.isNodata(data)
    def nodata = doubleNodata
    def map(r: Raster[Short], f:Short => Double) = r match {
      case r:ConstantRaster[Short] => Raster(f(r.const))
      case r:Raster[Short] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object I2B extends RasterBuilder[Int, Byte]{
    def isNodata(data:Int) = RasterUtils.isNodata(data)
    def nodata = byteNodata
    def map(r: Raster[Int], f:Int =>  Byte) = r match {
      case r:ConstantRaster[Int] => Raster(f(r.const))
      case r:Raster[Int] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object I2S extends RasterBuilder[Int, Short]{
    def isNodata(data:Int) = RasterUtils.isNodata(data)
    def nodata = shortNodata
    def map(r: Raster[Int], f:Int =>  Short) = r match {
      case r:ConstantRaster[Int] => Raster(f(r.const))
      case r:Raster[Int] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object I2I extends RasterBuilder[Int, Int]{
    def isNodata(data:Int) = RasterUtils.isNodata(data)
    def nodata = intNodata
    def map(r: Raster[Int], f:Int =>  Int) = r match {
      case r:ConstantRaster[Int] => Raster(f(r.const))
      case r:Raster[Int] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object I2F extends RasterBuilder[Int, Float]{
    def isNodata(data:Int) = RasterUtils.isNodata(data)
    def nodata = floatNodata
    def map(r: Raster[Int], f:Int =>  Float) = r match {
      case r:ConstantRaster[Int] => Raster(f(r.const))
      case r:Raster[Int] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object I2D extends RasterBuilder[Int, Double]{
    def isNodata(data:Int) = RasterUtils.isNodata(data)
    def nodata = doubleNodata
    def map(r: Raster[Int], f:Int =>  Double) = r match {
      case r:ConstantRaster[Int] => Raster(f(r.const))
      case r:Raster[Int] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object F2B extends RasterBuilder[Float, Byte]{
    def isNodata(data:Float) = RasterUtils.isNodata(data)
    def nodata = byteNodata
    def map(r: Raster[Float], f:Float => Byte) = r match {
      case r:ConstantRaster[Float] => Raster(f(r.const))
      case r:Raster[Float] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object F2S extends RasterBuilder[Float, Short]{
    def isNodata(data:Float) = RasterUtils.isNodata(data)
    def nodata = shortNodata
    def map(r: Raster[Float], f:Float => Short) = r match {
      case r:ConstantRaster[Float] => Raster(f(r.const))
      case r:Raster[Float] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object F2I extends RasterBuilder[Float, Int]{
    def isNodata(data:Float) = RasterUtils.isNodata(data)
    def nodata = intNodata
    def map(r: Raster[Float], f:Float => Int) = r match {
      case r:ConstantRaster[Float] => Raster(f(r.const))
      case r:Raster[Float] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object F2F extends RasterBuilder[Float, Float]{
    def isNodata(data:Float) = RasterUtils.isNodata(data)
    def nodata = floatNodata
    def map(r: Raster[Float], f:Float => Float) = r match {
      case r:ConstantRaster[Float] => Raster(f(r.const))
      case r:Raster[Float] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object F2D extends RasterBuilder[Float, Double]{
    def isNodata(data:Float) = RasterUtils.isNodata(data)
    def nodata = byteNodata
    def map(r: Raster[Float], f:Float => Double) = r match {
      case r:ConstantRaster[Float] => Raster(f(r.const))
      case r:Raster[Float] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object D2B extends RasterBuilder[Double, Byte]{
    def isNodata(data:Double) = RasterUtils.isNodata(data)
    def nodata = byteNodata
    def map(r: Raster[Double], f:Double => Byte) = r match {
      case r:ConstantRaster[Double] => Raster(f(r.const))
      case r:Raster[Double] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object D2S extends RasterBuilder[Double, Short]{
    def isNodata(data:Double) = RasterUtils.isNodata(data)
    def nodata = shortNodata
    def map(r: Raster[Double], f:Double => Short) = r match {
      case r:ConstantRaster[Double] => Raster(f(r.const))
      case r:Raster[Double] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object D2I extends RasterBuilder[Double, Int]{
    def isNodata(data:Double) = RasterUtils.isNodata(data)
    def nodata = intNodata
    def map(r: Raster[Double], f:Double => Int) = r match {
      case r:ConstantRaster[Double] => Raster(f(r.const))
      case r:Raster[Double] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object D2F extends RasterBuilder[Double, Float]{
    def isNodata(data:Double) = RasterUtils.isNodata(data)
    def nodata = floatNodata
    def map(r: Raster[Double], f:Double => Float) = r match {
      case r:ConstantRaster[Double] => Raster(f(r.const))
      case r:Raster[Double] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object D2D extends RasterBuilder[Double, Double]{
    def isNodata(data:Double) = RasterUtils.isNodata(data)
    def nodata = doubleNodata
    def map(r: Raster[Double], f:Double => Double) = r match {
      case r:ConstantRaster[Double] => Raster(f(r.const))
      case r:Raster[Double] => Raster(r.data.map(d => if(isNodata(d)) nodata else f(d)), r.cols, r.rows)
    }
  }
}

