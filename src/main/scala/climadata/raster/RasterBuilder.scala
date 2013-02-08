package climadata.raster

import scala.language.implicitConversions

trait RasterBuilder[@specialized A, @specialized B] {
  def map(r:Raster[A], f:A => B)(implicit nda:NoData[A], ndb:NoData[B]):Raster[B]
}

trait RasterBuilders {
  implicit object B2B extends RasterBuilder[Byte, Byte]{
    def map(r: Raster[Byte], f:Byte => Byte)(implicit nda:NoData[Byte], ndb:NoData[Byte]) = r match {
      case r:ConstantRaster[Byte] => Raster(f(r.const))
      case r:Raster[Byte] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object B2S extends RasterBuilder[Byte, Short]{
    def map(r: Raster[Byte], f:Byte => Short)(implicit nda:NoData[Byte], ndb:NoData[Short]) = r match {
      case r:ConstantRaster[Byte] => Raster(f(r.const))
      case r:Raster[Byte] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object B2I extends RasterBuilder[Byte, Int]{
    def map(r: Raster[Byte], f:Byte => Int)(implicit nda:NoData[Byte], ndb:NoData[Int]) = r match {
      case r:ConstantRaster[Byte] => Raster(f(r.const))
      case r:Raster[Byte] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object B2F extends RasterBuilder[Byte, Float]{
    def map(r: Raster[Byte], f:Byte => Float)(implicit nda:NoData[Byte], ndb:NoData[Float]) = r match {
      case r:ConstantRaster[Byte] => Raster(f(r.const))
      case r:Raster[Byte] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object B2D extends RasterBuilder[Byte, Double]{
    def map(r: Raster[Byte], f:Byte => Double)(implicit nda:NoData[Byte], ndb:NoData[Double]) = r match {
      case r:ConstantRaster[Byte] => Raster(f(r.const))
      case r:Raster[Byte] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object S2B extends RasterBuilder[Short, Byte]{
    def map(r: Raster[Short], f:Short => Byte)(implicit nda:NoData[Short], ndb:NoData[Byte]) = r match {
      case r:ConstantRaster[Short] => Raster(f(r.const))
      case r:Raster[Short] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object S2S extends RasterBuilder[Short, Short]{
    def map(r: Raster[Short], f:Short => Short)(implicit nda:NoData[Short], ndb:NoData[Short]) = r match {
      case r:ConstantRaster[Short] => Raster(f(r.const))
      case r:Raster[Short] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object S2I extends RasterBuilder[Short, Int]{
    def map(r: Raster[Short], f:Short => Int)(implicit nda:NoData[Short], ndb:NoData[Int]) = r match {
      case r:ConstantRaster[Short] => Raster(f(r.const))
      case r:Raster[Short] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object S2F extends RasterBuilder[Short, Float]{
    def map(r: Raster[Short], f:Short => Float)(implicit nda:NoData[Short], ndb:NoData[Float]) = r match {
      case r:ConstantRaster[Short] => Raster(f(r.const))
      case r:Raster[Short] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object S2D extends RasterBuilder[Short, Double]{
    def map(r: Raster[Short], f:Short => Double)(implicit nda:NoData[Short], ndb:NoData[Double]) = r match {
      case r:ConstantRaster[Short] => Raster(f(r.const))
      case r:Raster[Short] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object I2B extends RasterBuilder[Int, Byte]{
    def map(r: Raster[Int], f:Int =>  Byte)(implicit nda:NoData[Int], ndb:NoData[Byte]) = r match {
      case r:ConstantRaster[Int] => Raster(f(r.const))
      case r:Raster[Int] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object I2S extends RasterBuilder[Int, Short]{
    def map(r: Raster[Int], f:Int =>  Short)(implicit nda:NoData[Int], ndb:NoData[Short]) = r match {
      case r:ConstantRaster[Int] => Raster(f(r.const))
      case r:Raster[Int] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object I2I extends RasterBuilder[Int, Int]{
    def map(r: Raster[Int], f:Int =>  Int)(implicit nda:NoData[Int], ndb:NoData[Int]) = r match {
      case r:ConstantRaster[Int] => Raster(f(r.const))
      case r:Raster[Int] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object I2F extends RasterBuilder[Int, Float]{
    def map(r: Raster[Int], f:Int =>  Float)(implicit nda:NoData[Int], ndb:NoData[Float]) = r match {
      case r:ConstantRaster[Int] => Raster(f(r.const))
      case r:Raster[Int] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object I2D extends RasterBuilder[Int, Double]{
    def map(r: Raster[Int], f:Int =>  Double)(implicit nda:NoData[Int], ndb:NoData[Double]) = r match {
      case r:ConstantRaster[Int] => Raster(f(r.const))
      case r:Raster[Int] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object F2B extends RasterBuilder[Float, Byte]{
    def map(r: Raster[Float], f:Float => Byte)(implicit nda:NoData[Float], ndb:NoData[Byte]) = r match {
      case r:ConstantRaster[Float] => Raster(f(r.const))
      case r:Raster[Float] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object F2S extends RasterBuilder[Float, Short]{
    def map(r: Raster[Float], f:Float => Short)(implicit nda:NoData[Float], ndb:NoData[Short]) = r match {
      case r:ConstantRaster[Float] => Raster(f(r.const))
      case r:Raster[Float] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object F2I extends RasterBuilder[Float, Int]{
    def map(r: Raster[Float], f:Float => Int)(implicit nda:NoData[Float], ndb:NoData[Int]) = r match {
      case r:ConstantRaster[Float] => Raster(f(r.const))
      case r:Raster[Float] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object F2F extends RasterBuilder[Float, Float]{
    def map(r: Raster[Float], f:Float => Float)(implicit nda:NoData[Float], ndb:NoData[Float]) = r match {
      case r:ConstantRaster[Float] => Raster(f(r.const))
      case r:Raster[Float] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object F2D extends RasterBuilder[Float, Double]{
    def map(r: Raster[Float], f:Float => Double)(implicit nda:NoData[Float], ndb:NoData[Double]) = r match {
      case r:ConstantRaster[Float] => Raster(f(r.const))
      case r:Raster[Float] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object D2B extends RasterBuilder[Double, Byte]{
    def map(r: Raster[Double], f:Double => Byte)(implicit nda:NoData[Double], ndb:NoData[Byte]) = r match {
      case r:ConstantRaster[Double] => Raster(f(r.const))
      case r:Raster[Double] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object D2S extends RasterBuilder[Double, Short]{
    def map(r: Raster[Double], f:Double => Short)(implicit nda:NoData[Double], ndb:NoData[Short]) = r match {
      case r:ConstantRaster[Double] => Raster(f(r.const))
      case r:Raster[Double] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object D2I extends RasterBuilder[Double, Int]{
    def map(r: Raster[Double], f:Double => Int)(implicit nda:NoData[Double], ndb:NoData[Int]) = r match {
      case r:ConstantRaster[Double] => Raster(f(r.const))
      case r:Raster[Double] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object D2F extends RasterBuilder[Double, Float]{
    def map(r: Raster[Double], f:Double => Float)(implicit nda:NoData[Double], ndb:NoData[Float]) = r match {
      case r:ConstantRaster[Double] => Raster(f(r.const))
      case r:Raster[Double] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }

  implicit object D2D extends RasterBuilder[Double, Double]{
    def map(r: Raster[Double], f:Double => Double)(implicit nda:NoData[Double], ndb:NoData[Double]) = r match {
      case r:ConstantRaster[Double] => Raster(f(r.const))
      case r:Raster[Double] => Raster(r.data.map(d => if(nda.isNodata(d)) ndb.nodata else f(d)), r.cols, r.rows)
    }
  }
}

