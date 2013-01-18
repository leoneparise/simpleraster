package climadata

import scala.language.implicitConversions
import scala.reflect.ClassTag

package object raster extends RasterBuilders with ResultTypes with Operations {
  type DI = DummyImplicit

  implicit def byte2Raster(const:Byte):Raster[Byte] = Raster(const)
  implicit def short2Raster(const:Short):Raster[Short] = Raster(const)
  implicit def intRaster(const:Int):Raster[Int] = Raster(const)
  implicit def float2Raster(const:Float):Raster[Float] = Raster(const)
  implicit def double2Raster(const:Double):Raster[Double] = Raster(const)
}