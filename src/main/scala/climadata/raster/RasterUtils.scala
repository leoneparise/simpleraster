package climadata.raster

import scala.reflect.Manifest

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
