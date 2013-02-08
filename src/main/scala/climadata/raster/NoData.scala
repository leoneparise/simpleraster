package climadata.raster

trait NoData[@specialized T] {
  def isNodata(d:T):Boolean
  val nodata:T
}

trait NoDatas {
  implicit object byteNodata extends NoData[Byte] {
    def isNodata(d: Byte): Boolean = d == nodata
    val nodata:Byte = Byte.MinValue
  }
  implicit object shortNodata extends NoData[Short] {
    def isNodata(d: Short): Boolean = d == nodata
    val nodata:Short = Short.MinValue
  }

  implicit object intNodata extends NoData[Int] {
    def isNodata(d: Int): Boolean = d == nodata
    val nodata:Int = Int.MinValue
  }

  implicit object floatNodata extends NoData[Float] {
    def isNodata(d: Float): Boolean = d.isNaN
    val nodata:Float = Float.NaN
  }

  implicit object doubleNodata extends NoData[Double] {
    def isNodata(d: Double): Boolean = d.isNaN
    val nodata:Double = Float.NaN
  }
}
