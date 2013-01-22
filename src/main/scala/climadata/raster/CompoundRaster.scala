package climadata.raster

class CompoundRaster[T](val raster:Raster[T]*) {
  def apply(i:Int):Raster[T] = raster(i)

  def apply(i:Int, row:Int, col:Int):T = raster(i)(row, col)
}
