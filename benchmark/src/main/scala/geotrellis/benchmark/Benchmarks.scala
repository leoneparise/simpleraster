package benchmark

import geotrellis._
import geotrellis.raster.op._
import geotrellis.process._
import geotrellis.statistics._

import com.google.caliper.Benchmark
import com.google.caliper.Runner 
import com.google.caliper.SimpleBenchmark

import scala.util.Random

import climadata.{raster => climadata}

/**
 * Extend this to create a main object which will run 'cls' (a benchmark).
 */
abstract class BenchmarkRunner(cls:java.lang.Class[_ <: Benchmark]) {
  def main(args:Array[String]): Unit = Runner.main(cls, args:_*)
}

abstract class CompareBenchmark extends SimpleBenchmark {
  var size:Int = 2048

  var server:Server = null

  var byteGeotrellisRaster:Raster = null
  var shortGeotrellisRaster:Raster = null
  var intGeotrellisRaster:Raster = null
  var floatGeotrellisRaster:Raster = null
  var doubleGeotrellisRaster:Raster = null

  var byteClimadataRaster:climadata.Raster[Byte] = null
  var shortClimadataRaster:climadata.Raster[Short] = null
  var intClimadataRaster:climadata.Raster[Int] = null
  var floatClimadataRaster:climadata.Raster[Float] = null
  var doubleClimadataRaster:climadata.Raster[Double] = null

  /**
   * Sugar for building arrays using a per-cell init function.
   */
  def init[A:Manifest](size:Int)(init: => A) = {
    val data = Array.ofDim[A](size)
    for (i <- 0 until size) data(i) = init
    data
  }

  /**
   * Sugar to run 'f' for 'reps' number of times.
   */
  def run(reps:Int)(f: => Unit) = {
    var i = 0
    while (i < reps) { f; i += 1 }
  }

  override def setUp() {
    server = Server("demo", Catalog.empty("test"))
    val len = size * size
    val re = RasterExtent(Extent(0, 0, size, size), 1.0, 1.0, size, size)
    byteGeotrellisRaster = Raster(ByteArrayRasterData(init(len)(Random.nextInt.toByte),size,size), re)
    shortGeotrellisRaster = Raster(ShortArrayRasterData(init(len)(Random.nextInt.toShort),size,size), re)
    intGeotrellisRaster = Raster(IntArrayRasterData(init(len)(Random.nextInt),size,size), re)
    floatGeotrellisRaster = Raster(FloatArrayRasterData(init(len)(Random.nextInt.toFloat),size,size), re)
    doubleGeotrellisRaster = Raster(DoubleArrayRasterData(init(len)(Random.nextInt.toDouble),size,size), re)

    byteClimadataRaster = climadata.Raster(init(len)(Random.nextInt.toByte), size, size)
    shortClimadataRaster = climadata.Raster(init(len)(Random.nextInt.toShort), size, size)
    intClimadataRaster = climadata.Raster(init(len)(Random.nextInt), size, size)
    floatClimadataRaster = climadata.Raster(init(len)(Random.nextInt.toFloat), size, size)
    doubleClimadataRaster = climadata.Raster(init(len)(Random.nextInt.toDouble), size, size)
  }
}

object WhileLoopBenchmark extends BenchmarkRunner(classOf[WhileLoopBenchmark])
class WhileLoopBenchmark extends CompareBenchmark {
  def timeGeotrellisByteRasterWhileLoop(reps:Int) = run(reps)(geotrellisByteRasterWhileLoop)
  def geotrellisByteRasterWhileLoop = {
    val r = byteGeotrellisRaster
    val rows = r.rows
    val cols = r.cols
    var row = 0
    var a = 0
    while(row < rows) {
      var col = 0
      while(col < cols) {
        val z = r.get(col,row)
        if (z != NODATA) a = r.get(col,row) * 2
        col += 1
      }
      row += 1
    }
  }

  def timeClimadataByteRasterWhileLoop(reps:Int) = run(reps)(climadataByteRasterWhileLoop)
  def climadataByteRasterWhileLoop = {
    val r = byteClimadataRaster
    val rows = r.rows
    val cols = r.cols
    var row = 0
    var a = 0
    while(row < rows) {
      var col = 0
      while(col < cols) {
        val z = r(col,row)
        if (z != NODATA) a = r(col,row) * 2
        col += 1
      }
      row += 1
    }
  }

  def timeGeotrellisShortRasterWhileLoop(reps:Int) = run(reps)(geotrellisShortRasterWhileLoop)
  def geotrellisShortRasterWhileLoop = {
    val r = shortGeotrellisRaster
    val rows = r.rows
    val cols = r.cols
    var row = 0
    var a = 0
    while(row < rows) {
      var col = 0
      while(col < cols) {
        val z = r.get(col,row)
        if (z != NODATA) a = r.get(col,row) * 2
        col += 1
      }
      row += 1
    }
  }

  def timeClimadataShortRasterWhileLoop(reps:Int) = run(reps)(climadataShortRasterWhileLoop)
  def climadataShortRasterWhileLoop = {
    val r = shortClimadataRaster
    val rows = r.rows
    val cols = r.cols
    var row = 0
    var a = 0
    while(row < rows) {
      var col = 0
      while(col < cols) {
        val z = r(col,row)
        if (z != NODATA) a = r(col,row) * 2
        col += 1
      }
      row += 1
    }
  }

  def timeGeotrellisIntRasterWhileLoop(reps:Int) = run(reps)(geotrellisIntRasterWhileLoop)
  def geotrellisIntRasterWhileLoop = {
    val r = intGeotrellisRaster
    val rows = r.rows
    val cols = r.cols
    var row = 0
    var a = 0
    while(row < rows) {
      var col = 0
      while(col < cols) {
        val z = r.get(col,row)
        if (z != NODATA) a = r.get(col,row) * 2
        col += 1
      }
      row += 1
    }
  }

  def timeClimadataIntRasterWhileLoop(reps:Int) = run(reps)(climadataIntRasterWhileLoop)
  def climadataIntRasterWhileLoop = {
    val r = intClimadataRaster
    val rows = r.rows
    val cols = r.cols
    var row = 0
    var a = 0
    while(row < rows) {
      var col = 0
      while(col < cols) {
        val z = r(col,row)
        if (z != NODATA) a = r(col,row) * 2
        col += 1
      }
      row += 1
    }
  }

  def timeGeotrellisFloatRasterWhileLoop(reps:Int) = run(reps)(geotrellisFloatRasterWhileLoop)
  def geotrellisFloatRasterWhileLoop = {
    val r = floatGeotrellisRaster
    val rows = r.rows
    val cols = r.cols
    var row = 0
    var a = 0.0
    while(row < rows) {
      var col = 0
      while(col < cols) {
        val z = r.getDouble(col,row)
        if (z != NODATA) a = r.getDouble(col,row) * 2
        col += 1
      }
      row += 1
    }
  }

  def timeClimadataFloatRasterWhileLoop(reps:Int) = run(reps)(climadataFloatRasterWhileLoop)
  def climadataFloatRasterWhileLoop = {
    val r = floatClimadataRaster
    val rows = r.rows
    val cols = r.cols
    var row = 0
    var a = 0.0
    while(row < rows) {
      var col = 0
      while(col < cols) {
        val z = r(col,row)
        if (z != NODATA) a = r(col,row) * 2
        col += 1
      }
      row += 1
    }
  }

  def timeGeotrellisDoubleRasterWhileLoop(reps:Int) = run(reps)(geotrellisDoubleRasterWhileLoop)
  def geotrellisDoubleRasterWhileLoop = {
    val r = doubleGeotrellisRaster
    val rows = r.rows
    val cols = r.cols
    var row = 0
    var a = 0.0
    while(row < rows) {
      var col = 0
      while(col < cols) {
        val z = r.getDouble(col,row)
        if (z != NODATA) a = r.getDouble(col,row) * 2
        col += 1
      }
      row += 1
    }
  }

  def timeClimadataDoubleRasterWhileLoop(reps:Int) = run(reps)(climadataDoubleRasterWhileLoop)
  def climadataDoubleRasterWhileLoop = {
    val r = doubleClimadataRaster
    val rows = r.rows
    val cols = r.cols
    var row = 0
    var a = 0.0
    while(row < rows) {
      var col = 0
      while(col < cols) {
        val z = r(col,row)
        if (z != NODATA) a = r(col,row) * 2
        col += 1
      }
      row += 1
    }
  }
}

object AddBenchmark extends BenchmarkRunner(classOf[AddBenchmark])
class AddBenchmark extends CompareBenchmark {
  def timeGeotrellisIntRasterAdd(reps:Int) = run(reps)(geotrellisIntRasterAdd)
  def geotrellisIntRasterAdd = {
    val r = intGeotrellisRaster

    server.run(local.Add(r,r))
  }

  def timeClimadataIntRasterAdd(reps:Int) = run(reps)(climadataIntRasterAdd)
  def climadataIntRasterAdd = {
    val r = intClimadataRaster
    r + r
  }

  def timeGeotrellisDoubleRasterAdd(reps:Int) = run(reps)(geotrellisDoubleRasterAdd)
  def geotrellisDoubleRasterAdd = {
    val r = doubleGeotrellisRaster

    server.run(local.Add(r,r))
  }

  def timeClimadataDoubleRasterAdd(reps:Int) = run(reps)(climadataDoubleRasterAdd)
  def climadataDoubleRasterAdd = {
    val r = doubleClimadataRaster

    r + r
  }  
}

object MapBenchmark extends BenchmarkRunner(classOf[MapBenchmark])
class MapBenchmark extends CompareBenchmark {
  def timeGeotrellisIntRasterMap(reps:Int) = run(reps)(geotrellisIntRasterMap)
  def geotrellisIntRasterMap = {
    val r = intGeotrellisRaster

    r map (_ + 1)
  }

  def timeClimadataIntRasterMap(reps:Int) = run(reps)(climadataIntRasterMap)
  def climadataIntRasterMap = {
    val r = intClimadataRaster
    r map (_ + 1)
  }

  def timeGeotrellisDoubleRasterMap(reps:Int) = run(reps)(geotrellisDoubleRasterMap)
  def geotrellisDoubleRasterMap = {
    val r = doubleGeotrellisRaster

    r mapDouble (_ + 1.0)
  }

  def timeClimadataDoubleRasterMap(reps:Int) = run(reps)(climadataDoubleRasterMap)
  def climadataDoubleRasterMap = {
    val r = doubleClimadataRaster

    r map (_ + 1.0)
  }  
}
