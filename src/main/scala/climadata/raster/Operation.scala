package climadata.raster

import scala.language.implicitConversions

trait Operation[T] {
  def handle(a:T, b:T):T
}

trait Sum[T] extends Operation[T]

trait Sub[T] extends Operation[T]

trait Multiply[T] extends Operation[T]

trait Divide[T] extends Operation[T]

trait Modulus[T] extends Operation[T]

trait And[T] extends Operation[T]

trait Or[T] extends Operation[T]


trait Operations {
  implicit object SumByte extends Sum[Byte] {
    def handle(a:Byte, b:Byte):Byte = (a + b).toByte
  }

  implicit object SumShort extends Sum[Short] {
    def handle(a:Short, b:Short):Short = (a + b).toShort
  }

  implicit object SumInt extends Sum[Int] {
    def handle(a:Int, b:Int):Int = a + b
  }

  implicit object SumFloat extends Sum[Float] {
    def handle(a:Float, b:Float):Float = (a + b).toFloat
  }

  implicit object SumDouble extends Sum[Double] {
    def handle(a:Double, b:Double):Double = a + b
  }

  implicit object SubByte extends Sub[Byte] {
    def handle(a:Byte, b:Byte):Byte = (a - b).toByte
  }

  implicit object SubShort extends Sub[Short] {
    def handle(a:Short, b:Short):Short = (a - b).toShort
  }

  implicit object SubInt extends Sub[Int] {
    def handle(a:Int, b:Int):Int = a - b
  }

  implicit object SubFloat extends Sub[Float] {
    def handle(a:Float, b:Float):Float = (a - b).toFloat
  }

  implicit object SubDouble extends Sub[Double] {
    def handle(a:Double, b:Double):Double = a - b
  }

  implicit object MultiplyByte extends Multiply[Byte] {
    def handle(a:Byte, b:Byte):Byte = (a * b).toByte
  }

  implicit object MultiplyShort extends Multiply[Short] {
    def handle(a:Short, b:Short):Short = (a * b).toShort
  }

  implicit object MultiplyInt extends Multiply[Int] {
    def handle(a:Int, b:Int):Int = a * b
  }

  implicit object MultiplyFloat extends Multiply[Float] {
    def handle(a:Float, b:Float):Float = (a * b).toFloat
  }

  implicit object MultiplyDouble extends Multiply[Double] {
    def handle(a:Double, b:Double):Double = a * b
  }

  implicit object DivideByte extends Divide[Byte] {
    def handle(a:Byte, b:Byte):Byte = (a / b).toByte
  }

  implicit object DivideShort extends Divide[Short] {
    def handle(a:Short, b:Short):Short = (a / b).toShort
  }

  implicit object DivideInt extends Divide[Int] {
    def handle(a:Int, b:Int):Int = a / b
  }

  implicit object DivideFloat extends Divide[Float] {
    def handle(a:Float, b:Float):Float = (a / b).toFloat
  }

  implicit object DivideDouble extends Divide[Double] {
    def handle(a:Double, b:Double):Double = a / b
  }

  implicit object ModulusByte extends Modulus[Byte] {
    def handle(a:Byte, b:Byte):Byte = (a % b).toByte
  }

  implicit object ModulusShort extends Modulus[Short] {
    def handle(a:Short, b:Short):Short = (a % b).toShort
  }

  implicit object ModulusInt extends Modulus[Int] {
    def handle(a:Int, b:Int):Int = a % b
  }

  implicit object AndByte extends And[Byte] {
    def handle(a:Byte, b:Byte):Byte = (a & b).toByte
  }

  implicit object AndShort extends And[Short] {
    def handle(a:Short, b:Short):Short = (a & b).toShort
  }

  implicit object AndInt extends And[Int] {
    def handle(a:Int, b:Int):Int = a & b
  }

  implicit object OrByte extends Or[Byte] {
    def handle(a:Byte, b:Byte):Byte = (a | b).toByte
  }

  implicit object OrShort extends Or[Short] {
    def handle(a:Short, b:Short):Short = (a | b).toShort
  }

  implicit object OrInt extends Or[Int] {
    def handle(a:Int, b:Int):Int = a | b
  }
}
