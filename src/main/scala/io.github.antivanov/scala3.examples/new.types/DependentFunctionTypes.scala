package io.github.antivanov.scala3.examples.new_types

trait Generator {
  type ValueType
  def next(): ValueType
}

val generators: Map[String, Generator] = Map(
  "field1" -> new Generator:
    type ValueType = String
    def next() = "value1",
  "field2" -> new Generator:
    type ValueType = Int
    def next() = 2
)

def repeatGenerator[T](times: Int)(generator: Generator): List[Generator#ValueType] =
  (1 to times).toList.map(_ => generator.next())

@main def dependentFunctionTypes: Unit =
  val generator: Generator = generators("field1")
  // Specifying the function type of listGenerator is only possible in Scala 3
  val listGenerator: Generator => List[Generator#ValueType] = repeatGenerator(5)
  val values = listGenerator(generator)

  println(values)

