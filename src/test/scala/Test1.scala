
import org.junit.Test
import org.junit.Assert._

class Test1 {
  @Test def t1(): Unit = {
    val msg = ""
    assertEquals("I was compiled by Scala 3. :)", msg)
  }
}