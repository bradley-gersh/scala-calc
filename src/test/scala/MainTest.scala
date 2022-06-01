import org.scalatest.flatspec.AnyFlatSpec
import java.io.ByteArrayOutputStream

import it.bitrock.scalalearn.Main

class FirstSpec extends AnyFlatSpec {

  "Main" should "print 'Hello, World!'" in {
    val stream = new ByteArrayOutputStream()
    Console.withOut(stream) {
      Main.main(Array())
    }
    assert(stream.toString == "Hello, World!\n")
  }

}
