//import java.nio.file.{Files, Paths}
//
//import output.in.xsd.xsd.Schema
//import ru.tinkoff.phobos.decoding.XmlDecoder
//
//object XsdTest {
//  def main(args: Array[String]): Unit = {
//    val bytes = Files.readAllBytes(Paths.get("in/xsd/xsd.xsd"))
//    println(XmlDecoder[Schema].decodeFromBytes(bytes))
//  }
//}
