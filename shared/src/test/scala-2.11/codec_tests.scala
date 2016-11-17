package mytests

import com.inthenow.zcheck.SpecLite

import org.strllar.scalaxdr.xdrbase.{XDRCodec, annotations}, annotations._

@xdrfile("stellar")
object somexdrfile {
  case class abc()
}


object XDRCodecSpec extends SpecLite {
  import org.strllar.scalaxdr.xdrbase.XDRCodec
  "macro should work" in {
    XDRCodec.checkAnnotations[somexdrfile.type]

    val bs = Array[Byte](0,0,0,1)
    val x = XDRCodec.fromBytes[somexdrfile.abc](bs)
    val y = XDRCodec.toBytes(x).array().toSeq

    println(x, y)
    1+1 must_== 2
  }
}