package mytests

import com.inthenow.zcheck.SpecLite

import org.strllar.scalaxdr.xdrbase.{XDRCodec, annotations}, annotations._

@xdrfile
object somexdrfile {
  case class abc()
}

object XDRCodecSpec extends SpecLite {
  import org.strllar.scalaxdr.xdrbase.XDRCodec
  "macro should work" in {
    XDRCodec
    1+1 must_== 2
  }
}