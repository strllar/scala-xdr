package org.strllar.scalaxdr.xdrbase

import scodec.bits._
import scodec.{Codec, Encoder, DecodeResult, Attempt, Err, SizeBound, codecs}

object XDRCodecs {
  //known issue: xdr use Unsigned Int as array length but scala support only Int as Seq length
  private[this] val MaxUnsignedInt = (1L << 32)-1
  private[this] def XDRVariableLength[T](maxlen :Option[Long],  lencodec :(Long) => Codec[Vector[T]]) :Codec[Vector[T]] = {
    val limit = maxlen.getOrElse(MaxUnsignedInt)
    codecs.uint32.narrow[Long](
      (x) => {
        if (x > limit) Attempt.failure(Err("exceed limit"))
        else Attempt.successful(x)
      },
      identity
    ).flatZip( (len) =>
      lencodec(len)
    ).widen[Vector[T]](
      _._2,
      (x:Vector[T]) => {
        if (x.length > limit) Attempt.failure(Err("exceed limit"))
        else Attempt.successful((x.length.toLong, x))
      }
    )
  }
  private[this] val XDRByteZero = codecs.byte.unit(0)
  val XDRVoid :Codec[Unit] = codecs.provide()

  val XDRInteger :Codec[Int] = codecs.int32.withToString("XDRInteger")
  val XDRUnsignedInteger :Codec[Long] = codecs.uint32.withToString("XDRUnsignedInteger")
  val XDREnumeration = XDRInteger.withToString("XDREnumeration")

  //todo: enumeration equivalent?
  val XDRBoolean :Codec[Boolean] = codecs.int32.narrow[Boolean](
    (x)=> {
      if (x == 0) Attempt.successful(false)
      else if (x==1) Attempt.successful(true)
      else Attempt.failure[Boolean](Err("not xdr boolean"))
    } ,
    (x) => {if (x) 1 else 0}
  ).withToString("XDRBoolean")

  val XDRHyper :Codec[Long] = codecs.int64.withToString("XDRHyper")

  //val XDRUnsignedHyper :Codec[BigInt] =

  val XDRFloat :Codec[Float] = codecs.float.withToString("XDRFloat")

  val XDRDouble :Codec[Double] = codecs.double.withToString("XDRDouble")

  //val XDRQuadruple :Codec[BigDecimal] =

  def XDRFixedLengthOpaque(len :Long) :Codec[Vector[Byte]] = {
    assert(len < Int.MaxValue && len > 0)

    val paddedLen = (len + 3) & ~3
    codecs.paddedFixedSizeBytes(
      paddedLen,
      codecs.bytes(len.toInt).xmap(_.toIndexedSeq.toVector, ByteVector(_:Vector[Byte])),
      XDRByteZero).withToString(s"XDROpaque[$len]")
  }

  def XDRVariableLengthOpaque(maxlen :Option[Long]) :Codec[Vector[Byte]] =
    XDRVariableLength(maxlen, XDRFixedLengthOpaque).withToString(s"XDROpaque<${maxlen.getOrElse("")}>")

  def XDRString(maxlen :Option[Long]) :Codec[String] = XDRVariableLengthOpaque(maxlen).xmap(
    (v :Vector[Byte]) => new String(v.toArray),
    (s :String) => s.getBytes.toVector
  ).withToString(s"XDRString<$maxlen>")

  def XDROptional[T](codec :Codec[T]) :Codec[Option[T]] = codecs.optional(XDRBoolean, codec)

  def XDRFixedLengthArray[T](codec :Codec[T])(len :Long) :Codec[Vector[T]] =
    codecs.vectorOfN(
      codecs.uint32.narrow[Int](
        (x) => if (x > Int.MaxValue) Attempt.failure(Err("unsupported array length than Int.MaxValue")) else Attempt.successful(x.toInt),
        _ + 0L
      ),
      codec
    ).withToString(s"XDRArray $codec[$len]")

  def XDRVariableLengthArray[T](maxlen :Option[Long],  codec :Codec[T]) :Codec[Vector[T]] =
    XDRVariableLength(maxlen, XDRFixedLengthArray(codec)).withToString(s"XDRArray $codec<${maxlen.getOrElse("")}>")

}