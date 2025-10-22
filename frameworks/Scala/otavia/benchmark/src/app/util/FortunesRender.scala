package app.util

import app.model.Fortune
import cc.otavia.buffer.{Buffer, BufferUtils}
import cc.otavia.serde.Serde

import java.nio.charset.StandardCharsets
import scala.annotation.switch

class FortunesRender extends Serde[Array[Fortune]] {

    private val text1 =
        "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"
            .getBytes(StandardCharsets.UTF_8)

    private val text2 = "<tr><td>".getBytes(StandardCharsets.UTF_8)

    private val text3 = "</td><td>".getBytes(StandardCharsets.UTF_8)

    private val text4 = "</td></tr>".getBytes(StandardCharsets.UTF_8)

    private val text5 = "</table></body></html>".getBytes(StandardCharsets.UTF_8)

    private val lt    = "&lt;".getBytes()
    private val gt    = "&gt;".getBytes()
    private val quot  = "&quot;".getBytes()
    private val squot = "&#39;".getBytes()
    private val amp   = "&amp;".getBytes()

    override def serialize(fortunes: Array[Fortune], out: Buffer): Unit = {
        out.writeBytes(text1)
        for (fortune <- fortunes) {
            out.writeBytes(text2)
            BufferUtils.writeIntAsString(out, fortune.id)
            out.writeBytes(text3)
            writeEscapeMessage(out, fortune.message)
            out.writeBytes(text4)
        }
        out.writeBytes(text5)
    }

    override def deserialize(in: Buffer): Array[Fortune] = throw new UnsupportedOperationException()

    private def writeEscapeMessage(buffer: Buffer, message: String): Unit = {
        var i = 0
        while (i < message.length) {
            val ch = message.charAt(i)
            writeChar(buffer, ch)
            i += 1
        }
    }

    private def writeChar(buffer: Buffer, ch: Char): Unit = (ch: @switch) match
        case '<'  => buffer.writeBytes(lt)
        case '>'  => buffer.writeBytes(gt)
        case '"'  => buffer.writeBytes(quot)
        case '\'' => buffer.writeBytes(squot)
        case '&'  => buffer.writeBytes(amp)
        case _ =>
            if (ch < 0x80) buffer.writeByte(ch.toByte)
            else if (ch < 0x800) buffer.writeShortLE((ch >> 6 | (ch << 8 & 0x3f00) | 0x80c0).toShort)
            else buffer.writeMediumLE(ch >> 12 | (ch << 2 & 0x3f00) | (ch << 16 & 0x3f0000) | 0x8080e0)

}
