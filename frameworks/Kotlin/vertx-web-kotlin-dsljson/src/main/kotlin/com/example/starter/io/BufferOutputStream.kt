package com.example.starter.io

import io.netty.buffer.ByteBuf
import io.vertx.core.buffer.Buffer
import io.vertx.core.json.JsonArray
import io.vertx.core.json.JsonObject
import java.io.IOException
import java.io.OutputStream
import java.nio.ByteBuffer
import java.nio.charset.Charset

class BufferOutputStream(initialSizeHint: Int = 0) : Buffer, OutputStream() {
    private val buffer: Buffer = Buffer.buffer(initialSizeHint)

    @Throws(IOException::class)
    override fun write(b: Int) {
        buffer.appendByte(b.toByte())
    }

    @Throws(IOException::class)
    override fun write(bytes: ByteArray) {
        buffer.appendBytes(bytes)
    }

    @Throws(IOException::class)
    override fun write(bytes: ByteArray, offset: Int, len: Int) {
        buffer.appendBytes(bytes, offset, len)
    }

    override fun toString(enc: String?): String {
        return buffer.toString(enc)
    }

    override fun toString(enc: Charset?): String {
        return buffer.toString(enc)
    }

    override fun writeToBuffer(other: Buffer?) {
        buffer.writeToBuffer(other)
    }

    override fun readFromBuffer(pos: Int, other: Buffer?): Int {
        return buffer.readFromBuffer(pos, other)
    }

    override fun copy(): Buffer {
        return buffer.copy()
    }

    override fun toJsonObject(): JsonObject {
        return buffer.toJsonObject()
    }

    override fun toJsonArray(): JsonArray {
        return buffer.toJsonArray()
    }

    override fun getByte(pos: Int): Byte {
        return buffer.getByte(pos)
    }

    override fun getUnsignedByte(pos: Int): Short {
        return buffer.getUnsignedByte(pos)
    }

    override fun getInt(pos: Int): Int {
        return buffer.getInt(pos)
    }

    override fun getIntLE(pos: Int): Int {
        return buffer.getIntLE(pos)
    }

    override fun getUnsignedInt(pos: Int): Long {
        return buffer.getUnsignedInt(pos)
    }

    override fun getUnsignedIntLE(pos: Int): Long {
        return buffer.getUnsignedIntLE(pos)
    }

    override fun getLong(pos: Int): Long {
        return buffer.getLong(pos)
    }

    override fun getLongLE(pos: Int): Long {
        return buffer.getLongLE(pos)
    }

    override fun getDouble(pos: Int): Double {
        return buffer.getDouble(pos)
    }

    override fun getFloat(pos: Int): Float {
        return buffer.getFloat(pos)
    }

    override fun getShort(pos: Int): Short {
        return buffer.getShort(pos)
    }

    override fun getShortLE(pos: Int): Short {
        return buffer.getShortLE(pos)
    }

    override fun getUnsignedShort(pos: Int): Int {
        return buffer.getUnsignedShort(pos)
    }

    override fun getUnsignedShortLE(pos: Int): Int {
        return buffer.getUnsignedShortLE(pos)
    }

    override fun getMedium(pos: Int): Int {
        return buffer.getMedium(pos)
    }

    override fun getMediumLE(pos: Int): Int {
        return buffer.getMediumLE(pos)
    }

    override fun getUnsignedMedium(pos: Int): Int {
        return buffer.getUnsignedMedium(pos)
    }

    override fun getUnsignedMediumLE(pos: Int): Int {
        return buffer.getUnsignedMediumLE(pos)
    }

    override fun getBytes(): ByteArray {
        return buffer.bytes
    }

    override fun getBytes(start: Int, end: Int): ByteArray {
        return buffer.getBytes(start, end)
    }

    override fun getBytes(dst: ByteArray?): Buffer {
        return buffer.getBytes(dst)
    }

    override fun getBytes(dst: ByteArray?, dstIndex: Int): Buffer {
        return buffer.getBytes(dst, dstIndex)
    }

    override fun getBytes(start: Int, end: Int, dst: ByteArray?): Buffer {
        return buffer.getBytes(start, end, dst)
    }

    override fun getBytes(start: Int, end: Int, dst: ByteArray?, dstIndex: Int): Buffer {
        return buffer.getBytes(start, end, dst, dstIndex)
    }

    override fun getBuffer(start: Int, end: Int): Buffer {
        return buffer.getBuffer(start, end)
    }

    override fun getString(start: Int, end: Int, enc: String?): String {
        return buffer.getString(start, end, enc)
    }

    override fun getString(start: Int, end: Int): String {
        return buffer.getString(start, end)
    }

    override fun appendBuffer(buff: Buffer?): Buffer {
        return buffer.appendBuffer(buff)
    }

    override fun appendBuffer(buff: Buffer?, offset: Int, len: Int): Buffer {
        return buffer.appendBuffer(buff, offset, len)
    }

    override fun appendBytes(bytes: ByteArray?): Buffer {
        return buffer.appendBytes(bytes)
    }

    override fun appendBytes(bytes: ByteArray?, offset: Int, len: Int): Buffer {
        return buffer.appendBytes(bytes, offset, len)
    }

    override fun appendByte(b: Byte): Buffer {
        return buffer.appendByte(b)
    }

    override fun appendUnsignedByte(b: Short): Buffer {
        return buffer.appendUnsignedByte(b)
    }

    override fun appendInt(i: Int): Buffer {
        return buffer.appendInt(i)
    }

    override fun appendIntLE(i: Int): Buffer {
        return buffer.appendIntLE(i)
    }

    override fun appendUnsignedInt(i: Long): Buffer {
        return buffer.appendUnsignedInt(i)
    }

    override fun appendUnsignedIntLE(i: Long): Buffer {
        return buffer.appendUnsignedIntLE(i)
    }

    override fun appendMedium(i: Int): Buffer {
        return buffer.appendMedium(i)
    }

    override fun appendMediumLE(i: Int): Buffer {
        return buffer.appendMediumLE(i)
    }

    override fun appendLong(l: Long): Buffer {
        return buffer.appendLong(l)
    }

    override fun appendLongLE(l: Long): Buffer {
        return buffer.appendLongLE(l)
    }

    override fun appendShort(s: Short): Buffer {
        return buffer.appendShort(s)
    }

    override fun appendShortLE(s: Short): Buffer {
        return buffer.appendShortLE(s)
    }

    override fun appendUnsignedShort(s: Int): Buffer {
        return buffer.appendUnsignedShort(s)
    }

    override fun appendUnsignedShortLE(s: Int): Buffer {
        return buffer.appendUnsignedShortLE(s)
    }

    override fun appendFloat(f: Float): Buffer {
        return buffer.appendFloat(f)
    }

    override fun appendDouble(d: Double): Buffer {
        return buffer.appendDouble(d)
    }

    override fun appendString(str: String?, enc: String?): Buffer {
        return buffer.appendString(str, enc)
    }

    override fun appendString(str: String?): Buffer {
        return buffer.appendString(str)
    }

    override fun setByte(pos: Int, b: Byte): Buffer {
        return buffer.setByte(pos, b)
    }

    override fun setUnsignedByte(pos: Int, b: Short): Buffer {
        return buffer.setUnsignedByte(pos, b)
    }

    override fun setInt(pos: Int, i: Int): Buffer {
        return buffer.setInt(pos, i)
    }

    override fun setIntLE(pos: Int, i: Int): Buffer {
        return buffer.setIntLE(pos, i)
    }

    override fun setUnsignedInt(pos: Int, i: Long): Buffer {
        return buffer.setUnsignedInt(pos, i)
    }

    override fun setUnsignedIntLE(pos: Int, i: Long): Buffer {
        return buffer.setUnsignedIntLE(pos, i)
    }

    override fun setMedium(pos: Int, i: Int): Buffer {
        return buffer.setMedium(pos, i)
    }

    override fun setMediumLE(pos: Int, i: Int): Buffer {
        return buffer.setMediumLE(pos, i)
    }

    override fun setLong(pos: Int, l: Long): Buffer {
        return buffer.setLong(pos, l)
    }

    override fun setLongLE(pos: Int, l: Long): Buffer {
        return buffer.setLongLE(pos, l)
    }

    override fun setDouble(pos: Int, d: Double): Buffer {
        return buffer.setDouble(pos, d)
    }

    override fun setFloat(pos: Int, f: Float): Buffer {
        return buffer.setFloat(pos, f)
    }

    override fun setShort(pos: Int, s: Short): Buffer {
        return buffer.setShort(pos, s)
    }

    override fun setShortLE(pos: Int, s: Short): Buffer {
        return buffer.setShortLE(pos, s)
    }

    override fun setUnsignedShort(pos: Int, s: Int): Buffer {
        return buffer.setUnsignedShort(pos, s)
    }

    override fun setUnsignedShortLE(pos: Int, s: Int): Buffer {
        return buffer.setUnsignedShortLE(pos, s)
    }

    override fun setBuffer(pos: Int, b: Buffer?): Buffer {
        return buffer.setBuffer(pos, b)
    }

    override fun setBuffer(pos: Int, b: Buffer?, offset: Int, len: Int): Buffer {
        return buffer.setBuffer(pos, b, offset, len)
    }

    override fun setBytes(pos: Int, b: ByteBuffer?): Buffer {
        return buffer.setBytes(pos, b)
    }

    override fun setBytes(pos: Int, b: ByteArray?): Buffer {
        return buffer.setBytes(pos, b)
    }

    override fun setBytes(pos: Int, b: ByteArray?, offset: Int, len: Int): Buffer {
        return buffer.setBytes(pos, b, offset, len)
    }

    override fun setString(pos: Int, str: String?): Buffer {
        return buffer.setString(pos, str)
    }

    override fun setString(pos: Int, str: String?, enc: String?): Buffer {
        return buffer.setString(pos, str, enc)
    }

    override fun length(): Int {
        return buffer.length()
    }

    override fun slice(): Buffer {
        return buffer.slice()
    }

    override fun slice(start: Int, end: Int): Buffer {
        return buffer.slice(start, end)
    }

    @Deprecated("Deprecated in Java")
    override fun getByteBuf(): ByteBuf {
        return buffer.byteBuf
    }
}