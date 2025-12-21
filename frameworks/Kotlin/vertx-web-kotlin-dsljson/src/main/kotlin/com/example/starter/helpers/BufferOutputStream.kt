package com.example.starter.helpers

import io.netty.buffer.ByteBuf
import io.vertx.core.buffer.Buffer
import io.vertx.core.internal.buffer.BufferInternal
import io.vertx.core.json.JsonArray
import io.vertx.core.json.JsonObject
import java.io.IOException
import java.io.OutputStream
import java.nio.ByteBuffer
import java.nio.charset.Charset

class BufferOutputStream(initialSizeHint: Int = 256) : BufferInternal, OutputStream() {
    private val buffer: BufferInternal = BufferInternal.buffer(initialSizeHint)

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

    override fun toString(enc: String): String {
        return buffer.toString(enc)
    }

    override fun toString(enc: Charset): String {
        return buffer.toString(enc)
    }

    override fun copy(): BufferInternal {
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

    override fun getDoubleLE(pos: Int): Double {
        return buffer.getDoubleLE(pos)
    }

    override fun getFloat(pos: Int): Float {
        return buffer.getFloat(pos)
    }

    override fun getFloatLE(pos: Int): Float {
        return buffer.getFloatLE(pos)
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

    override fun getBytes(dst: ByteArray): Buffer {
        return buffer.getBytes(dst)
    }

    override fun getBytes(dst: ByteArray, dstIndex: Int): Buffer {
        return buffer.getBytes(dst, dstIndex)
    }

    override fun getBytes(start: Int, end: Int, dst: ByteArray): Buffer {
        return buffer.getBytes(start, end, dst)
    }

    override fun getBytes(start: Int, end: Int, dst: ByteArray, dstIndex: Int): Buffer {
        return buffer.getBytes(start, end, dst, dstIndex)
    }

    override fun getBuffer(start: Int, end: Int): Buffer {
        return buffer.getBuffer(start, end)
    }

    override fun getString(start: Int, end: Int, enc: String): String {
        return buffer.getString(start, end, enc)
    }

    override fun getString(start: Int, end: Int): String {
        return buffer.getString(start, end)
    }

    override fun appendBuffer(b: Buffer): BufferInternal {
        return buffer.appendBuffer(b)
    }

    override fun appendBuffer(b: Buffer, offset: Int, len: Int): BufferInternal {
        return buffer.appendBuffer(b, offset, len)
    }

    override fun appendBytes(bytes: ByteArray): BufferInternal {
        return buffer.appendBytes(bytes)
    }

    override fun appendBytes(bytes: ByteArray, offset: Int, len: Int): BufferInternal {
        return buffer.appendBytes(bytes, offset, len)
    }

    override fun appendByte(b: Byte): BufferInternal {
        return buffer.appendByte(b)
    }

    override fun appendUnsignedByte(b: Short): BufferInternal {
        return buffer.appendUnsignedByte(b)
    }

    override fun appendInt(i: Int): BufferInternal {
        return buffer.appendInt(i)
    }

    override fun appendIntLE(i: Int): BufferInternal {
        return buffer.appendIntLE(i)
    }

    override fun appendUnsignedInt(i: Long): BufferInternal {
        return buffer.appendUnsignedInt(i)
    }

    override fun appendUnsignedIntLE(i: Long): BufferInternal {
        return buffer.appendUnsignedIntLE(i)
    }

    override fun appendMedium(i: Int): BufferInternal {
        return buffer.appendMedium(i)
    }

    override fun appendMediumLE(i: Int): BufferInternal {
        return buffer.appendMediumLE(i)
    }

    override fun appendLong(l: Long): BufferInternal {
        return buffer.appendLong(l)
    }

    override fun appendLongLE(l: Long): BufferInternal {
        return buffer.appendLongLE(l)
    }

    override fun appendShort(s: Short): BufferInternal {
        return buffer.appendShort(s)
    }

    override fun appendShortLE(s: Short): BufferInternal {
        return buffer.appendShortLE(s)
    }

    override fun appendUnsignedShort(s: Int): BufferInternal {
        return buffer.appendUnsignedShort(s)
    }

    override fun appendUnsignedShortLE(s: Int): BufferInternal {
        return buffer.appendUnsignedShortLE(s)
    }

    override fun appendFloat(f: Float): BufferInternal {
        return buffer.appendFloat(f)
    }

    override fun appendFloatLE(f: Float): BufferInternal {
        return buffer.appendFloatLE(f)
    }

    override fun appendDouble(d: Double): BufferInternal {
        return buffer.appendDouble(d)
    }

    override fun appendDoubleLE(d: Double): BufferInternal {
        return buffer.appendDoubleLE(d)
    }

    override fun appendString(str: String, enc: String): BufferInternal {
        return buffer.appendString(str, enc)
    }

    override fun appendString(str: String): BufferInternal {
        return buffer.appendString(str)
    }

    override fun setByte(pos: Int, b: Byte): BufferInternal {
        return buffer.setByte(pos, b)
    }

    override fun setUnsignedByte(pos: Int, b: Short): BufferInternal {
        return buffer.setUnsignedByte(pos, b)
    }

    override fun setInt(pos: Int, i: Int): BufferInternal {
        return buffer.setInt(pos, i)
    }

    override fun setIntLE(pos: Int, i: Int): BufferInternal {
        return buffer.setIntLE(pos, i)
    }

    override fun setUnsignedInt(pos: Int, i: Long): BufferInternal {
        return buffer.setUnsignedInt(pos, i)
    }

    override fun setUnsignedIntLE(pos: Int, i: Long): BufferInternal {
        return buffer.setUnsignedIntLE(pos, i)
    }

    override fun setMedium(pos: Int, i: Int): BufferInternal {
        return buffer.setMedium(pos, i)
    }

    override fun setMediumLE(pos: Int, i: Int): BufferInternal {
        return buffer.setMediumLE(pos, i)
    }

    override fun setLong(pos: Int, l: Long): BufferInternal {
        return buffer.setLong(pos, l)
    }

    override fun setLongLE(pos: Int, l: Long): BufferInternal {
        return buffer.setLongLE(pos, l)
    }

    override fun setDouble(pos: Int, d: Double): BufferInternal {
        return buffer.setDouble(pos, d)
    }

    override fun setDoubleLE(pos: Int, d: Double): BufferInternal {
        return buffer.setDoubleLE(pos, d)
    }

    override fun setFloat(pos: Int, f: Float): BufferInternal {
        return buffer.setFloat(pos, f)
    }

    override fun setFloatLE(pos: Int, f: Float): BufferInternal {
        return buffer.setFloatLE(pos, f)
    }

    override fun setShort(pos: Int, s: Short): BufferInternal {
        return buffer.setShort(pos, s)
    }

    override fun setShortLE(pos: Int, s: Short): BufferInternal {
        return buffer.setShortLE(pos, s)
    }

    override fun setUnsignedShort(pos: Int, s: Int): BufferInternal {
        return buffer.setUnsignedShort(pos, s)
    }

    override fun setUnsignedShortLE(pos: Int, s: Int): BufferInternal {
        return buffer.setUnsignedShortLE(pos, s)
    }

    override fun setBuffer(pos: Int, b: Buffer): BufferInternal {
        return buffer.setBuffer(pos, b)
    }

    override fun setBuffer(pos: Int, b: Buffer, offset: Int, len: Int): BufferInternal {
        return buffer.setBuffer(pos, b, offset, len)
    }

    override fun setBytes(pos: Int, b: ByteBuffer): BufferInternal {
        return buffer.setBytes(pos, b)
    }

    override fun setBytes(pos: Int, b: ByteArray): BufferInternal {
        return buffer.setBytes(pos, b)
    }

    override fun setBytes(pos: Int, b: ByteArray, offset: Int, len: Int): BufferInternal {
        return buffer.setBytes(pos, b, offset, len)
    }

    override fun setString(pos: Int, str: String): BufferInternal {
        return buffer.setString(pos, str)
    }

    override fun setString(pos: Int, str: String, enc: String): BufferInternal {
        return buffer.setString(pos, str, enc)
    }

    override fun length(): Int {
        return buffer.length()
    }

    override fun slice(): BufferInternal {
        return buffer.slice()
    }

    override fun slice(start: Int, end: Int): BufferInternal {
        return buffer.slice(start, end)
    }

    override fun getByteBuf(): ByteBuf {
        return buffer.byteBuf
    }

    override fun writeToBuffer(b: Buffer) {
        return buffer.writeToBuffer(b)
    }

    override fun readFromBuffer(pos: Int, b: Buffer): Int {
        return buffer.readFromBuffer(pos, b)
    }
}