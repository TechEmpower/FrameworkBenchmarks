package pronghorn.types.codecs

import org.bson.*
import org.bson.codecs.*
import pronghorn.types.Fortune

object FortuneCodec : Codec<Fortune> {
    private val fortuneClass = Fortune::class.java

    override fun encode(writer: BsonWriter, value: Fortune, encoderContext: EncoderContext) {
        writer.writeStartDocument()
        writer.writeInt32(value.id)
        writer.writeString(value.message)
        writer.writeEndDocument()
    }

    override fun getEncoderClass(): Class<Fortune> = fortuneClass

    override fun decode(reader: BsonReader, decoderContext: DecoderContext): Fortune {
        reader.readStartDocument()
        reader.readBsonType()
        reader.skipName()
        val id = when(reader.currentBsonType) {
            BsonType.DOUBLE -> reader.readDouble().toInt()
            BsonType.INT32 -> reader.readInt32()
            else -> throw Exception("Unexpected ID Type")
        }
        reader.readBsonType()
        reader.skipName()
        reader.skipValue()
        val message = reader.readString("message")
        val fortune = Fortune(id, message)
        reader.readEndDocument()
        return fortune
    }
}
