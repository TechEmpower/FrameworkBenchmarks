package pronghorn.types.codecs

import org.bson.*
import org.bson.codecs.*
import pronghorn.types.World

object WorldCodec: Codec<World> {
    private val worldClass = World::class.java

    override fun encode(writer: BsonWriter, value: World, encoderContext: EncoderContext) {
        writer.writeStartDocument()
        writer.writeInt32(value.id)
        writer.writeInt32(value.randomNumber)
        writer.writeEndDocument()
    }

    override fun getEncoderClass(): Class<World> = worldClass

    override fun decode(reader: BsonReader, decoderContext: DecoderContext): World {
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
        reader.readBsonType()
        reader.skipName()
        val randomNumber = when(reader.currentBsonType) {
            BsonType.DOUBLE -> reader.readDouble().toInt()
            BsonType.INT32 -> reader.readInt32()
            else -> throw Exception("Unexpected randomNumber Type")
        }
        val world = World(id, randomNumber)
        reader.readEndDocument()
        return world
    }
}
