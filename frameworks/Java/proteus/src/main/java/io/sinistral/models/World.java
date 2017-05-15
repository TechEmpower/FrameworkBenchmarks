/**
 * 
 */
package io.sinistral.models;

import com.jsoniter.output.JsonStream;
import com.jsoniter.output.StreamImplNumber;
import com.jsoniter.output.StreamImplString;

/**
 * @author jbauer
 *
 */
public final class World {
    public int id;
    public int randomNumber;

    /**
     * Constructs a new world object with the given parameters.
     *
     * @param id the ID of the world
     * @param randomNumber the random number of the world
     */
    public World(int id, int randomNumber) {
        this.id = id;
        this.randomNumber = randomNumber;
    }

 
    public void serialize(JsonStream stream) throws Exception {
        stream.writeByte(JsonStream.OBJECT_START);
        StreamImplString.writeStringWithoutQuote(stream,"\"id\":");
        StreamImplNumber.writeInt(stream,this.id);
        stream.writeByte(JsonStream.COMMA);

        StreamImplString.writeStringWithoutQuote(stream,",\"randomNumber\":");
        StreamImplNumber.writeInt(stream,this.randomNumber);
        stream.writeByte(JsonStream.OBJECT_END);
    }
}
