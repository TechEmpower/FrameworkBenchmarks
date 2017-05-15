package com.networknt.techempower.model;

import com.dslplatform.json.JsonObject;
import com.dslplatform.json.JsonWriter;
import com.dslplatform.json.NumberConverter;
import com.sun.org.apache.xpath.internal.operations.Number;

/**
 * The model for the "world" database table.
 */
public final class World implements JsonObject {
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

    @Override
    public void serialize(JsonWriter writer, boolean minimal) {
        writer.writeAscii("{\"id\":");
        NumberConverter.serialize(this.id, writer);
        writer.writeAscii(",\"randomNumber\":");
        NumberConverter.serialize(this.randomNumber, writer);
        writer.writeByte(com.dslplatform.json.JsonWriter.OBJECT_END);
    }
}
