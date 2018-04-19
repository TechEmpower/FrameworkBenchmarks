/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert.json;

import java.io.*;
import java.lang.reflect.*;
import java.nio.*;
import java.nio.charset.*;
import java.util.function.*;
import org.redkale.convert.*;
import org.redkale.util.*;

/**
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@SuppressWarnings("unchecked")
public final class JsonConvert extends TextConvert<JsonReader, JsonWriter> {

    public static final Type TYPE_MAP_STRING_STRING = new TypeToken<java.util.HashMap<String, String>>() {
    }.getType();

    private static final ObjectPool<JsonReader> readerPool = JsonReader.createPool(Integer.getInteger("convert.json.pool.size", 16));

    private static final ObjectPool<JsonWriter> writerPool = JsonWriter.createPool(Integer.getInteger("convert.json.pool.size", 16));

    private final boolean tiny;

    protected JsonConvert(JsonFactory factory, boolean tiny) {
        super(factory);
        this.tiny = tiny;
    }

    @Override
    public JsonFactory getFactory() {
        return (JsonFactory) factory;
    }

    public static JsonConvert root() {
        return JsonFactory.root().getConvert();
    }

    //------------------------------ reader -----------------------------------------------------------
    public JsonReader pollJsonReader(final ByteBuffer... buffers) {
        return new JsonByteBufferReader((ConvertMask) null, buffers);
    }

    public JsonReader pollJsonReader(final InputStream in) {
        return new JsonStreamReader(in);
    }

    public JsonReader pollJsonReader() {
        return readerPool.get();
    }

    public void offerJsonReader(final JsonReader in) {
        if (in != null) readerPool.accept(in);
    }

    //------------------------------ writer -----------------------------------------------------------
    public JsonByteBufferWriter pollJsonWriter(final Supplier<ByteBuffer> supplier) {
        return new JsonByteBufferWriter(tiny, supplier);
    }

    public JsonWriter pollJsonWriter(final OutputStream out) {
        return new JsonStreamWriter(tiny, out);
    }

    public JsonWriter pollJsonWriter(final Charset charset, final OutputStream out) {
        return new JsonStreamWriter(tiny, charset, out);
    }

    public JsonWriter pollJsonWriter() {
        return writerPool.get().tiny(tiny);
    }

    public void offerJsonWriter(final JsonWriter out) {
        if (out != null) writerPool.accept(out);
    }

    //------------------------------ convertFrom -----------------------------------------------------------
    public <T> T convertFrom(final Type type, final String text) {
        if (text == null) return null;
        return convertFrom(type, Utility.charArray(text));
    }

    public <T> T convertFrom(final Type type, final char[] text) {
        if (text == null) return null;
        return convertFrom(type, text, 0, text.length);
    }

    public <T> T convertFrom(final Type type, final char[] text, final int start, final int len) {
        if (text == null || type == null) return null;
        final JsonReader in = readerPool.get();
        in.setText(text, start, len);
        T rs = (T) factory.loadDecoder(type).convertFrom(in);
        readerPool.accept(in);
        return rs;
    }

    public <T> T convertFrom(final Type type, final InputStream in) {
        if (type == null || in == null) return null;
        return (T) factory.loadDecoder(type).convertFrom(new JsonStreamReader(in));
    }

    @Override
    public <T> T convertFrom(final Type type, final ByteBuffer... buffers) {
        if (type == null || buffers == null || buffers.length == 0) return null;
        return (T) factory.loadDecoder(type).convertFrom(new JsonByteBufferReader((ConvertMask) null, buffers));
    }

    @Override
    public <T> T convertFrom(final Type type, final ConvertMask mask, final ByteBuffer... buffers) {
        if (type == null || buffers == null || buffers.length == 0) return null;
        return (T) factory.loadDecoder(type).convertFrom(new JsonByteBufferReader(mask, buffers));
    }

    public <T> T convertFrom(final Type type, final JsonReader reader) {
        if (type == null) return null;
        @SuppressWarnings("unchecked")
        T rs = (T) factory.loadDecoder(type).convertFrom(reader);
        return rs;
    }

    //------------------------------ convertTo -----------------------------------------------------------
    @Override
    public String convertTo(final Object value) {
        if (value == null) return "null";
        return convertTo(value.getClass(), value);
    }

    @Override
    public String convertTo(final Type type, final Object value) {
        if (type == null) return null;
        if (value == null) return "null";
        final JsonWriter out = writerPool.get().tiny(tiny);
        factory.loadEncoder(type).convertTo(out, value);
        String result = out.toString();
        writerPool.accept(out);
        return result;
    }

    @Override
    public String convertMapTo(final Object... values) {
        if (values == null) return "null";
        final JsonWriter out = writerPool.get().tiny(tiny);
        ((AnyEncoder) factory.getAnyEncoder()).convertMapTo(out, values);
        String result = out.toString();
        writerPool.accept(out);
        return result;
    }

    public void convertTo(final OutputStream out, final Object value) {
        if (value == null) {
            new JsonStreamWriter(tiny, out).writeNull();
        } else {
            convertTo(out, value.getClass(), value);
        }
    }

    public void convertTo(final OutputStream out, final Type type, final Object value) {
        if (type == null) return;
        if (value == null) {
            new JsonStreamWriter(tiny, out).writeNull();
        } else {
            final JsonWriter writer = writerPool.get().tiny(tiny);
            factory.loadEncoder(type).convertTo(writer, value);
            byte[] bs = writer.toBytes();
            writerPool.accept(writer);
            try {
                out.write(bs);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }

    public void convertMapTo(final OutputStream out, final Object... values) {
        if (values == null) {
            new JsonStreamWriter(tiny, out).writeNull();
        } else {
            final JsonWriter writer = writerPool.get().tiny(tiny);
            ((AnyEncoder) factory.getAnyEncoder()).convertMapTo(writer, values);
            byte[] bs = writer.toBytes();
            writerPool.accept(writer);
            try {
                out.write(bs);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    public ByteBuffer[] convertTo(final Supplier<ByteBuffer> supplier, final Object value) {
        if (supplier == null) return null;
        JsonByteBufferWriter out = new JsonByteBufferWriter(tiny, null, supplier);
        if (value == null) {
            out.writeNull();
        } else {
            factory.loadEncoder(value.getClass()).convertTo(out, value);
        }
        return out.toBuffers();
    }

    @Override
    public ByteBuffer[] convertTo(final Supplier<ByteBuffer> supplier, final Type type, final Object value) {
        if (supplier == null || type == null) return null;
        JsonByteBufferWriter out = new JsonByteBufferWriter(tiny, null, supplier);
        if (value == null) {
            out.writeNull();
        } else {
            factory.loadEncoder(type).convertTo(out, value);
        }
        return out.toBuffers();
    }

    @Override
    public ByteBuffer[] convertMapTo(final Supplier<ByteBuffer> supplier, final Object... values) {
        if (supplier == null) return null;
        JsonByteBufferWriter out = new JsonByteBufferWriter(tiny, null, supplier);
        if (values == null) {
            out.writeNull();
        } else {
            ((AnyEncoder) factory.getAnyEncoder()).convertMapTo(out, values);
        }
        return out.toBuffers();
    }

    public void convertTo(final JsonWriter writer, final Object value) {
        if (value == null) {
            writer.writeNull();
        } else {
            factory.loadEncoder(value.getClass()).convertTo(writer, value);
        }
    }

    public void convertTo(final JsonWriter writer, final Type type, final Object value) {
        if (type == null) return;
        if (value == null) {
            writer.writeNull();
        } else {
            factory.loadEncoder(type).convertTo(writer, value);
        }
    }

    public void convertMapTo(final JsonWriter writer, final Object... values) {
        if (values == null) {
            writer.writeNull();
        } else {
            ((AnyEncoder) factory.getAnyEncoder()).convertMapTo(writer, values);
        }
    }

    public JsonWriter convertToWriter(final Object value) {
        if (value == null) return null;
        return convertToWriter(value.getClass(), value);
    }

    public JsonWriter convertToWriter(final Type type, final Object value) {
        if (type == null) return null;
        final JsonWriter out = writerPool.get().tiny(tiny);
        factory.loadEncoder(type).convertTo(out, value);
        return out;
    }

    public JsonWriter convertMapToWriter(final Object... values) {
        final JsonWriter out = writerPool.get().tiny(tiny);
        ((AnyEncoder) factory.getAnyEncoder()).convertMapTo(out, values);
        return out;
    }
}
