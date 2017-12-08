package io.sinistral.models;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WorldEncoder extends com.jsoniter.spi.EmptyEncoder
{
	private static Logger Logger = LoggerFactory.getLogger(WorldEncoder.class.getCanonicalName());
 
	public void encode(Object obj, com.jsoniter.output.JsonStream stream) throws java.io.IOException
	{
		if (obj == null)
		{
			stream.writeNull();
			return;
		}
		stream.writeRaw("{\"id\":", 6);
		encode_((io.sinistral.models.World) obj, stream);
		stream.writeByte(com.jsoniter.output.JsonStream.OBJECT_END);
	}

	public static void encode_(io.sinistral.models.World obj, com.jsoniter.output.JsonStream stream) throws java.io.IOException
	{
		stream.writeVal((int) obj.id);
		stream.writeRaw(",\"randomNumber\":", 16);
		stream.writeVal((int) obj.randomNumber);
	}
	
	public static void encodeRaw(io.sinistral.models.World obj, java.io.OutputStream stream)  
	{
		try
		{ 
				stream.write(com.jsoniter.output.JsonStream.OBJECT_START);
				stream.write("\"id\":".getBytes());
				stream.write(Integer.toString(obj.id).getBytes());
				stream.write(com.jsoniter.output.JsonStream.COMMA);
				stream.write("\"randomNumber\":".getBytes());
				stream.write(Integer.toString(obj.randomNumber).getBytes());
				stream.write(com.jsoniter.output.JsonStream.OBJECT_END);
		 
		} catch (Exception e)
		{
			Logger.error(e.getMessage());
		}
	}
}
