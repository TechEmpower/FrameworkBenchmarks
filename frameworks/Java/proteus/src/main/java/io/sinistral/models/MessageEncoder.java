package io.sinistral.models;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MessageEncoder extends com.jsoniter.spi.EmptyEncoder
{
	private static Logger Logger = LoggerFactory.getLogger(MessageEncoder.class.getCanonicalName());

	public void encode(Object obj, com.jsoniter.output.JsonStream stream) throws java.io.IOException
	{
		if (obj == null)
		{
			stream.writeNull();
			return;
		}
		stream.writeByte(com.jsoniter.output.JsonStream.OBJECT_START);
		encode_((io.sinistral.models.Message) obj, stream);
		stream.writeByte(com.jsoniter.output.JsonStream.OBJECT_END);
	}

	public static void encode_(io.sinistral.models.Message obj, com.jsoniter.output.JsonStream stream) throws java.io.IOException
	{
		boolean notFirst = false;
		if (obj.message != null)
		{
			if (notFirst)
			{
				stream.write(com.jsoniter.output.JsonStream.COMMA);
			}
			else
			{
				notFirst = true;
			}
			stream.writeRaw("\"message\":", 10);
			stream.writeString((java.lang.String) obj.message);
		}
	}
	
	public static void encodeRaw(io.sinistral.models.Message obj, java.io.OutputStream stream)  
	{
		try
		{
			boolean notFirst = false;
			if (obj.message != null)
			{
				if (notFirst)
				{
					stream.write(com.jsoniter.output.JsonStream.COMMA);
				}
				else
				{
					notFirst = true;
				}
				stream.write(com.jsoniter.output.JsonStream.OBJECT_START);
				stream.write("\"message\":".getBytes());
				stream.write(com.jsoniter.output.JsonStream.QUOTE); 
				stream.write(obj.message.getBytes());
				stream.write(com.jsoniter.output.JsonStream.QUOTE);
				stream.write(com.jsoniter.output.JsonStream.OBJECT_END);
			}
		} catch (Exception e)
		{
			Logger.error(e.getMessage());
		}
	}
}
