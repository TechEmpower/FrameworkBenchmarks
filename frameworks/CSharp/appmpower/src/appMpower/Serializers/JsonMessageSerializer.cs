using System.Text.Json;

namespace appMpower.Serializers
{
   public class JsonMessageSerializer : IJsonSerializer<JsonMessage>
   {
      private readonly static JsonMessageSerializer _jsonMessageSerializer = new JsonMessageSerializer();

      public void Serialize(Utf8JsonWriter utf8JsonWriter, JsonMessage jsonMessage)
      {
         utf8JsonWriter.WriteStartObject();
         utf8JsonWriter.WriteString("message", jsonMessage.Message);
         utf8JsonWriter.WriteEndObject();
         //utf8JsonWriter.Flush();
      }
   }
}
