using System.Text.Json;
using appMpowerAot.DataObjects;

namespace appMpowerAot.Serializers
{
   public class JsonMessageSerializer : IJsonSerializer<JsonMessage>
   {
      public void Serialize(Utf8JsonWriter utf8JsonWriter, JsonMessage jsonMessage)
      {
         utf8JsonWriter.WriteStartObject();
         utf8JsonWriter.WriteString("message", jsonMessage.Message);
         utf8JsonWriter.WriteEndObject();
         utf8JsonWriter.Flush();
      }
   }
}
