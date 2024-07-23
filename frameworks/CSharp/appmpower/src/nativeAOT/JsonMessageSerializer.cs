using System.Text.Json;

namespace nativeAOT
{
   public class JsonMessageSerializer : IJsonSerializer<JsonMessage>
   {
      public void Serialize(Utf8JsonWriter utf8JsonWriter, JsonMessage jsonMessage)
      {
         utf8JsonWriter.WriteStartObject();
         utf8JsonWriter.WriteString("message", jsonMessage.message);
         utf8JsonWriter.WriteEndObject();
         utf8JsonWriter.Flush();
      }
   }
}
