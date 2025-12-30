using System.Text.Json;
using appMpower.Orm.Objects;

namespace appMpower.Orm.Serializers
{
   public class FortunesSerializer : IJsonSerializer<List<Fortune>>
   {
      public void Serialize(Utf8JsonWriter utf8JsonWriter, List<Fortune> fortunes)
      {
         utf8JsonWriter.WriteStartArray();

         foreach (Fortune fortune in fortunes)
         {
            utf8JsonWriter.WriteStartObject();
            utf8JsonWriter.WriteNumber("id", fortune.Id);
            utf8JsonWriter.WriteString("message", fortune.Message);
            utf8JsonWriter.WriteEndObject();
         }

         utf8JsonWriter.WriteEndArray();
         utf8JsonWriter.Flush();
      }
   }
}