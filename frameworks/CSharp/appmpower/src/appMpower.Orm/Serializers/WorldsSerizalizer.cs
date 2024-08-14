using System.Text.Json;
using appMpower.Orm.Objects;

namespace appMpower.Orm.Serializers
{
   public class WorldsSerializer : IJsonSerializer<World[]>
   {
      public void Serialize(Utf8JsonWriter utf8JsonWriter, World[] worlds)
      {
         utf8JsonWriter.WriteStartArray();

         foreach (World world in worlds)
         {
            utf8JsonWriter.WriteStartObject();
            utf8JsonWriter.WriteNumber("id", world.Id);
            utf8JsonWriter.WriteNumber("randomNumber", world.RandomNumber);
            utf8JsonWriter.WriteEndObject();
         }

         utf8JsonWriter.WriteEndArray();
         //utf8JsonWriter.Flush();
      }
   }
}
