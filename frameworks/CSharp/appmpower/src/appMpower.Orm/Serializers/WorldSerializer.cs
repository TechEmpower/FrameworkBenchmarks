using System.Text.Json;
using appMpower.Orm.Objects;

namespace appMpower.Orm.Serializers
{
   public class WorldSerializer : IJsonSerializer<World>
   {
      public void Serialize(Utf8JsonWriter utf8JsonWriter, World world)
      {
         utf8JsonWriter.WriteStartObject();
         utf8JsonWriter.WriteNumber("id", world.Id);
         utf8JsonWriter.WriteNumber("randomNumber", world.RandomNumber);
         utf8JsonWriter.WriteEndObject();
         utf8JsonWriter.Flush();
      }
   }
}
