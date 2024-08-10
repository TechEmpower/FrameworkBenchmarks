using System.Text.Json;
using appMpower.Orm.Objects;

namespace appMpower.Orm.Serializers
{
   public class CachedWorldsSerializer : IJsonSerializer<CachedWorld[]>
   {
      public void Serialize(Utf8JsonWriter utf8JsonWriter, CachedWorld[] cachedWorlds)
      {
         utf8JsonWriter.WriteStartArray();

         foreach (CachedWorld cachedWorld in cachedWorlds)
         {
            utf8JsonWriter.WriteStartObject();
            utf8JsonWriter.WriteNumber("id", cachedWorld.Id);
            utf8JsonWriter.WriteNumber("randomNumber", cachedWorld.RandomNumber);
            utf8JsonWriter.WriteEndObject();
         }

         utf8JsonWriter.WriteEndArray();
         utf8JsonWriter.Flush();
      }
   }
}