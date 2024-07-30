using System.Text.Json;
using appMpowerAot.DataObjects;

namespace appMpowerAot.Serializers
{
   public class CachedWorldSerializer : IJsonSerializer<CachedWorld>
   {
      public void Serialize(Utf8JsonWriter utf8JsonWriter, CachedWorld world)
      {
         utf8JsonWriter.WriteStartObject();
         utf8JsonWriter.WriteNumber("id", world.Id);
         utf8JsonWriter.WriteNumber("randomNumber", world.RandomNumber);
         utf8JsonWriter.WriteEndObject();
         utf8JsonWriter.Flush();
      }
   }
}