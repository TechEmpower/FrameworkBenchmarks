using System.Text.Json;

namespace appMpower
{
   public class CachedWorldSerializer : Kestrel.IJsonSerializer<CachedWorld>
   {
      public void Serialize(Utf8JsonWriter utf8JsonWriter, CachedWorld world)
      {
         utf8JsonWriter.WriteStartObject();
         utf8JsonWriter.WriteNumber("id", world.Id);
         utf8JsonWriter.WriteNumber("randomNumber", world.RandomNumber);
         utf8JsonWriter.WriteEndObject();
      }
   }
}