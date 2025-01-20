using System.Text.Json;

namespace appMpower.Serializers
{
   public interface IJsonSerializer<T>
   {
      public void Serialize(Utf8JsonWriter utf8JsonWriter, T t);
   }
}