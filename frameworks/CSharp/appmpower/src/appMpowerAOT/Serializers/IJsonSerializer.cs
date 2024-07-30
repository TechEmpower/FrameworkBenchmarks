using System.Text.Json;

namespace appMpowerAot.Serializers
{
   public interface IJsonSerializer<T>
   {
      public void Serialize(Utf8JsonWriter utf8JsonWriter, T t);
   }
}