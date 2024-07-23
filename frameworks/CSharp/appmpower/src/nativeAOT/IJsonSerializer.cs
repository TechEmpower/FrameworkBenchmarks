using System.Text.Json;

namespace nativeAOT
{
   public interface IJsonSerializer<T>
   {
      public void Serialize(Utf8JsonWriter utf8JsonWriter, T t);
   }
}