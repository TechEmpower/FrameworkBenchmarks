using System.Text.Json;

namespace appMpower.Kestrel
{
   public interface IJsonSerializer<T>
   {
      public void Serialize(Utf8JsonWriter utf8JsonWriter, T t);
   }
}