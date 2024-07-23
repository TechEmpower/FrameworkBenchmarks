namespace appMpower
{
   public struct CachedWorld
   {
      public int Id { get; set; }

      public int RandomNumber { get; set; }

      public static implicit operator CachedWorld(World world) => new CachedWorld { Id = world.Id, RandomNumber = world.RandomNumber };
   }
}