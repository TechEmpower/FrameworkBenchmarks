using System.ComponentModel.DataAnnotations.Schema;

namespace Benchmarks.Model
{

    [Table("world")]
    public class World
    {

        [Column("id")]
        public int Id { get; set; }

        [Column("randomnumber")]
        public int RandomNumber { get; set; }

    }

}
