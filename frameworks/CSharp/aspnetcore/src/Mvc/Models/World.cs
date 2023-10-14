using System.ComponentModel.DataAnnotations.Schema;
using System.Runtime.Serialization;

namespace Mvc.Models;

[Table("world")]
public class World
{
    [Column("id")]
    public int Id { get; set; }

    [IgnoreDataMember]
    [NotMapped]
    public int _Id { get; set; }

    [Column("randomnumber")]
    public int RandomNumber { get; set; }
}