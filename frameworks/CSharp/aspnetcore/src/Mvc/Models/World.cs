using System.ComponentModel.DataAnnotations.Schema;
using System.Runtime.Serialization;
using System.Text.Json.Serialization;

namespace Mvc.Models;

[Table("world")]
public class World
{
    [Column("id")]
    public int Id { get; set; }

    [IgnoreDataMember]
    [NotMapped]
    [JsonIgnore]
    public int _Id { get; set; }

    [Column("randomnumber")]
    public int RandomNumber { get; set; }
}