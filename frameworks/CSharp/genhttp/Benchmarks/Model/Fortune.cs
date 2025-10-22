using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace Benchmarks.Model;

[Table("fortune")]
public class Fortune
{

    [Column("id")]
    public int Id { get; set; }

    [Column("message")]
    [StringLength(2048)]
    public string Message { get; set; }

}
