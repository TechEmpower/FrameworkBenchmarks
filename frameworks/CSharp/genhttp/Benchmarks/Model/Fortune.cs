using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace Benchmarks.Model
{

    [Table("fortune")]
    public class Fortune : IComparable<Fortune>, IComparable
    {

        [Column("id")]
        public int ID { get; set; }

        [Column("message")]
        [StringLength(2048)]
        public string Message { get; set; }

        public int CompareTo(object obj) => CompareTo((Fortune)obj);

        public int CompareTo(Fortune other) => string.CompareOrdinal(Message, other.Message);

    }

}
