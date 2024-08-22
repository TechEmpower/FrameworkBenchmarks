using Microsoft.EntityFrameworkCore;
using System.ComponentModel.DataAnnotations;
using System.Runtime.Serialization;

public static class ConnectionString
{
    public static string? Value { get; set; }
    public static string Set(string ConnectionString) => Value = ConnectionString;
}

public class DatabaseContext : DbContext
{
    public DatabaseContext() : base()
    {
        // Create Database And Tables If Database Not Exist; If Exist Database, If Never Exist Any Tables, Create Tables In Database
        Database.EnsureCreated();
    }

    // Define Tables
    public DbSet<WorldRow> World { get; set; }
    public DbSet<CachedWorldRow> CachedWorld { get; set; }
    public DbSet<FortuneRow> Fortune { get; set; }

    protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder) => optionsBuilder.UseNpgsql(ConnectionString.Value);

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {

    }
}

/* Start Table Rows */

public class WorldRow : object
{
    [Key]
    public int id { get; set; }
    public int randomNumber { get; set; }
}

public class CachedWorldRow : object
{
    [Key]
    public int id { get; set; }
    public int randomNumber { get; set; }
}

public class FortuneRow : IComparable<FortuneRow>, IComparable
{
    [Key]
    public int id { get; set; }

    [StringLength(2048)]
    [IgnoreDataMember]
    [Required]
    public string? message { get; set; }

    public int CompareTo(object? obj)
    {
        return CompareTo((FortuneRow?)obj);
    }

    public int CompareTo(FortuneRow? other)
    {
        return string.CompareOrdinal(message, other?.message);
    }
}

/* End Table Rows */