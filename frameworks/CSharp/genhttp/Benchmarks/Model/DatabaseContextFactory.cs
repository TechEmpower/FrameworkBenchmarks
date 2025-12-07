namespace Benchmarks.Model;

using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Design;

public class DatabaseContextFactory : IDesignTimeDbContextFactory<DatabaseContext>
{

    public DatabaseContext CreateDbContext(string[] args)
    {
        var options = new DbContextOptionsBuilder<DatabaseContext>()
                      .UseNpgsql("Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;SSL Mode=Disable")
                      .Options;

        return new DatabaseContext(options);
    }

}
