using System.Text.Encodings.Web;
using System.Text.Unicode;
using Microsoft.AspNetCore.Builder;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;

public class Startup
{
    private readonly IConfiguration _configuration;

    public Startup(IConfiguration configuration)
    {
        _configuration = configuration;
    }

    public void ConfigureServices(IServiceCollection services)
    {
        //services.Configure<AppSettings>(_configuration);
        services.AddSingleton<ConcurrentRandom>();

        var appSettings = _configuration.Get<AppSettings>();
        services.AddSingleton(appSettings);

        /*
        if (appSettings.Database == DatabaseServer.PostgreSql)
        {
            services.AddSingleton<DbProviderFactory>(NpgsqlFactory.Instance);
            services.AddSingleton<RawDb>();
        }
        else if (appSettings.Database == DatabaseServer.MySql)
        {
            services.AddSingleton<DbProviderFactory>(MySqlConnectorFactory.Instance);
            services.AddSingleton<RawDb>();
        }
        */

#if ADO      
        NativeMethods.DbProvider(0); 
#else
        NativeMethods.DbProvider(1); //ODBC
#endif        

#if POSTGRESQL      
        NativeMethods.Dbms(1); 
#else
        NativeMethods.Dbms(0); //MySQL
#endif

        var settings = new TextEncoderSettings(UnicodeRanges.BasicLatin, UnicodeRanges.Katakana, UnicodeRanges.Hiragana);

        settings.AllowCharacter('—');
        services.AddWebEncoders(options =>
        {
            options.TextEncoderSettings = settings;
        });
    }

    public void Configure(IApplicationBuilder app)
    {
        app.UsePlainText();
        app.UseJson();
        app.UseUtf8Json();
        app.UseFortunesRaw();
        app.UseSingleQueryRaw();
        app.UseMultipleQueriesRaw();
        app.UseMultipleUpdatesRaw();
    }
}

// Assume these classes and methods are defined somewhere else in your project

public enum DatabaseServer
{
    PostgreSql,
    MySql
}

public class ConcurrentRandom
{
    // Implementation of ConcurrentRandom class
}

public class RawDb
{
    // Implementation of RawDb class
}

// Extension methods for IApplicationBuilder (placeholders for actual implementations)
public static class ApplicationBuilderExtensions
{
    public static void UseUtf8Json(this IApplicationBuilder app)
    {
        // Implementation for UseUtf8Json middleware
    }

    public static void UseFortunesRaw(this IApplicationBuilder app)
    {
        // Implementation for UseFortunesRaw middleware
        /*
        private readonly static KeyValuePair<string, StringValues> _headerServer =
            new KeyValuePair<string, StringValues>("Server", "k");
        private readonly static KeyValuePair<string, StringValues> _headerContentType =
            new KeyValuePair<string, StringValues>("Content-Type", "text/html; charset=UTF-8");
         headerDictionary.Add(new KeyValuePair<string, StringValues>("Content-Length", (writer.Length + 32).ToString()));
        */

    }

    public static void UseMultipleQueriesRaw(this IApplicationBuilder app)
    {
        // Implementation for UseMultipleQueriesRaw middleware
    }

    public static void UseMultipleUpdatesRaw(this IApplicationBuilder app)
    {
        // Implementation for UseMultipleUpdatesRaw middleware
    }
}
