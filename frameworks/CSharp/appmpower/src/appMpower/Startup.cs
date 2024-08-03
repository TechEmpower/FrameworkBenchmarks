using System.Text.Encodings.Web;
using System.Text.Unicode;
using Microsoft.AspNetCore.Builder;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;

namespace appMpower; 

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
        //services.AddSingleton<ConcurrentRandom>();

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
        app.UseSingleQueryRaw();
        app.UseFortunesRaw();
        //app.UseMultipleQueriesRaw();
        //app.UseMultipleUpdatesRaw();
    }
}

// Assume these classes and methods are defined somewhere else in your project

// Extension methods for IApplicationBuilder (placeholders for actual implementations)
public static class ApplicationBuilderExtensions
{
    public static void UseMultipleQueriesRaw(this IApplicationBuilder app)
    {
        // Implementation for UseMultipleQueriesRaw middleware
    }

    public static void UseMultipleUpdatesRaw(this IApplicationBuilder app)
    {
        // Implementation for UseMultipleUpdatesRaw middleware
    }
}
