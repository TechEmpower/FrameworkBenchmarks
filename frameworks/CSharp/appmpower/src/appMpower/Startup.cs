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
        var appSettings = _configuration.Get<AppSettings>();
        services.AddSingleton(appSettings);

#if !DEBUG
    #if ODBC      
        NativeMethods.DbProvider(1); //ODBC
    #else
        NativeMethods.DbProvider(0); //ADO
    #endif        

    #if POSTGRESQL      
        NativeMethods.Dbms(1); //PostgreSQL
    #else
        NativeMethods.Dbms(0); //MySQL
    #endif
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
        app.UseSingleQuery();
        app.UseCaching();
        app.UseFortunes();
        app.UseMultipleQueries();
        app.UseMultipleUpdates();
    }
}