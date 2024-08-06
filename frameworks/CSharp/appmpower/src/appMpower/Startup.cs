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

#if !DEBUG
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
        app.UseFortunes();
        app.UseMultipleQueries();
        app.UseMultipleUpdates();
    }
}