using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Web;
using Benchmarks.Model;

using Cottle;

using GenHTTP.Api.Content;
using GenHTTP.Api.Protocol;

using GenHTTP.Modules.IO;
using GenHTTP.Modules.Pages;

namespace Benchmarks.Tests;

public class FortuneHandler : IHandler
{
    private IDocument _template;

    #region Functionality

    public async ValueTask PrepareAsync()
    {
        var resource = Resource.FromAssembly("Template.html").Build();

        using var reader = new StreamReader(await resource.GetContentAsync());
        
        _template = Document.CreateDefault(reader).DocumentOrThrow;
    }

    public async ValueTask<IResponse> HandleAsync(IRequest request)
    {
        if (_template == null)
        {
            await PrepareAsync();
        }
        
        var template = _template ?? throw new InvalidOperationException("Template has not been initialized");
        
        var fortunes = await GetFortunes();
        
        var context = BuildContext(fortunes);

        var content = template.Render(context);

        return request.GetPage(content).Build();
    }

    private static async Task<List<Fortune>> GetFortunes()
    {
        var fortunes = await QueryDatabaseAsync();
        
        fortunes.Add(new() { Id = 0, Message = "Additional fortune added at request time." });
        
        fortunes.Sort((x, y) => string.CompareOrdinal(x.Message, y.Message));
        
        return fortunes;
    }

    private static async Task<List<Fortune>> QueryDatabaseAsync()
    {
        await using var connection = await Database.DataSource.OpenConnectionAsync();

        await using var command = connection.CreateCommand();

        command.CommandText = "SELECT id, message FROM fortune";
        
        var result = new List<Fortune>(16);
        
        await using var reader = await command.ExecuteReaderAsync();
        
        while (await reader.ReadAsync())
        {
            result.Add(new ()
            {
                Id = reader.GetInt32(0),
                Message = HttpUtility.HtmlEncode(reader.GetString(1))
            });
        }

        return result;
    }

    private static IContext BuildContext(List<Fortune> cookies)
    {
        var values = new Value[cookies.Count];
        
        for (var i = 0; i < cookies.Count; i++)
        {
            values[i] = Value.FromMap(new FortuneMap(cookies[i].Id, cookies[i].Message));
        }
        
        var store = new Dictionary<Value, Value>
        {
            ["cookies"] = Value.FromEnumerable(values.ToArray())
        };
        
        return Context.CreateBuiltin(store);
    }
    
    #endregion
    
}
