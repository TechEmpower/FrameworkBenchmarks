using Benchmarks.Model;
using Cottle;
using GenHTTP.Api.Content;
using GenHTTP.Api.Protocol;

using GenHTTP.Modules.IO;
using GenHTTP.Modules.Pages;
using GenHTTP.Modules.Pages.Rendering;

using Microsoft.EntityFrameworkCore;

namespace Benchmarks.Tests;

public class FortuneHandler : IHandler
{

    #region Get-/Setters

    private TemplateRenderer Template { get; }

    #endregion

    #region Initialization

    public FortuneHandler()
    {
        var resource = Resource.FromAssembly("Template.html").Build();

        Template = Renderer.From(resource);
    }

    #endregion

    #region Functionality

    public ValueTask PrepareAsync() => new();

    public async ValueTask<IResponse> HandleAsync(IRequest request)
    {
        var data = new Dictionary<Value, Value>
        {
            ["cookies"] = Value.FromEnumerable(await GetFortunes())
        };

        return request.GetPage(await Template.RenderAsync(data)).Build();
    }

    private static async ValueTask<List<Value>> GetFortunes()
    {
        await using var context = DatabaseContext.CreateNoTracking();

        var fortunes = new List<Fortune>
        {
            new Fortune()
            {
                Id = 5,
                Message = "fdg"
            }
        };

            //await context.Fortune.ToListAsync().ConfigureAwait(false);

        var result = new List<Value>(fortunes.Count + 1);

        foreach (var fortune in fortunes)
        {
            result.Add(Value.FromDictionary(new Dictionary<Value, Value>()
            {
                ["id"] = fortune.Id,
                ["message"] = fortune.Message
            }));
        }

        result.Add(Value.FromDictionary(new Dictionary<Value, Value>()
        {
            ["message"] = "Additional fortune added at request time."
        }));

        result.Sort((one, two) =>
        {
            var firstMessage = one.Fields["message"].AsString;
            var secondMessage = two.Fields["message"].AsString;

            return string.Compare(firstMessage, secondMessage, StringComparison.Ordinal);
        });

        return result;
    }

    #endregion

}
