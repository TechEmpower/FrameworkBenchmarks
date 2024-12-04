using GenHTTP.Api.Content;
using GenHTTP.Api.Protocol;

namespace Benchmarks.Utilities;

public sealed class ServerHeaderConcern : IConcern
{

    #region Initialization

    public ServerHeaderConcern(IHandler content)
    {
        Content = content;
    }

    #endregion

    #region Get-/Setters

    public IHandler Content { get; }

    #endregion

    #region Functionality

    public ValueTask PrepareAsync() => Content.PrepareAsync();

    public async ValueTask<IResponse> HandleAsync(IRequest request)
    {
        var response = await Content.HandleAsync(request);

        if (response != null)
        {
            response.Headers.Add("Server", "TFB");
        }

        return response;
    }

    #endregion

}
