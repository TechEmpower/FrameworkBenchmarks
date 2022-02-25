using System;
using System.Collections.Generic;
using System.Threading.Tasks;

using GenHTTP.Api.Content;
using GenHTTP.Api.Protocol;

namespace Benchmarks.Utilities
{

    public sealed class ServerHeaderConcern : IConcern
    {

        #region Get-/Setters

        public IHandler Content { get; }

        public IHandler Parent { get; }

        #endregion

        #region Initialization

        public ServerHeaderConcern(IHandler parent, Func<IHandler, IHandler> contentFactory)
        {
            Parent = parent;
            Content = contentFactory(this);
        }

        #endregion

        #region Functionality

        public IEnumerable<ContentElement> GetContent(IRequest request) => Content.GetContent(request);

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

}
