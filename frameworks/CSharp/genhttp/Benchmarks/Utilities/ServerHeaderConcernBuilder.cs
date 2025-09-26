using System;

using GenHTTP.Api.Content;

namespace Benchmarks.Utilities
{

    public sealed class ServerHeaderConcernBuilder : IConcernBuilder
    {

        public IConcern Build(IHandler parent, Func<IHandler, IHandler> contentFactory)
        {
            return new ServerHeaderConcern(parent, contentFactory);
        }

    }

}
