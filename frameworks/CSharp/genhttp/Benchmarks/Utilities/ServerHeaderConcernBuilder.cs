using GenHTTP.Api.Content;

namespace Benchmarks.Utilities;

public sealed class ServerHeaderConcernBuilder : IConcernBuilder
{

    public IConcern Build(IHandler parent, Func<IHandler, IHandler> contentFactory) => new ServerHeaderConcern(parent, contentFactory);

}
