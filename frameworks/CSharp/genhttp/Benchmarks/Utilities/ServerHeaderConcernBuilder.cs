using GenHTTP.Api.Content;

namespace Benchmarks.Utilities;

public sealed class ServerHeaderConcernBuilder : IConcernBuilder
{

    public IConcern Build(IHandler content) => new ServerHeaderConcern(content);

}
