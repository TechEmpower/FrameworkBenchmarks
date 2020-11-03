// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Threading.Tasks;
using PlatformExtensions;

namespace PlatformBenchmarks
{
    public partial class BenchmarkApplication
    {
#if !DATABASE
        [Route("/plaintext")]
        static string PlainText() => "Hello, World!";

        [Route("/json")]
        static JsonMessage Json() => new JsonMessage { message = "Hello, World!" };
#else
        [Route("/db")]
        static Task<World> SingleQuery() => Db.GetRow();

        [Route("/queries/{count}")]
        static Task<World[]> MultipleQueries(int count) => Db.GetRows(count);

        [Route("/cached-worlds/{count}")]
        static Task<World[]> Caching(int count) => Db.GetCachedRows(count);

        [Route("/updates/{count}")]
        static Task<World[]> Updates(int count) => Db.UpdateAndGetRows(count);
#endif
        public static RawDb Db { get; set; }
    }

    public struct JsonMessage
    {
        public string message { get; set; }
    }
}