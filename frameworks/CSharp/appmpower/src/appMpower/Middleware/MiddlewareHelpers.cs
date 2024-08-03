// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using Microsoft.AspNetCore.Http;

namespace appMpower; 

public static class MiddlewareHelpers
{
    public static int GetMultipleQueriesQueryCount(HttpContext httpContext)
    {
        int queries = 1;
        var queriesRaw = httpContext.Request.Query["queries"];

        if (queriesRaw.Count == 1)
        {
            int.TryParse(queriesRaw, out queries);
        }

        return queries > 500 ? 500 : (queries > 0 ? queries : 1);
    }
}
