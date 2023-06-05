// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Net;
using System.Runtime.InteropServices;

namespace PlatformBenchmarks;

public static class BenchmarkConfigurationHelpers
{
    public static IWebHostBuilder UseBenchmarksConfiguration(this IWebHostBuilder builder, IConfiguration configuration)
    {
        builder.UseConfiguration(configuration);

        builder.UseSockets(options =>
        {
            if (int.TryParse(builder.GetSetting("threadCount"), out var threadCount))
            {
                options.IOQueueCount = threadCount;
            }

            options.WaitForDataBeforeAllocatingBuffer = false;
            if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
            {
                options.UnsafePreferInlineScheduling = Environment.GetEnvironmentVariable("DOTNET_SYSTEM_NET_SOCKETS_INLINE_COMPLETIONS") == "1";
            }

            Console.WriteLine($"Options: WaitForData={options.WaitForDataBeforeAllocatingBuffer}, PreferInlineScheduling={options.UnsafePreferInlineScheduling}, IOQueue={options.IOQueueCount}");
        });

        return builder;
    }

    public static IPEndPoint CreateIPEndPoint(this IConfiguration config)
    {
        var url = config["server.urls"] ?? config["urls"];

        if (string.IsNullOrEmpty(url))
        {
            return new IPEndPoint(IPAddress.Loopback, 8080);
        }

        var address = BindingAddress.Parse(url);

        IPAddress ip;

        if (string.Equals(address.Host, "localhost", StringComparison.OrdinalIgnoreCase))
        {
            ip = IPAddress.Loopback;
        }
        else if (!IPAddress.TryParse(address.Host, out ip))
        {
            ip = IPAddress.IPv6Any;
        }

        return new IPEndPoint(ip, address.Port);
    }
}
