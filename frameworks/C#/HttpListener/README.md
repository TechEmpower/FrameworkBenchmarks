This uses [.NET's HttpListener class](http://msdn.microsoft.com/en-us/library/system.net.httplistener.aspx) which is somewhat similar to Go's net/http package or node.js's built-in HTTP server. HttpListener is a .NET Framework wrapper around [HTTP.SYS](http://www.iis.net/learn/get-started/introduction-to-iis/introduction-to-iis-architecture#Hypertext), the kernel-mode HTTP driver that is used by IIS.

These tests are based on [aspnet](https://github.com/TechEmpower/FrameworkBenchmarks/tree/master/aspnet) by [@pdonald](https://github.com/pdonald/).

By using .NET's HttpListener class, we eliminate the overhead of IIS and ASP.NET. This does not use ASP.NET WebForms (.aspx), but instead uses Razor (.cshtml), with the the template parsed at design-time into C# by RazorGenerator.