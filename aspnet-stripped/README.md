This is a heavily stripped-down version of [pdonald](https://github.com/pdonald/)'s aspnet tests.
Right now this is designed to run on Windows against SQL Server on Windows.

For CPU-bound tests such as db, json, fortunes, and plaintext, this is approximately twice as fast as the non-stripped aspnet tests. The performance is obtained by using ASP.NET IHttpHandlers, old-style .aspx files (when HTML is output) and removing unnecessary IIS/ASP.NET modules, especially ones related to URL routing and especially ones written in .NET.

To replace ASP.NET's URL routing, we directly specify the path to IHttpHandlers in web.config. The idea is that native code in IIS probably (definitely?) does the routing for handlers specified this way, so the routing should be extremely fast, as opposed to ASP.NET's System.Web.Routing code that is extremely configurable. To route to an .aspx page, we use NoAspxHandlerFactory, a small piece of 'middleware' that can directly route from this web.config setting to a .aspx file.

This is stripped down so much that I'm not sure if anyone would actually want to use these techniques in production. The point of this is the following:

1. To provide a better comparison against microframeworks/microplatforms that barely do anything.
2. To give us a sense of the upperbound of performance for more realistic tests.