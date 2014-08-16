using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using EvHttpSharp;
using Newtonsoft.Json;

namespace EvHttpSharpBenchmark
{
	class Program
	{
		public static readonly JsonSerializer Serializer = new JsonSerializer();

		static void Main (string[] args)
		{
            LibLocator.Init();
		    var host = new EventHttpMultiworkerListener(Handler, int.Parse(args[2]));
		    host.Start(args[0], ushort.Parse(args[1]));
		}

		private static void Handler(EventHttpRequest req)
		{
			var headers = new Dictionary<string, string>();
			var resp = "Hello, World!";

			if (!req.Uri.Contains("plaintext"))
			{
				var sw = new StringWriter();
				Serializer.Serialize(sw, new {message = "Hello, World!"});
				resp = sw.ToString();
				headers["Content-Type"] = "application/json";
			}
			req.Respond (HttpStatusCode.OK, headers, Encoding.UTF8.GetBytes (resp));
		}
	}
}
