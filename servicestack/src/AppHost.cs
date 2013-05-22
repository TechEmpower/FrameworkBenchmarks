using System;
using System.Linq;
using System.Configuration;
using System.Collections.Generic;
using ServiceStack.Common.Web;
using ServiceStack.Configuration;
using ServiceStack.OrmLite;
using ServiceStack.ServiceInterface;
using ServiceStack.ServiceInterface.Auth;
using ServiceStack.ServiceInterface.ServiceModel;
using ServiceStack.WebHost.Endpoints;

namespace ServiceStackBenchmark
{
	public class AppHost
		: AppHostBase
	{		
		public AppHost() //Tell ServiceStack the name and where to find your web services
            : base("ServiceStackBenchmark", typeof(JsonService).Assembly) { }

		public override void Configure(Funq.Container container)
		{
			ServiceStack.Text.JsConfig.EmitCamelCaseNames = true;

            SetConfig(new EndpointHostConfig
            {
                DefaultContentType = ContentType.Json
            });

			//Register all your dependencies
			//container.Register(new TodoRepository());
		}

		public static void Start()
		{
			new AppHost().Init();
		}
	}
}
