/*
 * Collie - An asynchronous event-driven network framework using Dlang development
 *
 * Copyright (C) 2015-2016  Shanghai Putao Technology Co., Ltd 
 *
 * Developer: putao's Dlang team
 *
 * Licensed under the Apache-2.0 License.
 *
 */
import std.stdio;
import std.experimental.logger;
import std.exception;
import std.typecons;
import std.functional;
import std.parallelism;

import collie.net;
import collie.codec.http;
import collie.codec.http.server;

debug { 
        extern(C) __gshared string[] rt_options = [ "gcopt=profile:1"];// maxPoolSize:50" ];
}



void main()
{
	RequestHandler newHandler(RequestHandler,HTTPMessage msg)
	{
		import get;
		import post;

		if(msg.method == HTTPMethod.HTTP_GET)
			return new GetHandler();
		if(msg.method == HTTPMethod.HTTP_POST)
			return new PostHandler();
		return null;
	}
    
    writeln("Edit source/app.d to start your project.");
    globalLogLevel(LogLevel.warning);
    HTTPServerOptions option = new HTTPServerOptions();
    option.handlerFactories ~= toDelegate(&newHandler);
    option.threads = totalCPUs;
    HttpServer server = new HttpServer(option);

    HTTPServerOptions.IPConfig ipconfig ;
    ipconfig.address = new InternetAddress("0.0.0.0", 8085);

    HTTPServerOptions.IPConfig ipconfig2 ;
    ipconfig2.address = new Internet6Address("0::0", 8085);
   
    server.addBind(ipconfig);
    server.addBind(ipconfig2);
    server.start();
}
