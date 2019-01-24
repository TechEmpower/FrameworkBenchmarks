/*
 * Collie - An asynchronous event-driven network framework using Dlang development
 *
 * Copyright (C) 2015-2018  Shanghai Putao Technology Co., Ltd 
 *
 * Developer: Putao's Dlang team
 *
 * Licensed under the Apache-2.0 License.
 *
 */
import std.getopt;
import std.stdio;

import hunt.io;
import hunt.system.Memory : totalCPUs;
import http.Processor;
import http.Server;
import DemoProcessor;

void main(string[] args) {

	ushort port = 8080;
	GetoptResult o = getopt(args, "port|p", "Port (default 8080)", &port);
	if (o.helpWanted) {
		defaultGetoptPrinter("A simple http server powered by Hunt!", o.options);
		return;
	}

	HttpServer httpServer = new HttpServer("0.0.0.0", port, totalCPUs-1);
	httpServer.onProcessorCreate(delegate HttpProcessor (TcpStream client) {
		return new DemoProcessor(client);
	});

	writefln("listening on http://%s", httpServer.bindingAddress.toString());
	httpServer.start();
}