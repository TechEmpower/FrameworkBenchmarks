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

import hunt.database;
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

version(POSTGRESQL) {
	debug {
		dbConnection = new Database("postgresql://benchmarkdbuser:benchmarkdbpass@10.1.11.44:5432/hello_world?charset=utf-8");
		dbConnection.getOption().setMinimumConnection(128);
	} else {
		dbConnection = new Database("postgresql://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?charset=utf-8");
		dbConnection.getOption().setMinimumConnection(128);
	}
		dbConnection.getOption().setMaximumConnection(128);
}
	HttpServer httpServer = new HttpServer("0.0.0.0", port, totalCPUs);
	httpServer.onProcessorCreate(delegate HttpProcessor (TcpStream client) {
		return new DemoProcessor(client);
	});

	writefln("listening on http://%s", httpServer.bindingAddress.toString());
	httpServer.start();
}