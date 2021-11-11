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

	version (POSTGRESQL) {
		DatabaseOption options;
		debug {
			options = new DatabaseOption(
					"postgresql://benchmarkdbuser:benchmarkdbpass@10.1.11.44:5432/hello_world?charset=utf-8");
		} else {
			options = new DatabaseOption(
					"postgresql://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?charset=utf-8");
		}
		
		options.setMinimumConnection(totalCPUs*3);
		options.setMaximumConnection(totalCPUs*3);
		dbConnection = new Database(options);
	}

	AbstractTcpServer httpServer = new HttpServer!(DemoProcessor)("0.0.0.0", port, totalCPUs);
	writefln("listening on http://%s", httpServer.bindingAddress.toString());
	httpServer.start();
}
