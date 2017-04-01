/*
 * Hunt - a framework for web and console application based on Collie using Dlang development
 *
 * Copyright (C) 2015-2016  Shanghai Putao Technology Co., Ltd 
 *
 * Developer: putao's Dlang team
 *
 * Licensed under the BSD License.
 *
 */
module app.controller.index;
import hunt.application;
import std.experimental.logger;

version(USE_ENTITY) import app.model.index;

class IndexController : Controller
{
	mixin MakeController;
    this()
    {
    }
	Response res(){return request.createResponse();}
	@Action void json()
	{
		import std.json;
		JSONValue js = JSONValue([
			"message" : "Hello, World!"		
		]);
		res.json(js);
	}

	@Action void plaintext()
	{
		res.html("Hello, World!");
	}
}
