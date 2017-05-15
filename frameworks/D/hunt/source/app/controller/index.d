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
import std.exception;
import std.datetime;
import std.conv;
import std.string;
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
		res.setHeader("Date",printDate);
		res.json(js);
	}

	@Action void plaintext()
	{
		res.setHeader("Date",printDate);
		res.html("Hello, World!");
	}

	private string printDate() {
		DateTime date = cast(DateTime)Clock.currTime;
		return format(
				"%.3s, %02d %.3s %d %02d:%02d:%02d GMT", // could be UTC too
				to!string(date.dayOfWeek).capitalize,
				date.day,
				to!string(date.month).capitalize,
				date.year,
				date.hour,
				date.minute,
				date.second);
	}
}
