module request;

import collie.codec.http;
import collie.codec.http.server;
import std.exception;
import std.datetime;
import std.conv;
import std.string;
import kiss.container.Vector;

abstract class BaseHandler : RequestHandler
{
    alias Buffer = Vector!(ubyte);
protected:
	final override void onResquest(HTTPMessage headers) nothrow
	{
		_header = headers;
	}

	final override void onBody(const ubyte[] data) nothrow
	{
        collectException(_buffer.put(data));
	}

	final override void onError(HTTPErrorCode code) nothrow {
		_erroCode = code;
		collectException({
				import collie.utils.memory;
				if(_header)gcFree(_header);
				gcFree(this);
			}());
	}

	final override void requestComplete() nothrow
	{
		_erroCode = HTTPErrorCode.REMOTE_CLOSED;
		collectException({
				import collie.utils.memory;
				if(_header)gcFree(_header);
				gcFree(this);
			}());
	}

	final @property bool isVaild(){return _erroCode == HTTPErrorCode.NO_ERROR;}

	pragma(inline)
	final string printDate() {
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

protected:
	HTTPMessage _header;
    Buffer _buffer;
private:
	HTTPErrorCode _erroCode = HTTPErrorCode.NO_ERROR;
}
