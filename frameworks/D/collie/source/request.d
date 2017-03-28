module request;

import collie.codec.http;
import collie.codec.http.server;
import collie.utils.vector;
import std.exception;

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

protected:
	HTTPMessage _header;
    Buffer _buffer;
private:
	HTTPErrorCode _erroCode = HTTPErrorCode.NO_ERROR;
}
