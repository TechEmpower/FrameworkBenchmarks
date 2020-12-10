using BeetleX;
using BeetleX.Buffers;
using BeetleX.EventArgs;
using SpanJson;
using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public partial class HttpHandler : ServerHandlerBase
    {
        private static AsciiString _line = new AsciiString("\r\n");

        private static AsciiString _2line = new AsciiString("\r\n\r\n");

        private static AsciiString _httpsuccess = new AsciiString("HTTP/1.1 200 OK\r\n");

        private static readonly AsciiString _headerServer = "Server: TFB\r\n";

        private static readonly AsciiString _headerContentLength = "Content-Length: ";

        private static readonly AsciiString _headerContentLengthZero = "Content-Length: 0\r\n";

        private static readonly AsciiString _headerContentTypeText = "Content-Type: text/plain\r\n";

        private static readonly AsciiString _headerContentTypeHtml = "Content-Type: text/html; charset=UTF-8\r\n";

        private static readonly AsciiString _headerContentTypeJson = "Content-Type: application/json\r\n";

        private static readonly AsciiString _path_Json = "/json";

        private static readonly AsciiString _path_Db = "/db";

        private static readonly AsciiString _path_Queries = "/queries";

        private static readonly AsciiString _path_Plaintext = "/plaintext";

        private static readonly AsciiString _path_Updates = "/updates";

        private static readonly AsciiString _path_Fortunes = "/fortunes";

        private static readonly AsciiString _result_plaintext = "Hello, World!";

        private static readonly AsciiString _cached_worlds = "/cached-worlds";

        private static byte _Space = 32;

        private static byte _question = 63;

        public HttpHandler()
        {

        }

        public Task Default(ReadOnlySpan<byte> url, PipeStream stream, HttpToken token, ISession session)
        {
            stream.Write("<b> beetlex server</b><hr/>");
            stream.Write($"{Encoding.ASCII.GetString(url)} not found!");
            OnCompleted(stream, session, token);
            return Task.CompletedTask;
        }

        public override void Connected(IServer server, ConnectedEventArgs e)
        {
            base.Connected(server, e);
            e.Session.Socket.NoDelay = true;
            var token = new HttpToken();
            token.Db = new RawDb(new ConcurrentRandom(), Npgsql.NpgsqlFactory.Instance);
            e.Session.Tag = token;
        }

        private int AnalysisUrl(ReadOnlySpan<byte> url)
        {
            for (int i = 0; i < url.Length; i++)
            {
                if (url[i] == _question)
                    return i;
            }
            return -1;

        }

        public override void SessionReceive(IServer server, SessionReceiveEventArgs e)
        {
            base.SessionReceive(server, e);
            PipeStream pipeStream = e.Session.Stream.ToPipeStream();
            HttpToken token = (HttpToken)e.Session.Tag;
            var result = pipeStream.IndexOf(_line.Data);
            while (result.End != null)
            {
                if (result.Length == 2)
                {
                    if (token.CurrentRequest != null)
                    {
                        token.Requests.Enqueue(token.CurrentRequest);
                        token.CurrentRequest = null;
                    }
                    pipeStream.ReadFree(result.Length);
                }
                else
                {
                    if (token.CurrentRequest == null)
                    {
                        token.CurrentRequest = new RequestData();
                        var buffer = System.Buffers.ArrayPool<byte>.Shared.Rent(result.Length);
                        pipeStream.Read(buffer, 0, result.Length);
                        token.CurrentRequest.Data = new ArraySegment<byte>(buffer, 0, result.Length);
                    }
                    else
                    {
                        pipeStream.ReadFree(result.Length);
                    }
                }
                if (pipeStream.Length > 0)
                    result = pipeStream.IndexOf(_line.Data);
                else
                    break;
            }
            if (pipeStream.Length == 0 && token.CurrentRequest == null)
            {
                ProcessReqeusts(token, pipeStream, e.Session);
            }
        }

        private async Task ProcessReqeusts(HttpToken token, PipeStream pipeStream, ISession session)
        {
        PROCESS:
            if (token.EnterProcess())
            {
                while (true)
                {
                    if (token.Requests.TryDequeue(out RequestData item))
                    {
                        using (item)
                        {
                            await OnProcess(item, pipeStream, token, session);
                        }
                    }
                    else
                    {
                        break;
                    }
                }
                session.Stream.Flush();
                token.CompletedProcess();
                if (!token.Requests.IsEmpty)
                {
                    goto PROCESS;
                }
            }
        }

        private Task OnProcess(RequestData requestData, PipeStream pipeStream, HttpToken token, ISession sessino)
        {
            var line = _line.AsSpan();
            int len = requestData.Data.Count;
            var receiveData = requestData.GetSpan();
            ReadOnlySpan<byte> http = line;
            ReadOnlySpan<byte> method = line;
            ReadOnlySpan<byte> url = line;
            int offset2 = 0;
            int count = 0;
            for (int i = 0; i < len; i++)
            {
                if (receiveData[i] == line[0])
                {
                    http = receiveData.Slice(offset2, i - offset2);
                    break;
                }
                else
                {
                    if (receiveData[i] == _Space)
                    {
                        if (count != 0)
                        {
                            url = receiveData.Slice(offset2, i - offset2);
                            offset2 = i + 1;
                        }
                        else
                        {
                            method = receiveData.Slice(offset2, i - offset2);
                            offset2 = i + 1;
                            count++;
                        }
                    }
                }
            }
            return OnStartLine(http, method, url, sessino, token, pipeStream);

        }


        public virtual Task OnStartLine(ReadOnlySpan<byte> http, ReadOnlySpan<byte> method, ReadOnlySpan<byte> url, ISession session, HttpToken token, PipeStream stream)
        {

            int queryIndex = AnalysisUrl(url);
            ReadOnlySpan<byte> baseUrl = default;
            ReadOnlySpan<byte> queryString = default;
            if (queryIndex > 0)
            {
                baseUrl = url.Slice(0, queryIndex);
                queryString = url.Slice(queryIndex + 1, url.Length - queryIndex - 1);
            }
            else
            {
                baseUrl = url;
            }
            OnWriteHeader(stream, token);
            if (baseUrl.Length == _path_Plaintext.Length && baseUrl.StartsWith(_path_Plaintext))
            {
                stream.Write(_headerContentTypeText.Data, 0, _headerContentTypeText.Length);
                OnWriteContentLength(stream, token);
                return Plaintext(url, stream, token, session);
            }
            else if (baseUrl.Length == _path_Json.Length && baseUrl.StartsWith(_path_Json))
            {
                stream.Write(_headerContentTypeJson.Data, 0, _headerContentTypeJson.Length);
                OnWriteContentLength(stream, token);
                return Json(stream, token, session);
            }
            else if (baseUrl.Length == _path_Db.Length && baseUrl.StartsWith(_path_Db))
            {
                stream.Write(_headerContentTypeJson.Data, 0, _headerContentTypeJson.Length);
                OnWriteContentLength(stream, token);
                return db(stream, token, session);
            }
            else if (baseUrl.Length == _path_Queries.Length && baseUrl.StartsWith(_path_Queries))
            {
                stream.Write(_headerContentTypeJson.Data, 0, _headerContentTypeJson.Length);
                OnWriteContentLength(stream, token);
                return queries(Encoding.ASCII.GetString(queryString), stream, token, session);
            }

            else if (baseUrl.Length == _cached_worlds.Length && baseUrl.StartsWith(_cached_worlds))
            {
                stream.Write(_headerContentTypeJson.Data, 0, _headerContentTypeJson.Length);
                OnWriteContentLength(stream, token);
                return caching(Encoding.ASCII.GetString(queryString), stream, token, session);
            }

            else if (baseUrl.Length == _path_Updates.Length && baseUrl.StartsWith(_path_Updates))
            {
                stream.Write(_headerContentTypeJson.Data, 0, _headerContentTypeJson.Length);
                OnWriteContentLength(stream, token);
                return updates(Encoding.ASCII.GetString(queryString), stream, token, session);
            }
            else if (baseUrl.Length == _path_Fortunes.Length && baseUrl.StartsWith(_path_Fortunes))
            {
                stream.Write(_headerContentTypeHtml.Data, 0, _headerContentTypeHtml.Length);
                OnWriteContentLength(stream, token);
                return fortunes(stream, token, session);
            }
            else
            {
                stream.Write(_headerContentTypeHtml.Data, 0, _headerContentTypeHtml.Length);
                OnWriteContentLength(stream, token);
                return Default(url, stream, token, session);
            }
        }

        private void OnWriteHeader(PipeStream stream, HttpToken token)
        {
            stream.Write(_httpsuccess.Data, 0, _httpsuccess.Length);
            stream.Write(_headerServer.Data, 0, _headerServer.Length);
            ArraySegment<byte> date = GMTDate.Default.DATE;
            stream.Write(date.Array, date.Offset, date.Count);
        }

        private void OnWriteContentLength(PipeStream stream, HttpToken token)
        {
            stream.Write(_headerContentLength.Data, 0, _headerContentLength.Length);
            token.ContentLength = stream.Allocate(10);
            stream.Write(_2line, 0, 4);
            token.ContentPostion = stream.CacheLength;
        }

        private void OnCompleted(PipeStream stream, ISession session, HttpToken token)
        {
            stream.ReadFree((int)stream.Length);
            token.FullLength((stream.CacheLength - token.ContentPostion).ToString());

        }

    }
}
