﻿using BeetleX;
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
            RequestDispatchs = new BeetleX.Dispatchs.DispatchCenter<HttpToken>(OnRequest, Math.Min(Environment.ProcessorCount, 16));
        }

        private BeetleX.Dispatchs.DispatchCenter<HttpToken> RequestDispatchs;

        public Task Default(PipeStream stream, HttpToken token, ISession session)
        {
            stream.Write("<b> beetlex server</b><hr/>");
            stream.Write("path not found!");
            OnCompleted(stream, session, token);
            return Task.CompletedTask;
        }

        public override void Connected(IServer server, ConnectedEventArgs e)
        {
            base.Connected(server, e);
            e.Session.Socket.NoDelay = true;
            var token = new HttpToken();
            token.ThreadDispatcher = RequestDispatchs.Next();
            token.Session = e.Session;
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

        private void OnRequest(HttpToken token)
        {
            if (token.Requests.TryDequeue(out RequestData result))
            {
                OnStartRequest(result, token.Session, token, token.Session.Stream.ToPipeStream());
            }
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
                        token.ThreadDispatcher.Enqueue(token);
                    }
                    pipeStream.ReadFree(result.Length);
                }
                else
                {
                    if (token.CurrentRequest == null)
                    {
                        var request = new RequestData();

                        byte[] buffer = null;
                        if (Program.Debug)
                            buffer = new byte[result.Length];
                        else
                            buffer = System.Buffers.ArrayPool<byte>.Shared.Rent(result.Length);
                        pipeStream.Read(buffer, 0, result.Length);
                        request.Data = new ArraySegment<byte>(buffer, 0, result.Length);
                        AnalysisAction(request);
                        if (request.Action == ActionType.Plaintext)
                        {
                            token.CurrentRequest = request;
                        }
                        else
                        {
                            pipeStream.ReadFree((int)pipeStream.Length);
                            OnStartRequest(request, e.Session, token, pipeStream);
                            return;
                        }
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
        }

        private void AnalysisAction(RequestData requestData)
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
            int queryIndex = AnalysisUrl(url);
            ReadOnlySpan<byte> baseUrl = default;
            ReadOnlySpan<byte> queryString = default;
            if (queryIndex > 0)
            {
                baseUrl = url.Slice(0, queryIndex);
                queryString = url.Slice(queryIndex + 1, url.Length - queryIndex - 1);
                requestData.QueryString = Encoding.ASCII.GetString(queryString);
            }
            else
            {
                baseUrl = url;
            }
            if (baseUrl.Length == _path_Plaintext.Length && baseUrl.StartsWith(_path_Plaintext))
            {
                requestData.Action = ActionType.Plaintext;
            }
            else if (baseUrl.Length == _path_Json.Length && baseUrl.StartsWith(_path_Json))
            {
                requestData.Action = ActionType.Json;
            }
            else if (baseUrl.Length == _path_Db.Length && baseUrl.StartsWith(_path_Db))
            {
                requestData.Action = ActionType.Db;
            }
            else if (baseUrl.Length == _path_Queries.Length && baseUrl.StartsWith(_path_Queries))
            {
                requestData.Action = ActionType.Queries;
            }

            else if (baseUrl.Length == _cached_worlds.Length && baseUrl.StartsWith(_cached_worlds))
            {
                requestData.Action = ActionType.Caching;
            }

            else if (baseUrl.Length == _path_Updates.Length && baseUrl.StartsWith(_path_Updates))
            {
                requestData.Action = ActionType.Updates;
            }
            else if (baseUrl.Length == _path_Fortunes.Length && baseUrl.StartsWith(_path_Fortunes))
            {
                requestData.Action = ActionType.Fortunes;
            }
            else
            {
                requestData.Action = ActionType.Other;
            }
        }

        public virtual async Task OnStartRequest(RequestData data, ISession session, HttpToken token, PipeStream stream)
        {
            OnWriteHeader(stream, token);
            ActionType type = data.Action;
            if (type == ActionType.Plaintext)
            {
                stream.Write(_headerContentTypeText.Data, 0, _headerContentTypeText.Length);
                OnWriteContentLength(stream, token);
                await Plaintext(stream, token, session);
            }
            else if (type == ActionType.Json)
            {
                stream.Write(_headerContentTypeJson.Data, 0, _headerContentTypeJson.Length);
                OnWriteContentLength(stream, token);
                await Json(stream, token, session);
            }
            else if (type == ActionType.Db)
            {
                stream.Write(_headerContentTypeJson.Data, 0, _headerContentTypeJson.Length);
                OnWriteContentLength(stream, token);
                await db(stream, token, session);
            }
            else if (type == ActionType.Queries)
            {
                stream.Write(_headerContentTypeJson.Data, 0, _headerContentTypeJson.Length);
                OnWriteContentLength(stream, token);
                await queries(data.QueryString, stream, token, session);
            }

            else if (type == ActionType.Caching)
            {
                stream.Write(_headerContentTypeJson.Data, 0, _headerContentTypeJson.Length);
                OnWriteContentLength(stream, token);
                await caching(data.QueryString, stream, token, session);
            }

            else if (type == ActionType.Updates)
            {
                stream.Write(_headerContentTypeJson.Data, 0, _headerContentTypeJson.Length);
                OnWriteContentLength(stream, token);
                await updates(data.QueryString, stream, token, session);
            }
            else if (type == ActionType.Fortunes)
            {
                stream.Write(_headerContentTypeHtml.Data, 0, _headerContentTypeHtml.Length);
                OnWriteContentLength(stream, token);
                await fortunes(stream, token, session);
            }
            else
            {
                stream.Write(_headerContentTypeHtml.Data, 0, _headerContentTypeHtml.Length);
                OnWriteContentLength(stream, token);
                await Default(stream, token, session);
            }
            if (!Program.Debug)
                data.Dispose();

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

            token.FullLength((stream.CacheLength - token.ContentPostion).ToString());
            if (token.Requests.IsEmpty && stream.Length == 0)
                session.Stream.Flush();
        }

    }
}
