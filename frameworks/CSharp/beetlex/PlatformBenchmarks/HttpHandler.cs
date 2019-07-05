using BeetleX;
using BeetleX.Buffers;
using BeetleX.EventArgs;
using SpanJson;
using System;
using System.Collections.Generic;
using System.Text;

namespace PlatformBenchmarks
{
    public partial class HttpHandler : ServerHandlerBase
    {
        private static AsciiString _line = new AsciiString("\r\n");

        private static AsciiString _2line = new AsciiString("\r\n\r\n");

        private static AsciiString _httpsuccess = new AsciiString("HTTP/1.1 200 OK\r\n");

        private static readonly AsciiString _headerServer = "Server: Beetlex\r\n";

        private static readonly AsciiString _headerContentLength = "Content-Length: ";

        private static readonly AsciiString _headerContentLengthZero = "Content-Length: 0\r\n";

        private static readonly AsciiString _headerContentTypeText = "Content-Type: text/plain\r\n";

        private static readonly AsciiString _headerContentTypeHtml = "Content-Type: text/html; charset=UTF-8\r\n";

        private static readonly AsciiString _headerContentTypeJson = "Content-Type: application/json\r\n";

        private static readonly AsciiString _path_Json = "/json";

        private static readonly AsciiString _path_Db = "/db";

        private static readonly AsciiString _path_Queries = "/queries";

        private static readonly AsciiString _path_Plaintext = "/plaintext";

        private static readonly AsciiString _path_Fortunes = "/fortunes";

        private static readonly AsciiString _result_plaintext = "Hello, World!";

        private static byte _Space = 32;

        private static byte _question = 63;

        private RawDb mPgsql;

        public HttpHandler()
        {

            mPgsql = new RawDb(new ConcurrentRandom(), Npgsql.NpgsqlFactory.Instance);
        }

        public void Default(ReadOnlySpan<byte> url, PipeStream stream, HttpToken token, ISession session)
        {
            stream.Write("<b> beetlex server</b><hr/>");
            stream.Write($"{Encoding.ASCII.GetString(url)} not found!");
            OnCompleted(stream, session, token);
        }

        public override void Connected(IServer server, ConnectedEventArgs e)
        {
            base.Connected(server, e);
            e.Session.Tag = new HttpToken();
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
            if(result.End ==null)
            {
                return;
            }
            int len = result.Length;
            pipeStream.Read(token.Buffer, 0, len);
            ReadOnlySpan<byte> line = new Span<byte>(token.Buffer, 0, len);
            ReadOnlySpan<byte> http = line;
            ReadOnlySpan<byte> method = line;
            ReadOnlySpan<byte> url = line;
            int offset2 = 0;
            int count = 0;
            for (int i = 0; i < line.Length; i++)
            {
                if (line[i] == _Space)
                {
                    if (count != 0)
                    {
                        url = line.Slice(offset2, i - offset2);
                        offset2 = i + 1;
                        http = line.Slice(offset2, line.Length - offset2 - 2);
                        break;
                    }
                    method = line.Slice(offset2, i - offset2);
                    offset2 = i + 1;
                    count++;
                }
            }
            OnStartLine(http, method, url, e.Session, token, pipeStream);
        }



        protected virtual void OnStartLine(ReadOnlySpan<byte> http, ReadOnlySpan<byte> method, ReadOnlySpan<byte> url, ISession session, HttpToken token, PipeStream stream)
        {
            int queryIndex = AnalysisUrl(url);
            ReadOnlySpan<byte> baseUrl=default;
            ReadOnlySpan<byte> queryString = default;
            if (queryIndex>0)
            {
                baseUrl = url.Slice(0, queryIndex);
                queryString = url.Slice(queryIndex+1, url.Length - queryIndex - 1);
            }
            else
            {
                baseUrl = url;
            }
            OnWriteHeader(stream, token);         
            if (baseUrl.Length== _path_Plaintext.Length && baseUrl.StartsWith(_path_Plaintext))
            {
                stream.Write(_headerContentTypeText.Data, 0, _headerContentTypeText.Length);
                OnWriteContentLength(stream, token);
                Plaintext(url, stream, token, session);
            }
            else if (baseUrl.Length==_path_Json.Length && baseUrl.StartsWith(_path_Json))
            {
                stream.Write(_headerContentTypeJson.Data, 0, _headerContentTypeJson.Length);
                OnWriteContentLength(stream, token);
                Json(url, stream, token, session);
            }
            else if(baseUrl.Length == _path_Db.Length && baseUrl.StartsWith(_path_Db))
            {
                stream.Write(_headerContentTypeJson.Data, 0, _headerContentTypeJson.Length);
                OnWriteContentLength(stream, token);
                db(stream, token, session);
            }
            else if(baseUrl.Length == _path_Queries.Length && baseUrl.StartsWith(_path_Queries))
            {
                stream.Write(_headerContentTypeJson.Data, 0, _headerContentTypeJson.Length);
                OnWriteContentLength(stream, token);
                queries(Encoding.ASCII.GetString(queryString), stream, token, session);
            }
            else if(baseUrl.Length == _path_Fortunes.Length && baseUrl.StartsWith(_path_Fortunes))
            {
                stream.Write(_headerContentTypeHtml.Data, 0, _headerContentTypeHtml.Length);
                OnWriteContentLength(stream, token);
                fortunes(stream, token, session);
            }
            else
            {
                stream.Write(_headerContentTypeHtml.Data, 0, _headerContentTypeHtml.Length);
                OnWriteContentLength(stream, token);
                Default(url, stream, token, session);
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
            session.Stream.Flush();
        }




    }
}
