using BeetleX.Light;
using BeetleX.Light.Memory;
using System;
using System.Buffers;
using System.Collections.Generic;
using System.IO;
using System.IO.Pipelines;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{




    public partial class HttpHandler : SesionBase
    {
        private static readonly AsciiString _line = new AsciiString("\r\n");

        private static readonly AsciiString _2line = new AsciiString("\r\n\r\n");

        private static readonly AsciiString _httpsuccess = new AsciiString("HTTP/1.1 200 OK\r\n");

        private static readonly AsciiString _headerServer = "Server: B\r\n";

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

        private readonly static uint _jsonPayloadSize = (uint)System.Text.Json.JsonSerializer.SerializeToUtf8Bytes(new JsonMessage { message = "Hello, World!" }).Length;



        private readonly static AsciiString _jsonPreamble =
            _httpsuccess
            + _headerContentTypeJson
            + _headerServer
            + _headerContentLength + _jsonPayloadSize.ToString() + _line;

        private readonly static AsciiString _plaintextPreamble =
              _httpsuccess
              + _headerContentTypeText
              + _headerServer
              + _headerContentLength + _result_plaintext.Length.ToString() + _line;


        private readonly static AsciiString _jsonResultPreamble =
        _httpsuccess
        + _headerContentTypeJson
        + _headerServer;

        private readonly static AsciiString _HtmlResultPreamble =
      _httpsuccess
      + _headerContentTypeHtml
      + _headerServer;




        private static byte _Space = 32;

        public const int _LengthSize = 8;

        private static byte _question = 63;

        struct ContentLengthMemory
        {
            public Memory<byte> Data { get; set; }

            public void Full(int length)
            {
                _headerContentLength.Data.CopyTo(Data);
                var span = Data.Slice(_headerContentLength.Length).Span;
                var len = span.Write(length.ToString(), Encoding.ASCII);
                for (int i = len; i < span.Length - 2; i++)
                {
                    span[i] = 32;
                }
                span[^2] = 13;
                span[^1] = 10;
            }
        }


        protected Memory<byte> GetContentLengthMemory(IStreamWriter writer)
        {
            var result = writer.WriteSequenceNetStream.GetWriteMemory(28);
            writer.WriteSequenceNetStream.WriteAdvance(28);
            return result;
        }

        public NetContext Context { get; set; }

        public HttpHandler()
        {

        }

        private Queue<RequestData> _Requests = new Queue<RequestData>();

        private RawDb _db;

        private RequestData _ReadRequest = null;
        public override void Connected(NetContext context)
        {
            base.Connected(context);
            this.Context = context;
            _db = new RawDb(new ConcurrentRandom(), Npgsql.NpgsqlFactory.Instance); ;
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

        public override void Receive(NetContext context, object message)
        {
            var stream = context.Reader.ReadSequenceNetStream;
            var reader = stream.GetReadOnlySequence();
            var len = reader.IndexOf(_line);
            while (len != null)
            {
                var lendata = len.Value;
                stream.ReadAdvance(lendata.Length);
                if (lendata.Length == 2)
                {
                    _Requests.Enqueue(_ReadRequest);
                    _ReadRequest = null;
                }
                else
                {
                    if (_ReadRequest == null)
                    {
                        _ReadRequest = new RequestData();
                    }
                    if (_ReadRequest.Action == null)
                    {
                        AnalysisAction(lendata, out var type, out var querystring);
                        _ReadRequest.Action = type;
                        _ReadRequest.QueryString = querystring;

                    }
                    else
                    {

                    }
                }
                reader = stream.GetReadOnlySequence();
                len = reader.IndexOf(_line);
            }
            if (_Requests.Count > 0)
            {
                OnStartRequest(context.Writer);
            }
        }

        //public override void SessionReceive(IServer server, SessionReceiveEventArgs e)
        //{
        //    base.SessionReceive(server, e);
        //    PipeStream pipeStream = e.Session.Stream.ToPipeStream();
        //    HttpToken token = (HttpToken)e.Session.Tag;
        //    var result = pipeStream.IndexOfLine();
        //    while (result.End != null)
        //    {
        //        if (result.Length == 2)
        //        {
        //            pipeStream.ReadFree(result.Length);
        //            OnStartRequest(token.CurrentRequest, e.Session, token, pipeStream);
        //        }
        //        else
        //        {
        //            if (token.CurrentRequest == null)
        //            {
        //                var request = new RequestData();
        //                byte[] buffer = null;
        //                buffer = new byte[result.Length];
        //                pipeStream.Read(buffer, 0, result.Length);
        //                request.Data = new ArraySegment<byte>(buffer, 0, result.Length);
        //                AnalysisAction(request);
        //                if (request.Action == ActionType.Plaintext)
        //                {
        //                    token.CurrentRequest = request;
        //                }
        //                else
        //                {
        //                    token.CurrentRequest = request;
        //                    pipeStream.ReadFree((int)pipeStream.Length);
        //                    OnStartRequest(request, e.Session, token, pipeStream);
        //                    return;
        //                }
        //            }
        //            else
        //            {
        //                pipeStream.ReadFree(result.Length);
        //            }
        //        }
        //        if (pipeStream.Length > 0)
        //            result = pipeStream.IndexOfLine();
        //        else
        //            break;
        //    }
        //}

        private void AnalysisAction(ReadOnlySequence<byte> line, out ActionType type, out string queryString)
        {
            type = ActionType.Plaintext;
            queryString = default;
            var spanIndex = line.PositionOf((byte)32);
            var postion = line.GetPosition(1, spanIndex.Value);
            line = line.Slice(postion);
            spanIndex = line.PositionOf((byte)32);
            var url = line.Slice(0, spanIndex.Value);
            int baseurlLen = 0;
            spanIndex = url.PositionOf((byte)63);
            if (spanIndex != null)
            {
                baseurlLen = (int)url.Slice(0, spanIndex.Value).Length;
                queryString = url.Slice(baseurlLen + 1).ReadString(Encoding.ASCII);
            }
            else
            {
                baseurlLen = (int)url.Length;
            }

            Span<byte> baseUrl = stackalloc byte[baseurlLen];
            url.Slice(0, baseurlLen).CopyTo(baseUrl);

            if (baseUrl.Length == _path_Plaintext.Length && baseUrl.StartsWith(_path_Plaintext))
            {
                type = ActionType.Plaintext;
            }
            else if (baseUrl.Length == _path_Json.Length && baseUrl.StartsWith(_path_Json))
            {
                type = ActionType.Json;
            }
            else if (baseUrl.Length == _path_Db.Length && baseUrl.StartsWith(_path_Db))
            {
                type = ActionType.Db;
            }
            else if (baseUrl.Length == _path_Queries.Length && baseUrl.StartsWith(_path_Queries))
            {
                type = ActionType.Queries;
            }

            else if (baseUrl.Length == _cached_worlds.Length && baseUrl.StartsWith(_cached_worlds))
            {
                type = ActionType.Caching;
            }

            else if (baseUrl.Length == _path_Updates.Length && baseUrl.StartsWith(_path_Updates))
            {
                type = ActionType.Updates;
            }
            else if (baseUrl.Length == _path_Fortunes.Length && baseUrl.StartsWith(_path_Fortunes))
            {
                type = ActionType.Fortunes;
            }
            else
            {
                type = ActionType.Other;
            }
        }

        public async Task OnStartRequest(IStreamWriter stream)
        {
            bool haveData = false;
            while (_Requests.Count > 0)
            {
                haveData = true;
                var data = _Requests.Dequeue();
                ActionType type = data.Action.Value;
                if (type == ActionType.Plaintext)
                {
                    Plaintext(stream);
                }
                else if (type == ActionType.Json)
                {
                    Json(stream);
                }
                else if (type == ActionType.Db)
                {
                    await db(stream);
                }
                else if (type == ActionType.Queries)
                {
                    await queries(data.QueryString, stream);
                }
                else if (type == ActionType.Caching)
                {
                    await caching(data.QueryString, stream);
                }
                else if (type == ActionType.Updates)
                {
                    await updates(data.QueryString, stream);
                }
                else if (type == ActionType.Fortunes)
                {
                    await fortunes(stream);
                }
                else
                {
                    await Default(stream);
                }
            }
            if (haveData)
            {
                stream.Flush();
            }
        }




        private Utf8JsonWriter GetJsonWriter(IStreamWriter stream)
        {
            Utf8JsonWriter utf8JsonWriter = _utf8JsonWriter ??= new Utf8JsonWriter((Stream)stream.WriteSequenceNetStream, new JsonWriterOptions { SkipValidation = true });
            utf8JsonWriter.Reset((Stream)stream.WriteSequenceNetStream);
            return utf8JsonWriter;
        }
        [ThreadStatic]
        private static Utf8JsonWriter _utf8JsonWriter;

        public static JsonWriterOptions _jsonWriterOptions = new JsonWriterOptions
        {
            SkipValidation = true
        };
    }
}
