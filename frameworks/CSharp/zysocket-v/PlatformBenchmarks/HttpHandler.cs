using System;
using System.Buffers;
using System.Text;
using System.Threading.Tasks;
using ZYSocket;
using ZYSocket.FiberStream;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {
        private static AsciiString _line = new AsciiString("\r\n");

        private static AsciiString _2line = new AsciiString("\r\n\r\n");

        private static AsciiString _httpsuccess = new AsciiString("HTTP/1.1 200 OK\r\n");

        private static readonly AsciiString _headerServer = "Server: zysocket\r\n";

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

        private static readonly byte[] LenData = new byte[10] { 32, 32, 32, 32, 32, 32, 32, 32, 32, 32 };

        private static byte _Space = 32;

        private static byte _question = 63;


        public HttpHandler()
        {

        }

        public void Default(IFiberRw<HttpToken> fiberRw,ref WriteBytes write)
        {
            write.Write("<b> zysocket server</b><hr/>");         
            write.Write($"error not found!");

            var length = write.Stream.Length - fiberRw.UserToken.HttpHandlerPostion;
            write.Stream.Position = fiberRw.UserToken.ContentPostion.postion;
            write.Write(length.ToString(), false);
            write.Flush();
        }

        private async Task OnCompleted(IFiberRw<HttpToken> fiberRw, WriteBytes write)
        {
            Task<int> WSend()
            {
                var length = write.Stream.Length - fiberRw.UserToken.HttpHandlerPostion;
                write.Stream.Position = fiberRw.UserToken.ContentPostion.postion;
                write.Write(length.ToString(), false);
                write.Flush(false);
                return fiberRw.Flush();
            }

            if (fiberRw.UserToken != null)
                await await fiberRw.Sync.Ask(WSend);
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

        public async Task Receive(IFiberRw<HttpToken> fiberRw, Memory<byte> memory_r, WriteBytes write)
        {
            var data = (await fiberRw.ReadLine(memory_r));
            ReadHander(fiberRw,ref data,ref write);
            fiberRw.StreamReadFormat.Position = fiberRw.StreamReadFormat.Length;
        }


        private void ReadHander(IFiberRw<HttpToken> fiberRw,ref Memory<byte> linedata,ref WriteBytes write)
        {
            
            var token = fiberRw.UserToken;
            ReadOnlySpan<byte> line = linedata.Span;
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
                        break;
                    }                  
                    offset2 = i + 1;
                    count++;
                }
            }

          
            int queryIndex = AnalysisUrl(url);
            ReadOnlySpan<byte> queryString = default;
            ReadOnlySpan<byte> baseUrl;
            if (queryIndex > 0)
            {
                baseUrl = url.Slice(0, queryIndex);
                queryString = url.Slice(queryIndex + 1, url.Length - queryIndex - 1);
            }
            else
            {
                baseUrl = url;
            }
            OnWriteHeader(ref write);

            if (baseUrl.Length == _path_Plaintext.Length && baseUrl.StartsWith(_path_Plaintext))
            {
                write.Write(_headerContentTypeText.Data, 0, _headerContentTypeText.Length);
                OnWriteContentLength(write, token);
                Plaintext(fiberRw, write);
            }
            else if (baseUrl.Length == _path_Json.Length && baseUrl.StartsWith(_path_Json))
            {
                write.Write(_headerContentTypeJson.Data, 0, _headerContentTypeJson.Length);
                OnWriteContentLength(write, token);
                Json(fiberRw, write);
            }
            else if (baseUrl.Length == _path_Db.Length && baseUrl.StartsWith(_path_Db))
            {
                write.Write(_headerContentTypeJson.Data, 0, _headerContentTypeJson.Length);
                OnWriteContentLength(write, token);
                db(fiberRw, write);
            }
            else if (baseUrl.Length == _path_Queries.Length && baseUrl.StartsWith(_path_Queries))
            {
                write.Write(_headerContentTypeJson.Data, 0, _headerContentTypeJson.Length);
                OnWriteContentLength(write, token);
                queries(Encoding.ASCII.GetString(queryString),fiberRw, write);
            }
            else if (baseUrl.Length == _path_Fortunes.Length && baseUrl.StartsWith(_path_Fortunes))
            {
                write.Write(_headerContentTypeHtml.Data, 0, _headerContentTypeHtml.Length);
                OnWriteContentLength(write, token);
                fortunes(fiberRw, write);
            }
            else
            {
                write.Write(_headerContentTypeHtml.Data, 0, _headerContentTypeHtml.Length);
                OnWriteContentLength(write, token);
                Default( fiberRw, ref write);
            }

        }


        private void OnWriteHeader(ref WriteBytes write)
        {
            write.Write(_httpsuccess.Data, 0, _httpsuccess.Length);
            write.Write(_headerServer.Data, 0, _headerServer.Length);
            ArraySegment<byte> date = GMTDate.Default.DATE;
            write.Write(date.Array, date.Offset, date.Count);
        }



        private void OnWriteContentLength(WriteBytes write, HttpToken token)
        {
            write.Write(_headerContentLength.Data, 0, _headerContentLength.Length);
            token.ContentPostion = write.Allocate(LenData);
            write.Write(_2line, 0, 4);
            token.HttpHandlerPostion = (int)write.Stream.Position;
        }

       



    }
}
