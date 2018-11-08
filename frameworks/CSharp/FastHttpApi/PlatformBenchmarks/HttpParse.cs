using System;
using System.Collections.Generic;
using System.Text;

namespace PlatformBenchmarks
{
    public class HttpParse
    {
        static HttpParse()
        {
            LineBytes = Encoding.UTF8.GetBytes("\r\n");

            NotFoundBytes = Encoding.UTF8.GetBytes("HTTP/1.1 404 not found\r\n\r\n");

            StringBuilder sb = new StringBuilder();
            sb.AppendLine("HTTP/1.1 200 OK");
            sb.AppendLine("Content-Length: 14");
            sb.AppendLine("Date: " + DateTime.Now.ToUniversalTime().ToString("r"));
            sb.AppendLine("");
            sb.Append("BeetleX Server");
            BeetleXServerBytes = Encoding.UTF8.GetBytes(sb.ToString());
            sb.Clear();

            //PlaintextBytes
            string helloword = "Hello, World!";
            sb.AppendLine("HTTP/1.1 200 OK");
            sb.AppendLine("Content-Length: " + helloword.Length);
            sb.AppendLine("Date: " + DateTime.Now.ToUniversalTime().ToString("r"));
            sb.AppendLine("");
            sb.Append(helloword);
            PlaintextBytes = Encoding.UTF8.GetBytes(sb.ToString());
            sb.Clear();

            //json

            helloword = Newtonsoft.Json.JsonConvert.SerializeObject(new JsonMessage { message = "Hello, World!" });
            sb.AppendLine("HTTP/1.1 200 OK");
            sb.AppendLine("Content-Length: " + helloword.Length);
            sb.AppendLine("Date: " + DateTime.Now.ToUniversalTime().ToString("r"));
            sb.AppendLine("");
            sb.Append(helloword);
            JsonBytes = Encoding.UTF8.GetBytes(sb.ToString());
            sb.Clear();
        }

        public static void Plaintext(BeetleX.Buffers.PipeStream stream)
        {
            stream.Write(PlaintextBytes, 0, PlaintextBytes.Length);
        }

        public static void Json(BeetleX.Buffers.PipeStream stream)
        {
            stream.Write(JsonBytes, 0, JsonBytes.Length);
        }
        public class JsonMessage
        {
            public string message { get; set; }
        }

        public static byte[] LineBytes;

        public static byte[] NotFoundBytes;

        public static byte[] PlaintextBytes;

        public static byte[] JsonBytes;

        public static byte[] BeetleXServerBytes;

        public static Tuple<string, string, string> AnalyzeRequestLine(ReadOnlySpan<char> line)
        {
            string v1 = null, v2 = null, v3 = null;
            int offset = 0;
            int count = 0;
            for (int i = 0; i < line.Length; i++)
            {
                if (line[i] == ' ')
                {
                    if (count == 0)
                    {
                        v1 = new string(line.Slice(offset, i - offset));
                        offset = i + 1;
                    }
                    else
                    {
                        v2 = new string(line.Slice(offset, i - offset));
                        offset = i + 1;
                        v3 = new string(line.Slice(offset, line.Length - offset));
                        break;
                    }
                    count++;
                }
            }
            return new Tuple<string, string, string>(v1, v2, v3);
        }
    }
}
