using BeetleX;
using BeetleX.Buffers;
using BeetleX.Dispatchs;
using System;
using System.Collections.Concurrent;
using System.Text;
using System.Text.Json;

namespace PlatformBenchmarks
{
    public class HttpToken
    {
        private byte[] mLengthBuffer = new byte[10];

        public RawDb Db { get; set; }

        public HttpToken()
        {

        }

        public SingleThreadDispatcher<HttpToken> ThreadDispatcher { get; set; }

        public ConcurrentQueue<RequestData> Requests { get; set; } = new ConcurrentQueue<RequestData>();

        public Utf8JsonWriter Utf8JsonWriter { get; set; }

        public ISession Session { get; set; }

        public RequestData CurrentRequest { get; set; }

        private HtmlBufferWriter mHtmlBufferWriter = null;

        public HtmlBufferWriter GetHtmlBufferWriter()
        {
            if (mHtmlBufferWriter == null)
                mHtmlBufferWriter = new HtmlBufferWriter(2048);
            return mHtmlBufferWriter;
        }

        public byte[] GetLengthBuffer(string length)
        {
            Encoding.ASCII.GetBytes(length, 0, length.Length, mLengthBuffer, 0);
            for (int i = length.Length; i < mLengthBuffer.Length; i++)
            {
                mLengthBuffer[i] = 32;
            }
            mLengthBuffer[6] = (byte)'\r';
            mLengthBuffer[7] = (byte)'\n';
            return mLengthBuffer;
        }

        public int ContentPostion { get; set; }

        public MemoryBlockCollection ContentLength { get; set; }

        public void FullLength(string length)
        {
            var item = GetLengthBuffer(length);
            ContentLength.Full(item);
        }

        private int mProcessStatus = 0;

        public void CompletedProcess()
        {
            System.Threading.Interlocked.Exchange(ref mProcessStatus, 0);
        }

        public bool EnterProcess()
        {
            return System.Threading.Interlocked.CompareExchange(ref mProcessStatus, 1, 0) == 0;
        }

    }
}
