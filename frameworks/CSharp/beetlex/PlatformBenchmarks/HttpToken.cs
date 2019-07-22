using BeetleX.Buffers;
using System;
using System.Collections.Generic;
using System.Text;

namespace PlatformBenchmarks
{
    public class HttpToken
    {
        private byte[] mLengthBuffer = new byte[10];

        public byte[] Buffer
        {
            get;
            private set;
        }       

        public HttpToken()
        {
            Buffer = new byte[2048];
        }

        public byte[] GetLengthBuffer(string length)
        {
            Encoding.ASCII.GetBytes(length, 0, length.Length, mLengthBuffer, 0);
            for (int i = length.Length; i < 10; i++)
            {
                mLengthBuffer[i] = 32;
            }
            return mLengthBuffer;
        }

        public int ContentPostion { get; set; }

        public  MemoryBlockCollection ContentLength { get; set; }

        public void FullLength(string length)
        {
            var item = GetLengthBuffer(length);
            ContentLength.Full(item);
        }
        
    }
}
