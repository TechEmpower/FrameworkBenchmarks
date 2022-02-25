using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public class HtmlBufferWriter
    {

        public HtmlBufferWriter(int size)
        {
            mData = new byte[size];
        }
        public void Write(byte data)
        {
            mData[mPostion] = data;
            mPostion++;
        }
        public void Write(string value)
        {
            var len = Encoding.UTF8.GetBytes(value, 0, value.Length, mData, mPostion);
            mPostion += len;
        }
        public void Write(byte[] data)
        {
            Write(data, 0, data.Length);
        }

        public void Write(byte[] data, int offset, int count)
        {
            if (count <= 8)
            {
                for (int i = 0; i < count; i++)
                {
                    mData[i + mPostion] = data[offset + i];
                }
            }
            else
            {
                System.Buffer.BlockCopy(data, offset, mData, mPostion, count);
            }
            mPostion += count;
        }

        private byte[] mData;

        private int mPostion = 0;

        public byte[] Data => mData;

        public int Length => mPostion;

        public void Reset()
        {
            mPostion = 0;
        }

    }
}
