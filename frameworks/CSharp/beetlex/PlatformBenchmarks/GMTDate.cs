using BeetleX.Buffers;
using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;

namespace PlatformBenchmarks
{
    internal class GMTDate
    {
        private List<byte[]> mWeekBuffers = new List<byte[]>();

        public List<byte[]> mYears = new List<byte[]>();

        public List<byte[]> mMoth = new List<byte[]>();

        public List<byte[]> mNumber = new List<byte[]>();

        private byte _1 = 58;

        private byte _s = 32;

        private byte _r = 13;

        private byte _n = 10;

        private byte[] GMT = new byte[3]
        {
        71,
        77,
        84
        };

        private static GMTDate mDefault;

        private Timer mUpdateTime;

        public static GMTDate Default
        {
            get
            {
                if (mDefault == null)
                {
                    mDefault = new GMTDate();
                    mDefault.Init();
                }
                return mDefault;
            }
        }

        public ArraySegment<byte> DATE
        {
            get;
            set;
        }

        public GMTDate()
        {
            mWeekBuffers.Add(Encoding.ASCII.GetBytes("Sun"));
            mWeekBuffers.Add(Encoding.ASCII.GetBytes("Mon"));
            mWeekBuffers.Add(Encoding.ASCII.GetBytes("Tue"));
            mWeekBuffers.Add(Encoding.ASCII.GetBytes("Wed"));
            mWeekBuffers.Add(Encoding.ASCII.GetBytes("Thu"));
            mWeekBuffers.Add(Encoding.ASCII.GetBytes("Fri"));
            mWeekBuffers.Add(Encoding.ASCII.GetBytes("Sat"));
            for (int j = 1970; j < 2470; j++)
            {
                mYears.Add(Encoding.ASCII.GetBytes(j.ToString()));
            }
            for (int i = 0; i <= 100; i++)
            {
                mNumber.Add(Encoding.ASCII.GetBytes(i.ToString("00")));
            }
            mMoth.Add(Encoding.ASCII.GetBytes("Jan"));
            mMoth.Add(Encoding.ASCII.GetBytes("Feb"));
            mMoth.Add(Encoding.ASCII.GetBytes("Mar"));
            mMoth.Add(Encoding.ASCII.GetBytes("Apr"));
            mMoth.Add(Encoding.ASCII.GetBytes("May"));
            mMoth.Add(Encoding.ASCII.GetBytes("Jun"));
            mMoth.Add(Encoding.ASCII.GetBytes("Jul"));
            mMoth.Add(Encoding.ASCII.GetBytes("Aug"));
            mMoth.Add(Encoding.ASCII.GetBytes("Sep"));
            mMoth.Add(Encoding.ASCII.GetBytes("Oct"));
            mMoth.Add(Encoding.ASCII.GetBytes("Nov"));
            mMoth.Add(Encoding.ASCII.GetBytes("Dec"));
        }

        private void Init()
        {
            DATE = GetData();
            mUpdateTime = new Timer(delegate
            {
                DATE = GetData();
            }, null, 1000, 1000);
        }

        private ArraySegment<byte> GetData()
        {
            return GetData(DateTime.Now);
        }

        public void Write(PipeStream stream)
        {
            var data = DATE;
            stream.Write(data.Array, 0, data.Count);
        }

        private ArraySegment<byte> GetData(DateTime date)
        {
            date = date.ToUniversalTime();
            int offset13 = 0;
            byte[] GTM_BUFFER = new byte[50];
            Encoding.ASCII.GetBytes("Date: ", 0, 6, GTM_BUFFER, 0);
            offset13 = 6;
            byte[] buffer = GTM_BUFFER;
            byte[] sub8 = mWeekBuffers[(int)date.DayOfWeek];
            buffer[offset13] = sub8[0];
            offset13++;
            buffer[offset13] = sub8[1];
            offset13++;
            buffer[offset13] = sub8[2];
            offset13++;
            buffer[offset13] = 44;
            offset13++;
            buffer[offset13] = _s;
            offset13++;
            sub8 = mNumber[date.Day];
            buffer[offset13] = sub8[0];
            offset13++;
            buffer[offset13] = sub8[1];
            offset13++;
            buffer[offset13] = _s;
            offset13++;
            sub8 = mMoth[date.Month - 1];
            buffer[offset13] = sub8[0];
            offset13++;
            buffer[offset13] = sub8[1];
            offset13++;
            buffer[offset13] = sub8[2];
            offset13++;
            buffer[offset13] = _s;
            offset13++;
            sub8 = mYears[date.Year - 1970];
            buffer[offset13] = sub8[0];
            offset13++;
            buffer[offset13] = sub8[1];
            offset13++;
            buffer[offset13] = sub8[2];
            offset13++;
            buffer[offset13] = sub8[3];
            offset13++;
            buffer[offset13] = _s;
            offset13++;
            sub8 = mNumber[date.Hour];
            buffer[offset13] = sub8[0];
            offset13++;
            buffer[offset13] = sub8[1];
            offset13++;
            buffer[offset13] = _1;
            offset13++;
            sub8 = mNumber[date.Minute];
            buffer[offset13] = sub8[0];
            offset13++;
            buffer[offset13] = sub8[1];
            offset13++;
            buffer[offset13] = _1;
            offset13++;
            sub8 = mNumber[date.Second];
            buffer[offset13] = sub8[0];
            offset13++;
            buffer[offset13] = sub8[1];
            offset13++;
            buffer[offset13] = _s;
            offset13++;
            sub8 = GMT;
            buffer[offset13] = sub8[0];
            offset13++;
            buffer[offset13] = sub8[1];
            offset13++;
            buffer[offset13] = sub8[2];
            offset13++;

            buffer[offset13] = _r;
            offset13++;
            buffer[offset13] = _n;
            offset13++;
            buffer[offset13] = _r;
            offset13++;
            buffer[offset13] = _n;
            offset13++;

            return new ArraySegment<byte>(GTM_BUFFER, 0, offset13);
        }
    }

}
