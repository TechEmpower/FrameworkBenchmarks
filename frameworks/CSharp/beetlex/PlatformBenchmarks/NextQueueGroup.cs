using System;
using System.Collections.Generic;
using System.Text;

namespace PlatformBenchmarks
{
    public class NextQueueGroup
    {

        private List<NextQueue> mQueues = new List<NextQueue>();

        public NextQueueGroup(int count = 0)
        {
            if (count == 0)
            {
                count = Math.Min(Environment.ProcessorCount, 16);
            }

            for (int i = 0; i < count; i++)
                mQueues.Add(new NextQueue());
        }

        private long mIndex = 0;

        public void Enqueue(IEventWork item, int waitLength = 5)
        {
            for (int i = 0; i < mQueues.Count; i++)
            {
                if (mQueues[i].Count < waitLength)
                {
                    mQueues[i].Enqueue(item);
                    return;
                }
            }
            Next().Enqueue(item);
        }

        public NextQueue Next(int waitLength)
        {
            for (int i = 0; i < mQueues.Count; i++)
            {
                if (mQueues[i].Count < waitLength)
                {
                    return mQueues[i];

                }
            }
            return Next();
        }

        public NextQueue Next()
        {
            var index = System.Threading.Interlocked.Increment(ref mIndex);
            return mQueues[(int)(index % mQueues.Count)];
        }
    }
}
