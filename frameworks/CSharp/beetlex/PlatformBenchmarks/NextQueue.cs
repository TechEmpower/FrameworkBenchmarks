using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public class NextQueue : IDisposable
    {
        public NextQueue()
        {
            mQueue = new System.Collections.Concurrent.ConcurrentQueue<IEventWork>();
          
        }

        private readonly object _workSync = new object();

        private bool _doingWork;

        private int mCount;

        private System.Collections.Concurrent.ConcurrentQueue<IEventWork> mQueue;

        public int Count => System.Threading.Interlocked.Add(ref mCount, 0);

        public void Enqueue(IEventWork item)
        {
            mQueue.Enqueue(item);
            System.Threading.Interlocked.Increment(ref mCount);
            lock (_workSync)
            {
                if (!_doingWork)
                {
                    System.Threading.ThreadPool.QueueUserWorkItem(OnStart);
                    _doingWork = true;
                }
            }
        }

        private void OnError(Exception e, IEventWork work)
        {
            try
            {
                Error?.Invoke(e, work);
            }
            catch
            {

            }
        }

        public static Action<Exception, IEventWork> Error { get; set; }

        private async void OnStart(object state)
        {
            while (true)
            {
                while (mQueue.TryDequeue(out IEventWork item))
                {
                    System.Threading.Interlocked.Decrement(ref mCount);
                    using (item)
                    {
                        try
                        {
                            await item.Execute();
                        }
                        catch (Exception e_)
                        {
                            OnError(e_, item);
                        }
                    }
                }
                lock (_workSync)
                {
                    if (mQueue.IsEmpty)
                    {
                        try
                        {
                            Unused?.Invoke();
                        }
                        catch { }
                        _doingWork = false;
                        return;
                    }
                }
            }

        }

        public Action Unused { get; set; }

        public void Dispose()
        {
            while (mQueue.TryDequeue(out IEventWork work))
            {
                try
                {
                    work.Dispose();
                }
                catch
                {

                }

            }
        }
    }
}
