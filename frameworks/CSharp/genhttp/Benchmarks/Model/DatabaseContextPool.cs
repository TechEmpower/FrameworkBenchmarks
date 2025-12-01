namespace Benchmarks.Model;

using System.Collections.Concurrent;

using Microsoft.EntityFrameworkCore;

public sealed class DatabaseContextPool<TContext> where TContext : DbContext
{
    private readonly ConcurrentBag<TContext> _pool = new();

    private readonly Func<TContext> _factory;

    private readonly int _maxSize;

    public DatabaseContextPool(Func<TContext> factory, int maxSize)
    {
        _factory = factory;
        _maxSize = maxSize;
    }

    public TContext Rent()
    {
        if (_pool.TryTake(out var ctx))
        {
            ctx.ChangeTracker.Clear();
            return ctx;
        }

        return _factory();
    }

    public void Return(TContext context)
    {
        if (_pool.Count >= _maxSize)
        {
            context.Dispose();
            return;
        }


        context.ChangeTracker.Clear();

        _pool.Add(context);
    }

}
