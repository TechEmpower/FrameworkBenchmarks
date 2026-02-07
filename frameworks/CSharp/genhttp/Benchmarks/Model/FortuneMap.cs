using System.Collections;
using Cottle;

namespace Benchmarks.Model;

public readonly struct FortuneMap : IMap
{
    private readonly int _id;
    private readonly string _message;
    
    public FortuneMap(int id, string message)
    {
        _id = id;
        _message = message;
    }
    
    public Value this[Value key] => TryGet(key, out var value) ? value : Value.Undefined;
    
    public int Count => 2;
    
    public bool Contains(Value key)
    {
        var keyStr = key.AsString;
        return keyStr == "id" || keyStr == "message";
    }
    
    public bool TryGet(Value key, out Value value)
    {
        var keyStr = key.AsString;
        if (keyStr == "id")
        {
            value = _id;
            return true;
        }
        if (keyStr == "message")
        {
            value = _message;
            return true;
        }
        
        value = Value.Undefined;
        return false;
    }
    
    public int CompareTo(IMap other)
    {
        if (other == null) return 1;
        
        if (other.TryGet("id", out var otherId))
        {
            return _id.CompareTo(otherId.AsNumber);
        }
        
        return Count.CompareTo(other.Count);
    }
    
    public bool Equals(IMap other)
    {
        if (other == null || other.Count != Count) 
            return false;
        
        return other.TryGet("id", out var otherId) && 
            other.TryGet("message", out var otherMsg) &&
            _id == (int)otherId.AsNumber && 
            _message == otherMsg.AsString;
    }
    
    public IEnumerator<KeyValuePair<Value, Value>> GetEnumerator()
    {
        yield return new KeyValuePair<Value, Value>("id", _id);
        yield return new KeyValuePair<Value, Value>("message", _message);
    }
    
    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
    
    public override bool Equals(object obj) => obj is IMap map && Equals(map);
    
    public override int GetHashCode() => HashCode.Combine(_id, _message);
    
}