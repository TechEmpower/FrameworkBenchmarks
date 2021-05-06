using System;
using System.Collections.Generic;
using System.Text;

namespace PlatformBenchmarks
{
    public readonly struct AsciiString : IEquatable<AsciiString>
    {
        private readonly byte[] _data;

        public byte[] Data => _data;

        public int Length => _data.Length;

        public AsciiString(string s)
        {
            _data = Encoding.ASCII.GetBytes(s);
        }

        public ReadOnlySpan<byte> AsSpan()
        {
            return _data;
        }

        public static implicit operator ReadOnlySpan<byte>(AsciiString str)
        {
            return str._data;
        }

        public static implicit operator byte[](AsciiString str)
        {
            return str._data;
        }

        public static implicit operator AsciiString(string str)
        {
            return new AsciiString(str);
        }

        public override string ToString()
        {
            return Encoding.ASCII.GetString(_data);
        }

        public int CopyTo(byte[] source,int offset)
        {
            var len = Length;
            Buffer.BlockCopy(_data, 0, source, offset, len);
            return len;
        }

        public static explicit operator string(AsciiString str)
        {
            return str.ToString();
        }

        public bool Equals(AsciiString other)
        {
            if (_data != other._data)
            {
                return SequenceEqual(_data, other._data);
            }
            return true;
        }

        private bool SequenceEqual(byte[] data1, byte[] data2)
        {
            return new Span<byte>(data1).SequenceEqual(data2);
        }

        public static bool operator ==(AsciiString a, AsciiString b)
        {
            return a.Equals(b);
        }

        public static bool operator !=(AsciiString a, AsciiString b)
        {
            return !a.Equals(b);
        }

        public override bool Equals(object other)
        {
            if (other is AsciiString)
            {
                return Equals((AsciiString)other);
            }
            return false;
        }

        public override int GetHashCode()
        {
            byte[] data = _data;
            int hash3 = 5381;
            int hash2 = hash3;
            byte[] array = data;
            foreach (int b in array)
            {
                hash3 = (((hash3 << 5) + hash3) ^ b);
            }
            return hash3 + hash2 * 1566083941;
        }
    }
}
