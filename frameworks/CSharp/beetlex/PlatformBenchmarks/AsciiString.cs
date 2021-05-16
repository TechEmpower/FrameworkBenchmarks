using System;
using System.Collections.Generic;
using System.Text;

namespace PlatformBenchmarks
{
    public readonly struct AsciiString : IEquatable<AsciiString>
    {
        private readonly byte[] _data;

        public AsciiString(string s) => _data = Encoding.ASCII.GetBytes(s);

        private AsciiString(byte[] b) => _data = b;

        public int Length => _data.Length;

        public byte[] Data => _data;

        public ReadOnlySpan<byte> AsSpan() => _data;

        public static implicit operator ReadOnlySpan<byte>(AsciiString str) => str._data;
        public static implicit operator byte[](AsciiString str) => str._data;

        public static implicit operator AsciiString(string str) => new AsciiString(str);

        public override string ToString() => Encoding.ASCII.GetString(_data);
        public static explicit operator string(AsciiString str) => str.ToString();

        public bool Equals(AsciiString other) => ReferenceEquals(_data, other._data) || SequenceEqual(_data, other._data);
        private bool SequenceEqual(byte[] data1, byte[] data2) => new Span<byte>(data1).SequenceEqual(data2);

        public static bool operator ==(AsciiString a, AsciiString b) => a.Equals(b);
        public static bool operator !=(AsciiString a, AsciiString b) => !a.Equals(b);
        public override bool Equals(object other) => (other is AsciiString) && Equals((AsciiString)other);

        public static AsciiString operator +(AsciiString a, AsciiString b)
        {
            var result = new byte[a.Length + b.Length];
            a._data.CopyTo(result, 0);
            b._data.CopyTo(result, a.Length);
            return new AsciiString(result);
        }

        public override int GetHashCode()
        {
            // Copied from x64 version of string.GetLegacyNonRandomizedHashCode()
            // https://github.com/dotnet/coreclr/blob/master/src/mscorlib/src/System/String.Comparison.cs
            var data = _data;
            int hash1 = 5381;
            int hash2 = hash1;
            foreach (int b in data)
            {
                hash1 = ((hash1 << 5) + hash1) ^ b;
            }
            return hash1 + (hash2 * 1566083941);
        }

    }
}
