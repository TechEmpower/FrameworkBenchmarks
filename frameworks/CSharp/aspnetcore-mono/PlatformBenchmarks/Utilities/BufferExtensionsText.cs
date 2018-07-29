// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Buffers;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;

namespace PlatformBenchmarks
{
    internal static class BufferExtensionsText
    {
        private readonly static AsciiString _htmlLessThan = "&lt;";
        private readonly static AsciiString _htmlGreaterThan = "&gt;";
        private readonly static AsciiString _htmlAmpersand = "&amp;";
        private readonly static AsciiString _htmlDoubleQuote = "&quot;";
        private readonly static AsciiString _htmlSingleQuote = "&apos;";
        private readonly static AsciiString _htmlEscapeStart = "&#";
        private readonly static AsciiString _htmlEscapeEnd = ";";

        public unsafe static void WriteUtf8String<T>(ref this BufferWriter<T> buffer, string text)
             where T : struct, IBufferWriter<byte>
        {
            if (string.IsNullOrEmpty(text)) return;

            fixed (char* start = text)
            {
                WriteUtf8(ref buffer, start, text.Length);
            }
        }

        public unsafe static void WriteUtf8HtmlString<T>(ref this BufferWriter<T> buffer, string text)
            where T : struct, IBufferWriter<byte>
        {
            if (string.IsNullOrEmpty(text)) return;

            fixed (char* start = text)
            {
                var current = start;
                var length = text.Length;

                var firstIndexToEncode = FirstCharToHtmlEncode(current, length);
                if (firstIndexToEncode == -1) 
                {
                    // Nothing to html encode.
                    WriteUtf8(ref buffer, current, length);
                    return;
                }

                while (true)
                {
                    if (firstIndexToEncode > 0)
                    {
                        WriteUtf8(ref buffer, current, firstIndexToEncode);
                        current += firstIndexToEncode;
                        length -= firstIndexToEncode;
                    }

                    var charsUsed = EncodeHtml(ref buffer, current, length);
                    current += charsUsed;
                    length -= charsUsed;

                    if (length > 0)
                    {
                        firstIndexToEncode = FirstCharToHtmlEncode(current, length);
                        if (firstIndexToEncode == -1)
                        {
                            // nothing left to html encode
                            WriteUtf8(ref buffer, current, length);
                            break;
                        }

                        continue;
                    }

                    break; // Done
                }
            }
        }

        private unsafe static void WriteUtf8<T>(ref this BufferWriter<T> buffer, char* input, int length)
             where T : struct, IBufferWriter<byte>
        {
            var current = input;
            var output = buffer.Span;
            while (true)
            {
                var lengthToEncode = Math.Min(length, output.Length);
                fixed (byte* pOutput = &MemoryMarshal.GetReference(output))
                {
                    var success = TryEncodeAsciiCharsToBytes(current, pOutput, lengthToEncode, out var consumed);
                    length -= consumed;
                    current += consumed;
                    buffer.Advance(consumed);
                    if (!success)
                    {
                        // Non-Ascii, move to Utf8 encoding
                        WriteUtf8Encoding(ref buffer, current, length);
                        return;
                    }
                }

                if (length > 0)
                {
                    // Need more output space, get a new span
                    buffer.Ensure(output.Length + 1);
                    output = buffer.Span;
                    continue;
                }

                break; // Done
            }
        }

        private static unsafe void WriteUtf8Encoding<T>(ref this BufferWriter<T> buffer, char* input, int length)
             where T : struct, IBufferWriter<byte>
        {
            var current = input;
            var output = buffer.Span;
            while (true)
            {
                var charsToUse = length;
                while (charsToUse > 0)
                {
                    if (Encoding.UTF8.GetByteCount(current, length) <= output.Length)
                    {
                        int byteCount;
                        fixed (byte* pBuffer = &MemoryMarshal.GetReference(output))
                        {
                            byteCount = Encoding.UTF8.GetBytes(current, charsToUse, pBuffer, output.Length);
                        }

                        buffer.Advance(byteCount);
                        length -= charsToUse;
                        current += charsToUse;
                        break;
                    }

                    // Try again with 1/2 the count
                    charsToUse /= 2;
                }

                if (length > 0)
                {
                    // Get a new span
                    buffer.Ensure(output.Length + 1);
                    output = buffer.Span;
                    continue;
                }

                break; // Done
            }
        }

        private unsafe static int EncodeHtml<T>(ref this BufferWriter<T> buffer, char* input, int length)
            where T : struct, IBufferWriter<byte>
        {
            var encoded = 0;
            while (true)
            {
                var ch = input[0];
                switch (ch)
                {
                    case '\"':
                        buffer.Write(_htmlDoubleQuote);
                        break;
                    case '&':
                        buffer.Write(_htmlAmpersand);
                        break;
                    case '\'':
                        buffer.Write(_htmlSingleQuote);
                        break;
                    case '<':
                        buffer.Write(_htmlLessThan);
                        break;
                    case '>':
                        buffer.Write(_htmlGreaterThan);
                        break;
                    default:
                        buffer.Write(_htmlEscapeStart);
                        buffer.WriteNumeric(ch);
                        buffer.Write(_htmlEscapeEnd);
                        break;
                }

                encoded++;
                length--;

                if (length > 0 && IsCharacterHtmlEncoded(input[1]))
                {
                    input++;
                    // Next char also should be encoded
                    continue;
                }

                break; // Done
            }

            return encoded;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static bool IsCharacterHtmlEncoded(char character)
        {
            const int htmlEncodedChars = 
                (0x1 << ('<' & 0x1F)) | 
                (0x1 << ('>' & 0x1F)) | 
                (0x1 << ('&' & 0x1F)) | 
                (0x1 << ('\'' & 0x1F)) | 
                (0x1 << ('\"' & 0x1F));

            bool result;
            if (character >= 'A')
            {
                result = false;
            }
            else if (character >= ' ')
            {
                result = ((htmlEncodedChars >> (character & 0x1F)) & 0x1) == 0 ? false : true;
            }
            else
            {
                // Control chars, encode these as &#1;
                result = true;
            }

            return result;
        }

        private static unsafe int FirstCharToHtmlEncode(char* text, int textLength)
        {
            var i = 0;
            for (; i < textLength; i++)
            {
                if (IsCharacterHtmlEncoded(text[i]))
                {
                    break;
                }
            }

            return i < textLength ? i : -1;
        }

        // Encode as bytes upto the first non-ASCII byte and return count encoded
        private static unsafe bool TryEncodeAsciiCharsToBytes(char* input, byte* output, int length, out int consumed)
        {
            // Note: Not BIGENDIAN
            const int Shift16Shift24 = (1 << 16) | (1 << 24);
            const int Shift8Identity = (1 << 8) | (1);

            var i = 0;

            if (length < 4) goto trailing;

            var unaligned = (int)(((ulong)input) & 0x7) >> 1;
            // Unaligned chars
            for (; i < unaligned; i++)
            {
                var ch = input[i];
                if (ch > 0x7f)
                {
                    goto exit; // Non-ascii
                }
                output[i] = (byte)ch; // Cast convert
            }

            // Aligned
            int ulongDoubleCount = (length - i) & ~0x7;
            for (; i < ulongDoubleCount; i += 8)
            {
                ulong inputUlong0 = *(ulong*)(input + i);
                ulong inputUlong1 = *(ulong*)(input + i + 4);
                // Pack 16 ASCII chars into 16 bytes
                if ((inputUlong0 & 0xFF80FF80FF80FF80u) == 0)
                {
                    *(uint*)(output + i) =
                        ((uint)((inputUlong0 * Shift16Shift24) >> 24) & 0xffff) |
                        ((uint)((inputUlong0 * Shift8Identity) >> 24) & 0xffff0000);
                }
                else
                {
                    goto exit; // Non-ascii
                }
                if ((inputUlong1 & 0xFF80FF80FF80FF80u) == 0)
                {
                    *(uint*)(output + i + 4) =
                        ((uint)((inputUlong1 * Shift16Shift24) >> 24) & 0xffff) |
                        ((uint)((inputUlong1 * Shift8Identity) >> 24) & 0xffff0000);
                }
                else
                {
                    i += 4;
                    goto exit; // Non-ascii
                }
            }
            if (length - 4 > i)
            {
                ulong inputUlong = *(ulong*)(input + i);
                if ((inputUlong & 0xFF80FF80FF80FF80u) == 0)
                {
                    // Pack 8 ASCII chars into 8 bytes
                    *(uint*)(output + i) =
                        ((uint)((inputUlong * Shift16Shift24) >> 24) & 0xffff) |
                        ((uint)((inputUlong * Shift8Identity) >> 24) & 0xffff0000);
                }
                else
                {
                    goto exit; // Non-ascii
                }

                i += 4;
            }

        trailing:
            for (; i < length; i++)
            {
                var ch = input[i];
                if (ch > 0x7f)
                {
                    goto exit; // Hit non-ascii
                }

                output[i] = (byte)ch; // Cast convert
            }

        exit:
            consumed = i;
            return length == i ? true : false;
        }
    }
}
