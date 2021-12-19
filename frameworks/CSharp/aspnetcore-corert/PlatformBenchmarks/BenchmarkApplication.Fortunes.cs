// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

#if DATABASE

using System.Collections.Generic;
using System.IO.Pipelines;
using System.Text.Encodings.Web;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public partial class BenchmarkApplication
    {
        private readonly static AsciiString _fortunesPreamble =
            _http11OK +
            _headerServer + _crlf +
            _headerContentTypeHtml + _crlf +
            _headerContentLength;

        private async Task Fortunes(PipeWriter pipeWriter)
        {
            OutputFortunes(pipeWriter, await Db.LoadFortunesRows());
        }

        private void OutputFortunes(PipeWriter pipeWriter, List<Fortune> model)
        {
            var writer = GetWriter(pipeWriter, sizeHint: 1600); // in reality it's 1361

            writer.Write(_fortunesPreamble);

            var lengthWriter = writer;
            writer.Write(_contentLengthGap);

            // Date header
            writer.Write(DateHeader.HeaderBytes);

            var bodyStart = writer.Buffered;
            // Body
            writer.Write(_fortunesTableStart);
            foreach (var item in model)
            {
                writer.Write(_fortunesRowStart);
                writer.WriteNumeric((uint)item.Id);
                writer.Write(_fortunesColumn);
                writer.WriteUtf8String(HtmlEncoder.Encode(item.Message));
                writer.Write(_fortunesRowEnd);
            }
            writer.Write(_fortunesTableEnd);
            lengthWriter.WriteNumeric((uint)(writer.Buffered - bodyStart));

            writer.Commit();
        }
    }
}

#endif