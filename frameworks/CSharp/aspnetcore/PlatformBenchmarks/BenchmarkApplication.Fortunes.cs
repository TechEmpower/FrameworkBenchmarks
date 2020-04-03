// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Collections.Generic;
using System.IO.Pipelines;
using System.Text.Encodings.Web;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public partial class BenchmarkApplication
    {
        private async Task Fortunes(PipeWriter pipeWriter)
        {
            OutputFortunes(pipeWriter, await Db.LoadFortunesRows());
        }

        private void OutputFortunes(PipeWriter pipeWriter, List<Fortune> model)
        {
            var writer = GetWriter(pipeWriter);

            // HTTP 1.1 OK
            writer.Write(_http11OK);

            // Server headers
            writer.Write(_headerServer);

            // Date header
            writer.Write(DateHeader.HeaderBytes);

            // Content-Type header
            writer.Write(_headerContentTypeHtml);

            // Content-Length header
            writer.Write(_headerContentLength);

            var lengthWriter = writer;
            writer.Write(_contentLengthGap);

            // End of headers
            writer.Write(_eoh);

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
