// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

#if DATABASE

using System.IO.Pipelines;
using System.Runtime.CompilerServices;
using RazorSlices;

namespace PlatformBenchmarks
{
    public partial class BenchmarkApplication
    {
        private async Task Fortunes(PipeWriter pipeWriter)
        {
            await OutputFortunes(pipeWriter, await Db.LoadFortunesRows(), FortunesTemplateFactory);
        }

        private ValueTask OutputFortunes<TModel>(PipeWriter pipeWriter, TModel model, SliceFactory<TModel> templateFactory)
        {
            // Render headers
            var preamble = """
                HTTP/1.1 200 OK
                Server: K
                Content-Type: text/html; charset=utf-8
                Transfer-Encoding: chunked
                """u8;
            var headersLength = preamble.Length + DateHeader.HeaderBytes.Length;
            var headersSpan = pipeWriter.GetSpan(headersLength);
            preamble.CopyTo(headersSpan);
            DateHeader.HeaderBytes.CopyTo(headersSpan[preamble.Length..]);
            pipeWriter.Advance(headersLength);

            // Render body
            var template = templateFactory(model);
            // Kestrel PipeWriter span size is 4K, headers above already written to first span & template output is ~1350 bytes,
            // so 2K chunk size should result in only a single span and chunk being used.
            var chunkedWriter = GetChunkedWriter(pipeWriter, chunkSizeHint: 2048);
            var renderTask = template.RenderAsync(chunkedWriter, null, HtmlEncoder);

            if (renderTask.IsCompletedSuccessfully)
            {
                renderTask.GetAwaiter().GetResult();
                EndTemplateRendering(chunkedWriter, template);
                return ValueTask.CompletedTask;
            }

            return AwaitTemplateRenderTask(renderTask, chunkedWriter, template);
        }

        private static async ValueTask AwaitTemplateRenderTask(ValueTask renderTask, ChunkedBufferWriter<WriterAdapter> chunkedWriter, RazorSlice template)
        {
            await renderTask;
            EndTemplateRendering(chunkedWriter, template);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static void EndTemplateRendering(ChunkedBufferWriter<WriterAdapter> chunkedWriter, RazorSlice template)
        {
            chunkedWriter.End();
            ReturnChunkedWriter(chunkedWriter);
            template.Dispose();
        }
    }
}

#endif