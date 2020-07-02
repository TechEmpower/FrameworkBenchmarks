﻿using BeetleX;
using BeetleX.Buffers;
using SpanJson;
using System;
using System.Collections.Generic;
using System.Text;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {
        public async void updates(string queryString, PipeStream stream, HttpToken token, ISession session)
        {
            int count = 1;
            if (!string.IsNullOrEmpty(queryString))
            {
                var values = queryString.Split('=');
                if (values.Length > 1)
                {
                    if (int.TryParse(values[1], out int size))
                    {
                        count = size;
                    }
                }
            }
            if (count > 500)
                count = 500;
            if (count < 1)
                count = 1;
            try
            {
                var data = await token.Db.LoadMultipleUpdatesRows(count);
                await JsonSerializer.NonGeneric.Utf8.SerializeAsync(data, stream);
            }
            catch (Exception e_)
            {
                HttpServer.ApiServer.Log(BeetleX.EventArgs.LogType.Error, null, $"updates error {e_.Message}@{e_.StackTrace}");
                stream.Write(e_.Message);
            }
            OnCompleted(stream, session, token);
        }
    }
}
