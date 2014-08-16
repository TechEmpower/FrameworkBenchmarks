using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Configuration;
using System.Data;
using System.Data.Common;
using System.Linq;
using System.Net;
using System.Threading;
using System.Web.Script.Serialization;

using MongoDB.Driver.Builders;

using Benchmarks.AspNet.Models;
using System.Reflection;
using System.Runtime.InteropServices;

namespace HttpListener
{
    class Program
    {
        private static void RequestCallback(Object state)
        {
            HttpListenerContext context = (HttpListenerContext)state;
            HttpListenerRequest request = context.Request;
            HttpListenerResponse response = context.Response;

            try
            {
                string responseString = null;
                switch (request.Url.LocalPath)
                {
                    case "/plaintext":
                        responseString = Plaintext(response);
                        break;
                    case "/json":
                        responseString = Json(response);
                        break;
                    case "/db":
                        responseString = Db(request, response);
                        break;
                    case "/fortunes":
                        responseString = Fortunes(request, response);
                        break;
                    case "/updates":
                        responseString = Updates(request, response);
                        break;
                    case "/mongodbdb":
                        responseString = MongoDBDb(request, response);
                        break;
                    case "/mongodbfortunes":
                        responseString = MongoDBFortunes(request, response);
                        break;
                    case "/mongodbupdates":
                        responseString = MongoDBUpdates(request, response);
                        break;
                    default:
                        responseString = NotFound(response);
                        break;
                }
                WriteResponse(response, responseString);
            }
            finally
            {
                response.Close();
            }
        }

        private static void WriteResponse(HttpListenerResponse response, String responseString)
        {
            response.ContentType = response.ContentType + "; charset=utf-8";
            byte[] buffer = System.Text.Encoding.UTF8.GetBytes(responseString);
            response.ContentLength64 = buffer.Length;
            try
            {
                response.OutputStream.Write(buffer, 0, buffer.Length);
            }
            catch (Win32Exception)
            {
                // Ignore I/O errors
            }
        }

        private static void Threads()
        {
            // To improve CPU utilization, increase the number of threads that the .NET thread pool expands by when
            // a burst of requests come in. We could do this by editing machine.config/system.web/processModel/minWorkerThreads,
            // but that seems too global a change, so we do it in code for just our AppPool. More info:
            //
            // http://support.microsoft.com/kb/821268
            // http://blogs.msdn.com/b/tmarq/archive/2007/07/21/asp-net-thread-usage-on-iis-7-0-and-6-0.aspx
            // http://blogs.msdn.com/b/perfworld/archive/2010/01/13/how-can-i-improve-the-performance-of-asp-net-by-adjusting-the-clr-thread-throttling-properties.aspx

            int newMinWorkerThreads = Convert.ToInt32(ConfigurationManager.AppSettings["minWorkerThreadsPerLogicalProcessor"]);
            if (newMinWorkerThreads > 0)
            {
                int minWorkerThreads, minCompletionPortThreads;
                ThreadPool.GetMinThreads(out minWorkerThreads, out minCompletionPortThreads);
                ThreadPool.SetMinThreads(Environment.ProcessorCount * newMinWorkerThreads, minCompletionPortThreads);
            }
        }

        static void Main(string[] args)
        {
            Threads();

            System.Net.HttpListener listener = new System.Net.HttpListener();
            // This doesn't seem to ignore all write exceptions, so in WriteResponse(), we still have a catch block.
            listener.IgnoreWriteExceptions = true;
            listener.Prefixes.Add("http://*:8080/");

            try
            {
                listener.Start();

                // Increase the HTTP.SYS backlog queue from the default of 1000 to 65535.
                // To verify that this works, run `netsh http show servicestate`.
                Network.Utils.HttpApi.SetRequestQueueLength(listener, 65535);

                for (;;)
                {
                    HttpListenerContext context = null;
                    try
                    {
                        context = listener.GetContext();

                        ThreadPool.QueueUserWorkItem(new WaitCallback(RequestCallback), context);
                        context = null; // ownership has been transferred to RequestCallback
                    }
                    catch (HttpListenerException ex)
                    {
                        Console.WriteLine(ex.Message);
                    }
                    finally
                    {
                        if (context != null)
                            context.Response.Close();
                    }
                }
            }
            catch (HttpListenerException ex)
            {
                Console.WriteLine(ex.Message);
            }
            finally
            {
                // Stop listening for requests.
                listener.Close();
                Console.WriteLine("Done Listening.");
            }
        }

        public static DbConnection CreateConnection(HttpListenerRequest request)
        {
            string providerName = request.QueryString["provider"];
            if (providerName == null)
            {
                throw new ApplicationException("Missing provider querystring argument");
            }
            ConnectionStringSettings connectionSettings = ConfigurationManager.ConnectionStrings[providerName];
            DbProviderFactory factory = DbProviderFactories.GetFactory(connectionSettings.ProviderName);
            DbConnection connection = factory.CreateConnection();
            connection.ConnectionString = connectionSettings.ConnectionString;
            return connection;
        }

        public static int GetQueries(HttpListenerRequest request)
        {
            int queries = 1;
            string queriesString = request.QueryString["queries"];
            if (queriesString != null)
            {
                // If this fails to parse, queries will be set to zero.
                int.TryParse(queriesString, out queries);
                queries = Math.Max(1, Math.Min(500, queries));
            }
            return queries;
        }

        private static string NotFound(HttpListenerResponse response)
        {
            response.StatusCode = (int)HttpStatusCode.NotFound;
            response.ContentType = "text/plain";
            return "not found";
        }

        private static string Plaintext(HttpListenerResponse response)
        {
            response.ContentType = "text/plain";
            return "Hello, World!";
        }

        private static string Json(HttpListenerResponse response)
        {
            response.ContentType = "application/json";
            return new JavaScriptSerializer().Serialize(new { message = "Hello, World!" });
        }

        private static string Db(HttpListenerRequest request, HttpListenerResponse response)
        {
            Random random = new Random();

            int queries = GetQueries(request);
            List<World> worlds = new List<World>(queries);

            using (DbConnection connection = CreateConnection(request))
            {
                connection.Open();

                using (DbCommand command = connection.CreateCommand())
                {
                    command.CommandText = "SELECT * FROM World WHERE id = @ID";

                    DbParameter parameter = command.CreateParameter();
                    parameter.ParameterName = "@ID";

                    command.Parameters.Add(parameter);

                    for (int i = 0; i < worlds.Capacity; i++)
                    {
                        int randomID = random.Next(0, 10000) + 1;

                        parameter.Value = randomID;

                        // Don't use CommandBehavior.SingleRow because that will make the MySql provider
                        // send two extra commands to limit the result to one row.
                        using (DbDataReader reader = command.ExecuteReader())
                        {
                            if (reader.Read())
                            {
                                worlds.Add(new World
                                {
                                    id = reader.GetInt32(0),
                                    randomNumber = reader.GetInt32(1)
                                });
                            }
                        }
                    }
                }
            }

            response.ContentType = "application/json";
            return new JavaScriptSerializer().Serialize(
                worlds.Count > 1 ? (Object)worlds : (Object)worlds[0]);
        }

        private static string Fortunes(HttpListenerRequest request, HttpListenerResponse response)
        {
            List<Fortune> fortunes = new List<Fortune>();

            using (DbConnection connection = CreateConnection(request))
            {
                connection.Open();

                using (DbCommand command = connection.CreateCommand())
                {
                    command.CommandText = "SELECT * FROM Fortune";

                    using (DbDataReader reader = command.ExecuteReader(CommandBehavior.SequentialAccess))
                    {
                        while (reader.Read())
                        {
                            fortunes.Add(new Fortune
                            {
                                ID = reader.GetInt32(0),
                                Message = reader.GetString(1)
                            });
                        }
                    }
                }
            }

            fortunes.Add(new Fortune { ID = 0, Message = "Additional fortune added at request time." });
            fortunes.Sort();

            response.ContentType = "text/html";
            var template = new Fortunes { Model = fortunes };
            return template.TransformText();
        }

        private static string Updates(HttpListenerRequest request, HttpListenerResponse response)
        {
            Random random = new Random();
            List<World> worlds = new List<World>(GetQueries(request));

            using (DbConnection connection = CreateConnection(request))
            {
                connection.Open();

                using (DbCommand selectCommand = connection.CreateCommand(),
                                 updateCommand = connection.CreateCommand())
                {
                    selectCommand.CommandText = "SELECT * FROM World WHERE id = @ID";
                    updateCommand.CommandText = "UPDATE World SET randomNumber = @Number WHERE id = @ID";

                    for (int i = 0; i < worlds.Capacity; i++)
                    {
                        int randomID = random.Next(0, 10000) + 1;
                        int randomNumber = random.Next(0, 10000) + 1;

                        DbParameter idParameter = selectCommand.CreateParameter();
                        idParameter.ParameterName = "@ID";
                        idParameter.Value = randomID;

                        selectCommand.Parameters.Clear();
                        selectCommand.Parameters.Add(idParameter);

                        World world = null;

                        // Don't use CommandBehavior.SingleRow because that will make the MySql provider
                        // send two extra commands to limit the result to one row.
                        using (DbDataReader reader = selectCommand.ExecuteReader())
                        {
                            if (reader.Read())
                            {
                                world = new World
                                {
                                    id = reader.GetInt32(0),
                                    randomNumber = reader.GetInt32(1)
                                };
                            }
                        }

                        DbParameter idUpdateParameter = updateCommand.CreateParameter();
                        idUpdateParameter.ParameterName = "@ID";
                        idUpdateParameter.Value = randomID;

                        DbParameter numberParameter = updateCommand.CreateParameter();
                        numberParameter.ParameterName = "@Number";
                        numberParameter.Value = randomNumber;

                        updateCommand.Parameters.Clear();
                        updateCommand.Parameters.Add(idUpdateParameter);
                        updateCommand.Parameters.Add(numberParameter);

                        updateCommand.ExecuteNonQuery();

                        world.randomNumber = randomNumber;
                        worlds.Add(world);
                    }
                }
            }

            response.ContentType = "application/json";
            return new JavaScriptSerializer().Serialize(
                worlds.Count > 1 ? (Object)worlds : (Object)worlds[0]);
        }

        private static string MongoDBDb(HttpListenerRequest request, HttpListenerResponse response)
        {
            Random random = new Random();

            int queries = GetQueries(request);
            List<World> worlds = new List<World>(queries);

            Benchmarks.AspNet.Models.MongoDB db = new Benchmarks.AspNet.Models.MongoDB("MongoDB");

            for (int i = 0; i < worlds.Capacity; i++)
            {
                int randomID = random.Next(0, 10000) + 1;
                worlds.Add(db.Worlds.FindOne(Query<World>.EQ(w => w.id, randomID)));
            }

            response.ContentType = "application/json";
            return new JavaScriptSerializer().Serialize(
                worlds.Count > 1 ? (Object)worlds : (Object)worlds[0]);
        }

        private static string MongoDBFortunes(HttpListenerRequest request, HttpListenerResponse response)
        {
            Benchmarks.AspNet.Models.MongoDB db = new Benchmarks.AspNet.Models.MongoDB("MongoDB");

            List<Fortune> fortunes = db.Fortunes.FindAll().ToList();

            fortunes.Add(new Fortune { ID = 0, Message = "Additional fortune added at request time." });
            fortunes.Sort();

            response.ContentType = "text/html";
            var template = new Fortunes { Model = fortunes };
            return template.TransformText();
        }

        private static string MongoDBUpdates(HttpListenerRequest request, HttpListenerResponse response)
        {
            Random random = new Random();

            Benchmarks.AspNet.Models.MongoDB db = new Benchmarks.AspNet.Models.MongoDB("MongoDB");

            int queries = GetQueries(request);
            List<World> worlds = new List<World>(queries);

            for (int i = 0; i < worlds.Capacity; i++)
            {
                int randomID = random.Next(0, 10000) + 1;
                int randomNumber = random.Next(0, 10000) + 1;

                World world = db.Worlds.FindOne(Query<World>.EQ(w => w.id, randomID));
                world.randomNumber = randomNumber;
                worlds.Add(world);

                db.Worlds.Save(world);
            }

            response.ContentType = "application/json";
            return new JavaScriptSerializer().Serialize(
                worlds.Count > 1 ? (Object)worlds : (Object)worlds[0]);
        }
    }
}

// Adapted from:
// http://stackoverflow.com/questions/15417062/changing-http-sys-kernel-queue-limit-when-using-net-httplistener
namespace Network.Utils
{
    public static class HttpApi
    {
        public static void SetRequestQueueLength(System.Net.HttpListener listener, uint len)
        {
            var listenerType = typeof(System.Net.HttpListener);
            var requestQueueHandleProperty = listenerType.GetProperties(BindingFlags.NonPublic | BindingFlags.Instance).First(p => p.Name == "RequestQueueHandle");

            var requestQueueHandle = (CriticalHandle)requestQueueHandleProperty.GetValue(listener);
            var result = HttpSetRequestQueueProperty(requestQueueHandle, HTTP_SERVER_PROPERTY.HttpServerQueueLengthProperty, ref len, (uint)Marshal.SizeOf(len), 0, IntPtr.Zero);

            if (result != 0)
            {
                throw new HttpListenerException((int)result);
            }
        }

        internal enum HTTP_SERVER_PROPERTY
        {
            HttpServerAuthenticationProperty,
            HttpServerLoggingProperty,
            HttpServerQosProperty,
            HttpServerTimeoutsProperty,
            HttpServerQueueLengthProperty,
            HttpServerStateProperty,
            HttpServer503VerbosityProperty,
            HttpServerBindingProperty,
            HttpServerExtendedAuthenticationProperty,
            HttpServerListenEndpointProperty,
            HttpServerChannelBindProperty,
            HttpServerProtectionLevelProperty,
        }

        [DllImport("httpapi.dll", CallingConvention = CallingConvention.StdCall)]
        internal static extern uint HttpSetRequestQueueProperty(
            CriticalHandle requestQueueHandle,
            HTTP_SERVER_PROPERTY serverProperty,
            ref uint pPropertyInfo,
            uint propertyInfoLength,
            uint reserved,
            IntPtr pReserved);
    }
}
