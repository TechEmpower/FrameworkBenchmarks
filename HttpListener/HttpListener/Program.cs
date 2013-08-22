using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Configuration;
using System.Data;
using System.Data.Common;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using System.Threading;
using System.Web.Script.Serialization;

using MongoDB.Driver.Builders;

using Benchmarks.AspNet.Models;


namespace HttpListener
{
    public class Program
    {
        private static void RequestCallback(object state)
        {
            var context = (HttpListenerContext)state;
            var request = context.Request;
            var response = context.Response;

            try
            {
                var localPath = request.Url.LocalPath;

                switch (localPath)
                {
                    case "/":
                        NotFound(response);
                        return;
                    case "/plaintext":
                        Plaintext(response);
                        return;
                    case "/fortunes":
                        Fortunes(request, response);
                        return;
                    case "/mongodbfortunes":
                        MongoDBFortunes(request, response);
                        return;
                }

                var serializer = GetSerializer(request);

                switch (localPath)
                {
                    case "/json":
                        Json(response, serializer);
                        return;
                    case "/db":
                        Db(request, response, serializer);
                        return;
                    case "/updates":
                        Updates(request, response, serializer);
                        return;
                    case "/mongodbdb":
                        MongoDBDb(request, response, serializer);
                        return;
                    case "/mongodbupdates":
                        MongoDBUpdates(request, response, serializer);
                        return;
                    default:
                        NotFound(response);
                        return;
                }
            }
            catch (Exception exception)
            {
                Error(response, exception);
            }
            finally
            {
                response.Close();
            }
        }

        private static Action<object, HttpListenerResponse> GetSerializer(HttpListenerRequest request)
        {
            var serializerName = request.QueryString["serializer"];

            if (serializerName == null)
            {
                throw new ApplicationException("Missing serializer querystring argument");
            }

            Action<object, HttpListenerResponse> serializer = null;
            switch (serializerName)
            {
                case "jsonnet":
                    serializer = JsonNetSerialiser;
                    break;
                case "servicestack":
                    serializer = ServiceStackSerialiser;
                    break;
                case "default":
                    serializer = DefaultJsonSerialiser;
                    break;
            }

            if (serializer == null)
            {
                throw new ApplicationException("Valid serializer required (jsonnet|servicestack|default)");
            }

            return serializer;
        }

        private static void JsonNetSerialiser(object objectToSerialize, HttpListenerResponse response)
        {
            var json = Newtonsoft.Json.JsonConvert.SerializeObject(objectToSerialize);

            WriteResponse(response, json, "application/json; charset=utf-8");
        }

        private static void ServiceStackSerialiser(object objectToSerialize, HttpListenerResponse response)
        {
            var json = ServiceStack.Text.JsonSerializer.SerializeToString(objectToSerialize);

            WriteResponse(response, json, "application/json; charset=utf-8");
        }

        private static void DefaultJsonSerialiser(object objectToSerialize, HttpListenerResponse response)
        {
            var json = new JavaScriptSerializer().Serialize(objectToSerialize);

            WriteResponse(response, json, "application/json; charset=utf-8");
        }

        private static void WriteResponse(HttpListenerResponse response, string responseString, string contentType)
        {
           var buffer = Encoding.UTF8.GetBytes(responseString);
           
            response.ContentType = contentType;
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

            var newMinWorkerThreads = Convert.ToInt32(ConfigurationManager.AppSettings["minWorkerThreadsPerLogicalProcessor"]);
            if (newMinWorkerThreads <= 0)
            {
                return;
            }

            int minWorkerThreads, minCompletionPortThreads;
            ThreadPool.GetMinThreads(out minWorkerThreads, out minCompletionPortThreads);
            ThreadPool.SetMinThreads(Environment.ProcessorCount * newMinWorkerThreads, minCompletionPortThreads);
        }

        public static void Main(string[] args)
        {
            Threads();

            var listener = new System.Net.HttpListener();
            // This doesn't seem to ignore all write exceptions, so in WriteResponse(), we still have a catch block.
            listener.IgnoreWriteExceptions = true;
            listener.Prefixes.Add("http://*:8080/");

            try
            {
                listener.Start();

                for (;;)
                {
                    HttpListenerContext context = null;
                    try
                    {
                        context = listener.GetContext();

                        ThreadPool.QueueUserWorkItem(RequestCallback, context);
                        context = null; // ownership has been transferred to RequestCallback
                    }
                    catch (HttpListenerException ex)
                    {
                        Console.WriteLine(ex.Message);
                    }
                    finally
                    {
                        if (context != null)
                        {
                            context.Response.Close();
                        }
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

        private static DbConnection CreateConnection(HttpListenerRequest request)
        {
            var providerName = request.QueryString["provider"];
            if (providerName == null)
            {
                throw new ApplicationException("Missing provider querystring argument");
            }

            var connectionSettings = ConfigurationManager.ConnectionStrings[providerName];
            var factory = DbProviderFactories.GetFactory(connectionSettings.ProviderName);
            var connection = factory.CreateConnection();

            if (connection != null)
            {
                connection.ConnectionString = connectionSettings.ConnectionString;
            }

            return connection;
        }

        private static int GetQueries(HttpListenerRequest request)
        {
            var queries = 1;
            var queriesString = request.QueryString["queries"];
            if (queriesString != null)
            {
                // If this fails to parse, queries will be set to zero.
                int.TryParse(queriesString, out queries);
                queries = Math.Max(1, Math.Min(500, queries));
            }
            return queries;
        }

        private static void NotFound(HttpListenerResponse response)
        {
            response.StatusCode = (int)HttpStatusCode.NotFound;
            WriteResponse(response, "not found", "text/plain; charset=utf-8");
        }

        private static void Error(HttpListenerResponse response, Exception exception)
        {
            response.StatusCode = (int)HttpStatusCode.InternalServerError;

            WriteResponse(response, exception.Message, "text/plain; charset=utf-8");
        }

        private static void Plaintext(HttpListenerResponse response)
        {
            WriteResponse(response, "Hello, World!", "text/plain; charset=utf-8");
        }

        private static void Json(HttpListenerResponse response, Action<object, HttpListenerResponse> serializer)
        {
            serializer.Invoke(new { message = "Hello, World!" }, response);
        }

        private static void Db(HttpListenerRequest request, HttpListenerResponse response, Action<object, HttpListenerResponse> serializer)
        {
            var random = new Random();

            var queries = GetQueries(request);
            var worlds = new List<World>(queries);

            using (var connection = CreateConnection(request))
            {
                connection.Open();

                using (var command = connection.CreateCommand())
                {
                    command.CommandText = "SELECT * FROM World WHERE id = @ID";

                    var parameter = command.CreateParameter();
                    parameter.ParameterName = "@ID";

                    command.Parameters.Add(parameter);

                    for (var i = 0; i < worlds.Capacity; i++)
                    {
                        var randomID = random.Next(0, 10000) + 1;

                        parameter.Value = randomID;

                        // Don't use CommandBehavior.SingleRow because that will make the MySql provider
                        // send two extra commands to limit the result to one row.
                        using (var reader = command.ExecuteReader())
                        {
                            if (reader.Read())
                            {
                                worlds.Add(
                                    new World
                                    {
                                        id = reader.GetInt32(0),
                                        randomNumber = reader.GetInt32(1)
                                    });
                            }
                        }
                    }
                }
            }

            serializer(
                worlds.Count > 1
                    ? worlds
                    : (Object)worlds[0],
                response);
        }

        private static void Fortunes(HttpListenerRequest request, HttpListenerResponse response)
        {
            var fortunes = new List<Fortune>();

            using (var connection = CreateConnection(request))
            {
                connection.Open();

                using (var command = connection.CreateCommand())
                {
                    command.CommandText = "SELECT * FROM Fortune";

                    using (var reader = command.ExecuteReader(CommandBehavior.SequentialAccess))
                    {
                        while (reader.Read())
                        {
                            fortunes.Add(
                                new Fortune
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

            WriteResponse(response, new Fortunes { Model = fortunes }.TransformText(), "text/html; charset=utf-8");
        }

        private static void Updates(HttpListenerRequest request, HttpListenerResponse response, Action<object, HttpListenerResponse> serializer)
        {
            var random = new Random();
            var worlds = new List<World>(GetQueries(request));

            using (var connection = CreateConnection(request))
            {
                connection.Open();

                using (DbCommand selectCommand = connection.CreateCommand(),
                                 updateCommand = connection.CreateCommand())
                {
                    selectCommand.CommandText = "SELECT * FROM World WHERE id = @ID";
                    updateCommand.CommandText = "UPDATE World SET randomNumber = @Number WHERE id = @ID";

                    for (var i = 0; i < worlds.Capacity; i++)
                    {
                        var randomID = random.Next(0, 10000) + 1;
                        var randomNumber = random.Next(0, 10000) + 1;

                        var idParameter = selectCommand.CreateParameter();
                        idParameter.ParameterName = "@ID";
                        idParameter.Value = randomID;

                        selectCommand.Parameters.Clear();
                        selectCommand.Parameters.Add(idParameter);

                        World world = null;

                        // Don't use CommandBehavior.SingleRow because that will make the MySql provider
                        // send two extra commands to limit the result to one row.
                        using (var reader = selectCommand.ExecuteReader())
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

                        var idUpdateParameter = updateCommand.CreateParameter();
                        idUpdateParameter.ParameterName = "@ID";
                        idUpdateParameter.Value = randomID;

                        var numberParameter = updateCommand.CreateParameter();
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

            serializer(
                worlds.Count > 1
                    ? worlds
                    : (Object)worlds[0],
                response);
        }

        private static void MongoDBDb(HttpListenerRequest request, HttpListenerResponse response, Action<object, HttpListenerResponse> serializer)
        {
            var random = new Random();

            var queries = GetQueries(request);
            var worlds = new List<World>(queries);

            var db = new Benchmarks.AspNet.Models.MongoDB("MongoDB");

            for (var i = 0; i < worlds.Capacity; i++)
            {
                var randomID = random.Next(0, 10000) + 1;
                worlds.Add(db.Worlds.FindOne(Query<World>.EQ(w => w.id, randomID)));
            }

            serializer(
                worlds.Count > 1
                    ? worlds
                    : (object)worlds[0],
                response);
        }

        private static void MongoDBFortunes(HttpListenerRequest request, HttpListenerResponse response)
        {
            var db = new Benchmarks.AspNet.Models.MongoDB("MongoDB");

            var fortunes = db.Fortunes.FindAll().ToList();

            fortunes.Add(new Fortune { ID = 0, Message = "Additional fortune added at request time." });
            fortunes.Sort();

            var template = new Fortunes { Model = fortunes };
            WriteResponse(response, template.TransformText(), "text/html; charset=utf-8");
        }

        private static void MongoDBUpdates(HttpListenerRequest request, HttpListenerResponse response, Action<object, HttpListenerResponse> serializer)
        {
            var random = new Random();

            var db = new Benchmarks.AspNet.Models.MongoDB("MongoDB");

            var queries = GetQueries(request);
            var worlds = new List<World>(queries);

            for (var i = 0; i < worlds.Capacity; i++)
            {
                var randomID = random.Next(0, 10000) + 1;
                var randomNumber = random.Next(0, 10000) + 1;

                World world = db.Worlds.FindOne(Query<World>.EQ(w => w.id, randomID));
                world.randomNumber = randomNumber;
                worlds.Add(world);

                db.Worlds.Save(world);
            }

            serializer(
                worlds.Count > 1
                    ? worlds
                    : (object)worlds[0],
                response);
        }
    }
}
