using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Data.Common;
using System.Threading;
using System.Threading.Tasks;
using System.Web;
using System.Web.Script.Serialization;

using Benchmarks.AspNet.Models;

public class DbHandler : IHttpHandler
{
    bool IHttpHandler.IsReusable
    {
        get { return true; }
    }

    void IHttpHandler.ProcessRequest(HttpContext context)
    {
        Random random = new Random();

        int queries = Common.GetQueries(context.Request);
        List<World> worlds = new List<World>(queries);
        
        using (DbConnection connection = Common.CreateConnection(context.Request))
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
        
        HttpResponse response = context.Response;
        response.ContentType = "application/json";
        response.Write(new JavaScriptSerializer().Serialize(
            worlds.Count > 1 ? (Object)worlds : (Object)worlds[0]));
    }
}