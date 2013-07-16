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

public class UpdatesHandler : IHttpHandler
{
    bool IHttpHandler.IsReusable
    {
        get { return true; }
    }

    void IHttpHandler.ProcessRequest(HttpContext context)
    {
        Random random = new Random();
        List<World> worlds = new List<World>(Common.GetQueries(context.Request));

        using (DbConnection connection = Common.CreateConnection(context.Request))
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

        HttpResponse response = context.Response;
        response.ContentType = "application/json";
        response.Write(new JavaScriptSerializer().Serialize(
            worlds.Count > 1 ? (Object)worlds : (Object)worlds[0]));
    }
}