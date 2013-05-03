using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data.Common;
using System.Threading.Tasks;
using System.Web.Mvc;

using MySql.Data.MySqlClient;

using Benchmarks.Mono.AspNet.Models;

namespace Benchmarks.Mono.AspNet.Controllers
{
    public class AdoNetMySqlController : Controller
    {
        static Random random = new Random();
        static string connectionString = ConfigurationManager.ConnectionStrings["MySQL"].ConnectionString;

        public ActionResult Index(int? queries)
        {
            List<World> worlds = new List<World>();

            using (MySqlConnection connection = new MySqlConnection(connectionString))
            {
                connection.Open();

                using (MySqlCommand command = new MySqlCommand("SELECT * FROM World WHERE id = @ID", connection))
                {
                    for (int i = 0; i < (queries ?? 1); i++)
                    {
                        int randomID = random.Next(0, 10000) + 1;

                        command.Parameters.Clear();
                        command.Parameters.AddWithValue("@ID", randomID);

                        using (DbDataReader reader = command.ExecuteReader())
                        {
                            if (reader.Read())
                            {
                                World world = new World();
                                world.id = reader.GetInt32(0);
                                world.randomNumber = reader.GetInt32(1);

                                worlds.Add(world);
                            }
                        }
                    }
                }
            }

            return queries != null ? Json(worlds, JsonRequestBehavior.AllowGet)
                                   : Json(worlds[0], JsonRequestBehavior.AllowGet);
        }

        public async Task<ActionResult> Async(int? queries)
        {
            List<World> worlds = new List<World>();

            using (MySqlConnection connection = new MySqlConnection(connectionString))
            {
                await connection.OpenAsync();

                using (MySqlCommand command = new MySqlCommand("SELECT * FROM World WHERE id = @ID", connection))
                {
                    for (int i = 0; i < (queries ?? 1); i++)
                    {
                        int randomID = random.Next(0, 10000) + 1;

                        command.Parameters.Clear();
                        command.Parameters.AddWithValue("@ID", randomID);

                        using (DbDataReader reader = await command.ExecuteReaderAsync())
                        {
                            if (await reader.ReadAsync())
                            {
                                World world = new World();
                                world.id = reader.GetInt32(0);
                                world.randomNumber = reader.GetInt32(1);

                                worlds.Add(world);
                            }
                        }
                    }
                }
            }

            return queries != null ? Json(worlds, JsonRequestBehavior.AllowGet)
                                   : Json(worlds[0], JsonRequestBehavior.AllowGet);
        }
    }
}