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
    public class MySqlController : Controller
    {
        static Random random = new Random();

        public ActionResult Raw(int queries)
        {
            List<World> worlds = new List<World>();

            using (MySqlConnection connection = new MySqlConnection(ConfigurationManager.ConnectionStrings["MySQL"].ConnectionString))
            {
                connection.Open();

                using (MySqlCommand command = new MySqlCommand("SELECT * FROM World WHERE id = @ID", connection))
                {
                    for (int i = 0; i < queries; i++)
                    {
                        int randomID = random.Next(0, 10000) + 1;

                        command.Parameters.Clear();
                        command.Parameters.AddWithValue("@ID", randomID);

                        using (DbDataReader reader = command.ExecuteReader())
                        {
                            if (reader.Read())
                            {
                                World world = new World();
                                world.ID = reader.GetInt32(0);
                                world.RandomNumber = reader.GetInt32(1);

                                worlds.Add(world);
                            }
                        }
                    }
                }
            }

            return Json(worlds, JsonRequestBehavior.AllowGet);
        }
        
        /* public async Task<ActionResult> Raw(int queries)
        {
            List<World> worlds = new List<World>();

            using (MySqlConnection connection = new MySqlConnection(ConfigurationManager.ConnectionStrings["MySQL"].ConnectionString))
            {
                await connection.OpenAsync();

                using (MySqlCommand command = new MySqlCommand("SELECT * FROM World WHERE id = @ID", connection))
                {
                    for (int i = 0; i < queries; i++)
                    {
                        int randomID = random.Next(0, 10000) + 1;

                        command.Parameters.Clear();
                        command.Parameters.AddWithValue("@ID", randomID);

                        using (DbDataReader reader = await command.ExecuteReaderAsync())
                        {
                            if (await reader.ReadAsync())
                            {
                                World world = new World();
                                world.ID = reader.GetInt32(0);
                                world.RandomNumber = reader.GetInt32(1);

                                worlds.Add(world);
                            }
                        }
                    }
                }
            }

            return Json(worlds, JsonRequestBehavior.AllowGet);
        } */

        public ActionResult EntityFramework(int queries)
        {
            List<World> worlds = new List<World>();

            using (Entities db = new Entities())
            {
                for (int i = 0; i < queries; i++)
                {
                    int randomID = random.Next(0, 10000) + 1;
                    worlds.Add(db.World.Find(randomID));
                }
            }

            return Json(worlds, JsonRequestBehavior.AllowGet);
        }
    }
}
