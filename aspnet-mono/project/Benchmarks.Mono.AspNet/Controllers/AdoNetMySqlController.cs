using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Data.Common;
using System.Web.Mvc;

using MySql.Data.MySqlClient;

using Benchmarks.Mono.AspNet.Models;

namespace Benchmarks.Mono.AspNet.Controllers
{
    public class AdoNetMySqlController : Controller
    {
        private static string connectionString = ConfigurationManager.ConnectionStrings["MySQL"].ConnectionString;
        
        public ActionResult Index(int? queries)
        {
            List<World> worlds = new List<World>(queries ?? 1);
            
            using (MySqlConnection connection = new MySqlConnection(connectionString))
            {
                connection.Open();
                
                using (MySqlCommand command = new MySqlCommand("SELECT * FROM World WHERE id = @ID", connection))
                {
                    Random random = new Random();
                    
                    for (int i = 0; i < worlds.Capacity; i++)
                    {
                        int randomID = random.Next(0, 10000) + 1;
                        
                        command.Parameters.Clear();
                        command.Parameters.AddWithValue("@ID", randomID);
                        
                        using (DbDataReader reader = command.ExecuteReader(CommandBehavior.SingleRow))
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
        
        public ActionResult Fortunes()
        {
            List<Fortune> fortunes = new List<Fortune>();
            
            using (MySqlConnection connection = new MySqlConnection(connectionString))
            {
                connection.Open();
                
                using (MySqlCommand command = new MySqlCommand("SELECT * FROM Fortune", connection))
                {
                    using (DbDataReader reader = command.ExecuteReader(CommandBehavior.SequentialAccess))
                    {
                        while (reader.Read())
                        {
                            Fortune fortune = new Fortune
                            {
                                ID = reader.GetInt32(0),
                                Message = reader.GetString(1)
                            };
                            
                            fortunes.Add(fortune);
                        }
                    }
                }
            }
            
            fortunes.Add(new Fortune { ID = 0, Message = "Additional fortune added at request time." });
            fortunes.Sort();
            
            return View(fortunes);
        }
    }
}