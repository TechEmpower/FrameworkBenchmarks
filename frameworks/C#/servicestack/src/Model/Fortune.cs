using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Data;
using System.Linq;

using MongoDB.Driver;
using MongoDB.Driver.Builders;

using ServiceStack.DataAnnotations;
using ServiceStack.OrmLite;
using ServiceStack.Text;

namespace ServiceStackBenchmark.Model
{
    [Alias("Fortune")]
    public class Fortune : IComparable<Fortune>
    {
        [PrimaryKey()]
        public int id { get; set; }

        [StringLength(100)]
        public string message { get; set; }

        public int CompareTo(Fortune fortune)
        {
            return message.CompareTo(fortune.message);
        }

    }

    public static class FortuneMethods
    {

        public static List<Fortune> GetFortunes(this IDbConnection db)
        {
            return db.Select<Fortune>();
        }

        public static List<Fortune> GetFortunes(this MongoDatabase db)
        {
            var collection = db.GetCollection<Fortune>("Fortune");
            return collection.FindAll().ToList();
        }

        public static bool CreateFortuneTable(this IDbConnection db)
        {
            if (db.TableExists("Fortune"))
                return true;

            try
            {
                db.CreateTable<Fortune>();

                // Populate the collection
                db.Insert<Fortune>(GetFortunes().ToArray());

                return true;
            }
            catch
            {
                return false;
            }
        }

        public static bool CreateFortuneTable(this MongoDatabase db)
        {
            if (db.CollectionExists("Fortune"))
                return true;

            try
            {
                // Populate the collection
                var collection = db.GetCollection<Fortune>("Fortune");
                collection.InsertBatch(GetFortunes());

                return true;
            }
            catch
            {
                return false;
            }
        }

        private static IEnumerable<Fortune> GetFortunes()
        {
            var fortunes = new List<Fortune>();
            fortunes.Add(new Fortune() { id = 1, message = "fortune: No such file or directory" });
            fortunes.Add(new Fortune() { id = 2, message = "A computer scientist is someone who fixes things that aren't broken." });
            fortunes.Add(new Fortune() { id = 3, message = "After enough decimal places, nobody gives a damn." });
            fortunes.Add(new Fortune() { id = 4, message = "A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1" });
            fortunes.Add(new Fortune() { id = 5, message = "A computer program does what you tell it to do, not what you want it to do." });
            fortunes.Add(new Fortune() { id = 6, message = "Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen" });
            fortunes.Add(new Fortune() { id = 7, message = "Any program that runs right is obsolete." });
            fortunes.Add(new Fortune() { id = 8, message = "A list is only as strong as its weakest link. — Donald Knuth" });
            fortunes.Add(new Fortune() { id = 9, message = "Feature: A bug with seniority." });
            fortunes.Add(new Fortune() { id = 10, message = "Computers make very fast, very accurate mistakes." });
            fortunes.Add(new Fortune() { id = 11, message = "<script>alert(\"This should not be displayed in a browser alert box.\");</script>" });
            fortunes.Add(new Fortune() { id = 12, message = "フレームワークのベンチマーク" });
            return fortunes;
        }

        public static string ToHtml(List<Fortune> fortunes)
        {
            string page = @"<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
            fortunes.ForEach(f => page += @"<tr><td>{0}</td><td>{1}</td></tr>".Fmt(f.id, f.message));
            page += @"</table></body></html>";
            return page;
        }

    }
}
