using System.Configuration;
using System.Web.Script.Serialization;

using MongoDB.Bson;
using MongoDB.Bson.Serialization;
using MongoDB.Driver;

namespace Benchmarks.AspNet.Models
{
    public class MongoDB
    {
        public MongoCollection<MongoWorld> Worlds { get; private set; }
        public MongoCollection<Fortune> Fortunes { get; private set; }

        static MongoDB()
        {
            BsonClassMap.RegisterClassMap<World>(m =>
            {
                m.MapProperty(w => w.id);
                m.MapProperty(w => w.randomNumber);
            });

            BsonClassMap.RegisterClassMap<MongoWorld>(m =>
            {
                m.MapIdProperty(w => w._id);
            });

            BsonClassMap.RegisterClassMap<Fortune>(m =>
            {
                m.MapProperty(f => f.ID).SetElementName("id");
                m.MapProperty(f => f.Message).SetElementName("message");
            });
        }

        public MongoDB(string connectionStringName)
        {
            string connectionString = ConfigurationManager.ConnectionStrings[connectionStringName].ConnectionString;

            MongoClient client = new MongoClient(connectionString);
            MongoServer server = client.GetServer();
            MongoDatabase database = server.GetDatabase("hello_world");

            Worlds = database.GetCollection<MongoWorld>("world");
            Fortunes = database.GetCollection<Fortune>("fortune");
        }
    }

    public class MongoWorld : World
    {
        [ScriptIgnore]
        public ObjectId _id { get; set; }
    }
}