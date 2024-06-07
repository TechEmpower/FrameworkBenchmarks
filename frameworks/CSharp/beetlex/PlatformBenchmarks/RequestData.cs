using System;
using System.Buffers;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public class RequestData 
    {

        public string QueryString { get; set; }

        public ActionType? Action { get; set; }


    }

    public enum ActionType
    {
        Plaintext,
        Json,
        Db,
        Queries,
        Caching,
        Updates,
        Fortunes,
        Other
    }
}
