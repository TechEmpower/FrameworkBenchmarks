using System;
using System.Web;
using System.Web.Mvc;

namespace Benchmarks.AspNet.Controllers
{
    public class JsonController : Controller
    {
        public ActionResult Default()
        {
            return new JsonResult { Data = new { message = "Hello World" } };
        }

        public ActionResult JsonNet()
        {
            return new JsonNetResult { Data = new { message = "Hello World" } };
        }

        public ActionResult ServiceStack()
        {
            return new ServiceStackResult { Data = new { message = "Hello World" } };
        }
    }

    public class JsonResult : ActionResult
    {
        public object Data
        {
            get;
            set;
        }

        public override void ExecuteResult(ControllerContext context)
        {
            if (context == null)
                throw new ArgumentNullException("context");

            HttpResponseBase response = context.HttpContext.Response;
            response.ContentType = "application/json";

            if (Data != null)
            {
                response.Write(Serialize());
            }
        }

        protected virtual string Serialize()
        {
            return new System.Web.Script.Serialization.JavaScriptSerializer().Serialize(Data);
        }
    }

    public class JsonNetResult : JsonResult
    {
        protected override string Serialize()
        {
            return Newtonsoft.Json.JsonConvert.SerializeObject(Data);
        }
    }

    public class ServiceStackResult : JsonResult
    {
        protected override string Serialize()
        {
            return ServiceStack.Text.JsonSerializer.SerializeToString(Data);
        }
    }
}
