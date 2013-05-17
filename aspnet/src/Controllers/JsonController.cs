using System.Web.Mvc;

namespace Benchmarks.AspNet.Controllers
{
    public class JsonController : Controller
    {
        public ActionResult Index()
        {
            return new JsonResult
            {
                Data = new { message = "Hello World" },
                JsonRequestBehavior = JsonRequestBehavior.AllowGet
            };
        }

        public ActionResult JsonNet()
        {
            return new JsonNetResult
            {
                Data = new { message = "Hello World" },
                JsonRequestBehavior = JsonRequestBehavior.AllowGet
            };
        }

        public ActionResult ServiceStack()
        {
            return new ServiceStackResult
            {
                Data = new { message = "Hello World" },
                JsonRequestBehavior = JsonRequestBehavior.AllowGet
            };
        }
    }
}
