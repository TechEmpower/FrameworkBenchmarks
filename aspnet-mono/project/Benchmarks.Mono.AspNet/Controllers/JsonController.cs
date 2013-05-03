using System.Web.Mvc;

namespace Benchmarks.Mono.AspNet.Controllers
{
    public class JsonController : Controller
    {
        public ActionResult Index()
        {
            return Json(new { message = "Hello World" }, JsonRequestBehavior.AllowGet);
        }
    }
}
