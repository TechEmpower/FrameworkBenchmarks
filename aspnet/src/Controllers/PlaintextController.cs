using System.Web.Mvc;

namespace Benchmarks.AspNet.Controllers
{
    public class PlaintextController : Controller
    {
        public ActionResult Index()
        {
            return Content("Hello, World!", "text/plain");
        }
    }
}