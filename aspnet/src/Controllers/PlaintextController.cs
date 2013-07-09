using System.Web.Mvc;

namespace Benchmarks.AspNet.Controllers
{
    [SessionState(SessionStateBehavior.Disabled)]
    public class PlaintextController : Controller
    {
        public ActionResult Index()
        {
            return Content("Hello, World!", "text/plain");
        }
    }
}
