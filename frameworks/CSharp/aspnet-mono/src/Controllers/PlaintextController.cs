using System.Text;
using System.Threading.Tasks;
using System.Web.Mvc;

namespace Benchmarks.AspNet.Controllers
{
    public class PlaintextController : Controller
    {
        private static readonly byte[] _helloWorldPayload = Encoding.UTF8.GetBytes("Hello, World!");
        private static readonly string _payloadLength = _helloWorldPayload.ToString();

        public Task Index()
        {
            var response = Response;
            response.StatusCode = 200;
            response.ContentType = "text/plain";
            var payloadLength = _helloWorldPayload.Length;
            response.Headers["Content-Length"] = _payloadLength;
            return response.OutputStream.WriteAsync(_helloWorldPayload, 0, payloadLength);
        }
    }
}