using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Text;
using System.Web;
using System.Web.Mvc;
using ServiceStack.Text;

namespace Benchmarks.AspNet
{
    /// <summary>
    /// Represents a class that is used to send JSON-formatted content to the response.
    /// Uses JSON.net for serialization.
    /// </summary>
    public class ServiceStackResult : JsonResult
    {
        public HttpStatusCode StatusCode { get; set; }
        public string StatusDescription  { get; set; }

        private const string GetNotAllowedMessage = "This request has been blocked because sensitive information could " +
                                                    "be disclosed to third party web sites when this is used in a GET " +
                                                    "request. To allow GET requests, set JsonRequestBehavior to AllowGet.";

        /// <summary>
        /// Initializes a new instance of the <see cref="ServiceStackResult"/> class.
        /// </summary>
        public ServiceStackResult()
        {
            StatusCode = HttpStatusCode.OK;
        }

        /// <summary>
        /// Enables processing of the result of an action method by a custom type that inherits from the <see cref="T:System.Web.Mvc.ActionResult"/> class.
        /// </summary>
        /// <param name="context">The context within which the result is executed.</param>
        /// <exception cref="T:System.ArgumentNullException">The <paramref name="context"/> parameter is null.</exception>
        public override void ExecuteResult(ControllerContext context)
        {
            bool allowGet    = JsonRequestBehavior == JsonRequestBehavior.AllowGet;
            bool isGetMethod = context.HttpContext.Request.HttpMethod.ToUpperInvariant() == "GET";

            if (isGetMethod && !allowGet)
                throw new InvalidOperationException(GetNotAllowedMessage);

            var response               = context.HttpContext.Response;
            response.ContentType       = ContentType ?? "application/json";
            response.ContentEncoding   = ContentEncoding ?? Encoding.UTF8;
            response.StatusCode        = (int) StatusCode;
            response.StatusDescription = StatusDescription ?? "Success";

            if (Data != null)
                response.Write(JsonSerializer.SerializeToString(Data));
        }
    }
}
