using System.Web;
using System.Web.UI;
using System.Web.Compilation;

// This bit of magic lets us access /fortunes.aspx as /fortunes. This could be used for any URL specified
// in Web.config.
public class NoAspxHandlerFactory : IHttpHandlerFactory
{
    public virtual IHttpHandler GetHandler(HttpContext context, string requestType, string virtualPath, string path)
    {
        // virtualPath is like "/fortunes", turn that into "/fortunes.aspx"
        return BuildManager.CreateInstanceFromVirtualPath(virtualPath + ".aspx", typeof(Page)) as Page;
    }

    public virtual void ReleaseHandler(IHttpHandler handler)
    {
    }
}