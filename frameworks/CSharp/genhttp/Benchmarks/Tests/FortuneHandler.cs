using System.Collections.Generic;
using System.Threading.Tasks;
using System.Linq;
using System.Web;

using Microsoft.EntityFrameworkCore;

using GenHTTP.Api.Content;
using GenHTTP.Api.Content.Templating;
using GenHTTP.Api.Protocol;

using GenHTTP.Modules.IO;
using GenHTTP.Modules.Razor;

using Benchmarks.Model;

namespace Benchmarks.Tests
{

    #region Factory

    public class FortuneHandlerBuilder : IHandlerBuilder
    {

        public IHandler Build(IHandler parent)
        {
            return new FortuneHandler(parent);
        }

    }

    #endregion

    #region Supporting data structures

    public sealed class FortuneModel : BasicModel
    {

        public List<Fortune> Cookies { get; }

        public FortuneModel(IRequest request, IHandler handler, List<Fortune> cookies) : base(request, handler)
        {
            Cookies = cookies;
        }

    }

    #endregion

    public class FortuneHandler : IHandler, IPageRenderer
    {
        private static readonly FlexibleContentType CONTENT_TYPE = new FlexibleContentType("text/html; charset=utf-8");

        #region Get-/Setters

        public IHandler Parent { get; }

        private IHandler Page { get; }

        private IRenderer<TemplateModel> Template { get; }

        #endregion

        #region Initialization

        public FortuneHandler(IHandler parent)
        {
            Parent = parent;

            Page = ModRazor.Page(Resource.FromAssembly("Fortunes.html"), (r, h) => GetFortunes(r, h))
                           .Title("Fortunes")
                           .AddAssemblyReference<HttpUtility>()
                           .AddUsing("System.Web")
                           .Build(this);

            Template = ModRazor.Template<TemplateModel>(Resource.FromAssembly("Template.html")).Build();
        }

        #endregion

        #region Functionality

        public async ValueTask PrepareAsync()
        {
            await Page.PrepareAsync();
            await Template.PrepareAsync();
        }

        public ValueTask<ulong> CalculateChecksumAsync() => new(17);

        public IEnumerable<ContentElement> GetContent(IRequest request) => Enumerable.Empty<ContentElement>();

        public async ValueTask<string> RenderAsync(TemplateModel model) => await Template.RenderAsync(model);

        public async ValueTask<IResponse> HandleAsync(IRequest request)
        {
            var response = await Page.HandleAsync(request);

            response.ContentType = CONTENT_TYPE;

            return response;
        }

        private static async ValueTask<FortuneModel> GetFortunes(IRequest request, IHandler handler)
        {
            using var context = DatabaseContext.CreateNoTracking();

            var fortunes = await context.Fortune.ToListAsync().ConfigureAwait(false);

            fortunes.Add(new Fortune() { Message = "Additional fortune added at request time." });

            fortunes.Sort();

            return new FortuneModel(request, handler, fortunes);
        }

        #endregion

    }

}
