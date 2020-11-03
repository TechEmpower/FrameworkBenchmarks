using System.Collections.Generic;
using System.Linq;

using GenHTTP.Api.Content;
using GenHTTP.Api.Content.Templating;
using GenHTTP.Api.Protocol;

using GenHTTP.Modules.IO;
using GenHTTP.Modules.Scriban;

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

    public class FortuneHandler : IHandler, IPageRenderer
    {

        #region Get-/Setters

        public IHandler Parent { get; }

        private IHandler Page { get; }

        private IRenderer<TemplateModel> Template { get; }

        #endregion

        #region Supporting data structures

        public class FortuneModel : PageModel
        {

            public List<Fortune> Cookies { get; }

            public FortuneModel(IRequest request, IHandler handler, List<Fortune> cookies) : base(request, handler)
            {
                Cookies = cookies;
            }

        }

        #endregion

        #region Initialization

        public FortuneHandler(IHandler parent)
        {
            Parent = parent;

            Page = ModScriban.Page(Data.FromResource("Fortunes.html"), (r, h) => GetFortunes(r, h))
                             .Title("Fortunes")
                             .Build(this);

            Template = ModScriban.Template<TemplateModel>(Data.FromResource("Template.html")).Build();
        }

        #endregion

        #region Functionality

        public IResponse Handle(IRequest request) => Page.Handle(request);

        public IEnumerable<ContentElement> GetContent(IRequest request) => Enumerable.Empty<ContentElement>();

        public IResponseBuilder Render(TemplateModel model)
        {
            return model.Request.Respond()
                                .Content(Template.Render(model))
                                .Header("Content-Type", "text/html; charset=utf-8");
        }

        private FortuneModel GetFortunes(IRequest request, IHandler handler)
        {
            using var context = DatabaseContext.Create();

            var fortunes = context.Fortune.ToList();

            fortunes.Add(new Fortune() { Message = "Additional fortune added at request time." });

            fortunes.Sort();

            return new FortuneModel(request, handler, fortunes);
        }

        #endregion

    }

}
