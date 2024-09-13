using CodeBehind;
using Microsoft.EntityFrameworkCore;

public partial class fortunes : CodeBehindController
{
    public async void PageLoad(HttpContext context)
    {
        context.Response.ContentType = "text/html; charset=UTF-8";

        await LoadFortunesRows();
    }

    private readonly Func<DatabaseContext, IEnumerable<FortuneRow>> _FortunesQuery
    = EF.CompileQuery((DatabaseContext context) => context.Fortune);

    private async Task<string> LoadFortunesRows()
    {
        FortunesModel model = new FortunesModel();
        var dbc = new DatabaseContext();

        foreach (var fortune in _FortunesQuery(dbc))
            model.Fortune.Add(fortune);

        model.Fortune.Add(new FortuneRow { message = "Additional fortune added at request time." });

        model.Fortune.Sort();
        View(model);
        return "";
    }
}
