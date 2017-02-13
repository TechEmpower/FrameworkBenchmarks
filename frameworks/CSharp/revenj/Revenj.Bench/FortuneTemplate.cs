using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Web;
using Revenj.Http;

namespace Revenj.Bench
{
	public partial class Fortunes : IHtmlView
	{
		private readonly static StringBuilder Fake = new StringBuilder(0);
		public Fortunes(List<KeyValuePair<int, string>> arg)
		{
			this._fortunesField = arg;
			this.GenerationEnvironment = Fake;
		}

		private new void Write(string what) { writer.Write(what); }

		public void Show(int what) { writer.Write(what); }
		public void Show(string what) { HttpUtility.HtmlEncode(what, writer); }

		private TextWriter writer;

		public void Render(TextWriter writer)
		{
			this.writer = writer;
			TransformText();
		}
	}
}
