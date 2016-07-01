using System.Collections.Generic;
using System.IO;

namespace Revenj.Bench
{
	public partial class Fortunes
	{
		private readonly TextWriter writer;

		public Fortunes(List<KeyValuePair<int, string>> arg, TextWriter writer)
		{
			this._fortunesField = arg;
			this.writer = writer;
		}

		public new void Write(string what) { writer.Write(what); }
	}
}
