using System;
using System.Collections;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Data.Common;
using System.Linq;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.HtmlControls;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Xml.Linq;
using System.Runtime.Serialization.Json;
using System.IO;
using System.Text;

using Benchmarks.AspNet.Models;

public partial class FortunesPage : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        List<Fortune> fortunes = new List<Fortune>();
        
        using (DbConnection connection = Common.CreateConnection(Request))
        {
            connection.Open();
            
            using (DbCommand command = connection.CreateCommand())
            {
                command.CommandText = "SELECT * FROM Fortune";
                
                using (DbDataReader reader = command.ExecuteReader(CommandBehavior.SequentialAccess))
                {
                    while (reader.Read())
                    {
                        fortunes.Add(new Fortune
                        {
                            ID = reader.GetInt32(0),
                            Message = reader.GetString(1)
                        });
                    }
                }
            }
        }
        
        fortunes.Add(new Fortune { ID = 0, Message = "Additional fortune added at request time." });
        fortunes.Sort();
        
        Fortunes = fortunes;
    }

    public List<Fortune> Fortunes
    {
        get; set;
    }
}
