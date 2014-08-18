package hellowicket.fortune;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.sql.DataSource;

import hellowicket.WicketApplication;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;

/**
 * A page that loads all fortune cookies
 */
public class FortunePage extends WebPage
{
  public FortunePage() throws Exception
  {
    List<Fortune> fortunes = new ArrayList<>(10000);

    Fortune newFortune = new Fortune(0, "Additional fortune added at request time.");
    fortunes.add(newFortune);

    DataSource dataSource = WicketApplication.get().getDataSource();
    try (Connection connection = dataSource.getConnection())
    {
      try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM Fortune",
              ResultSet.TYPE_FORWARD_ONLY,
              ResultSet.CONCUR_READ_ONLY))
      {
          try (ResultSet resultSet = statement.executeQuery())
          {
              while (resultSet.next())
              {
                  fortunes.add(new Fortune(
                          resultSet.getInt("id"),
                          resultSet.getString("message")));
              }
          }
      }
    }

    Collections.sort(fortunes);

    ListView<Fortune> listView = new ListView<Fortune>("fortunes", fortunes)
    {
      @Override
      protected void populateItem(ListItem<Fortune> item)
      {
        Fortune fortune = item.getModelObject();
        item.add(new Label("id", fortune.id));
        item.add(new Label("message", fortune.message));
      }
    };
    add(listView);
  }
}
