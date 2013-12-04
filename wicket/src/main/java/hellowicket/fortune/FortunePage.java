package hellowicket.fortune;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.hibernate.Query;
import org.hibernate.Session;

import hellowicket.HibernateUtil;

/**
 * A page that loads all fortune cookies
 */
public class FortunePage extends WebPage
{
  public FortunePage()
  {
    Session session = HibernateUtil.getSessionFactory().openSession();

    Query query = session.createQuery("from Fortune");
    query.setReadOnly(true);
    List list = query.list();
    List<Fortune> fortunes = new ArrayList<Fortune>(list);
    session.close();

    Fortune newFortune = new Fortune();
    newFortune.message = "Additional fortune added at request time.";
    fortunes.add(newFortune);

    sort(fortunes);

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

  private void sort(List<Fortune> fortunes)
  {
    Collections.sort(fortunes, new Comparator<Fortune>()
    {
      @Override
      public int compare(Fortune f1, Fortune f2)
      {
        return f1.message.compareTo(f2.message);
      }
    });
  }
}
