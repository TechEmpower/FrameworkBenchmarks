package hello.web;

import hello.domain.*;

import java.io.*;
import java.util.*;
import java.util.concurrent.*;

import javax.servlet.http.*;

import org.hibernate.Session;
import org.hibernate.Transaction;

import org.springframework.http.*;
import org.springframework.http.converter.*;
import org.springframework.http.converter.json.*;
import org.springframework.http.server.*;
import org.springframework.stereotype.*;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

@Controller
public class HelloFortuneController
{

  @RequestMapping(value = "/fortunes")
  public ModelAndView fortunes()
  {
    
    final Session session = HibernateUtil.getSessionFactory().openSession();
    try
    {
      List fortunes = new ArrayList(session.createCriteria(Fortune.class).list());
      fortunes.add(new Fortune(0, "Additional fortune added at request time."));
      Collections.sort(fortunes);

      return new ModelAndView("fortunes", "fortunes", fortunes);
    }
    finally
    {
      session.close();
    }
  }
}
