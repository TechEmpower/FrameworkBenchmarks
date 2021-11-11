package com.example.helloworld.resources;

import io.dropwizard.hibernate.UnitOfWork;

import java.util.Collections;
import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import com.example.helloworld.db.FortuneDAO;
import com.example.helloworld.db.model.Fortune;
import com.example.helloworld.resources.views.FortuneView;

@Path("/fortunes")
@Produces(MediaType.TEXT_HTML + ";charset=UTF-8")
public class FortuneResource {

	private final FortuneDAO fortuneDAO;

	public FortuneResource(FortuneDAO fortuneDAO) {
		this.fortuneDAO = fortuneDAO;
	}

	@GET
	@UnitOfWork // Needed only for Hibernate - not for Mongo or JDBI
	public FortuneView dbTest() {
		final List<Fortune> fortunes = fortuneDAO.list();

		fortunes.add(new Fortune("Additional fortune added at request time."));

		Collections.sort(fortunes);
		return new FortuneView(fortunes);
	}
}
