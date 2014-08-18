package com.techempower.ee7.rest;

import javax.inject.Inject;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;

import org.jboss.logging.Logger;

@Provider
public class CatchAllExceptionMapper implements ExceptionMapper<Exception> {

  @Inject
  private Logger log;

  @Override
  public Response toResponse(Exception exception) {
    log.info("Request Failed: " + exception.getMessage());
    return Response.status(Status.BAD_REQUEST).build();
  }
}
