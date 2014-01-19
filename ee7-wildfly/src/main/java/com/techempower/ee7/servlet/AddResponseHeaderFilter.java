package com.techempower.ee7.servlet;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import javax.servlet.DispatcherType;
import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.annotation.WebFilter;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;

@WebFilter(urlPatterns = "*", asyncSupported = true, dispatcherTypes = DispatcherType.REQUEST)
public class AddResponseHeaderFilter implements Filter {

  private static final String HEADER_SERVER_NAME = "Server";
  private static final String HEADER_SERVER_VALUE = "Wildfly, U-tow";
  private static final String HEADER_DATE_NAME = "Date";
  private static final SimpleDateFormat RFC822_DATE_FORMAT = getDateFormatRFC822();

  @Override
  public void init(FilterConfig filterConfig) throws ServletException {
    // Nothing
  }

  @Override
  public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
      throws IOException, ServletException {
    HttpServletResponseWrapper wrappedResponse =
        new HttpServletResponseWrapper((HttpServletResponse) response);
    wrappedResponse.addHeader(HEADER_DATE_NAME, RFC822_DATE_FORMAT.format(new Date()));
    wrappedResponse.addHeader(HEADER_SERVER_NAME, HEADER_SERVER_VALUE);
    chain.doFilter(request, response);
  }

  @Override
  public void destroy() {
    // Nothing
  }

  private static SimpleDateFormat getDateFormatRFC822() {
    SimpleDateFormat dateFormatRFC822 =
        new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US);
    dateFormatRFC822.setTimeZone(TimeZone.getTimeZone("GMT"));
    return dateFormatRFC822;
  }
}
