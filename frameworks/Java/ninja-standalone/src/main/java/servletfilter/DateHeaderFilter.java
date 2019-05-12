package servletfilter;

import java.io.IOException;
import java.util.Collection;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;

import ninja.Result;

public class DateHeaderFilter implements Filter {

	@Override
	public void init(FilterConfig filterConfig) throws ServletException {
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		chain.doFilter(request, response);
		if (response instanceof HttpServletResponse) {
			final HttpServletResponse res = ((HttpServletResponse) response);
			Collection<String> dateHeaders = res.getHeaders(Result.DATE);
			if (dateHeaders != null && dateHeaders.size() > 0) {
				//use the first value from the Date headers
				res.setHeader(Result.DATE, dateHeaders.iterator().next());
			}
		}
	}

	@Override
	public void destroy() {
	}

}
