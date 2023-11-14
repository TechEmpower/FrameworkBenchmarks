package com.techempower.benchmark.pippo.handler;

import com.techempower.benchmark.pippo.BenchmarkUtils;
import com.techempower.benchmark.pippo.dao.Dao;
import com.techempower.benchmark.pippo.model.Fortune;
import ro.pippo.core.route.RouteContext;
import ro.pippo.core.route.RouteHandler;

import java.util.Comparator;
import java.util.List;

/**
 * <p>Test type 4: Fortunes</p>
 *
 * <p>Example request:</p>
 *
 * <pre>
 * GET /fortunes HTTP/1.1
 * Host: server
 * User-Agent: Mozilla/5.0 (X11; Linux x86_64) Gecko/20130501 Firefox/30.0 AppleWebKit/600.00 Chrome/30.0.0000.0 Trident/10.0 Safari/600.00
 * Cookie: uid=12345678901234567890; __utma=1.1234567890.1234567890.1234567890.1234567890.12; wd=2560x1600
 * Accept: text/html,application/xhtml+xml,application/xml;q=0.9,"*&#47;"/"*&#47;";q=0.8
 * Accept-Language: en-US,en;q=0.5
 * Connection: keep-alive
 * </pre>
 *
 * <p>Example response:</p>
 *
 * <pre>
 * HTTP/1.1 200 OK
 * Content-Length: 1196
 * Content-Type: text/html; charset=UTF-8
 * Server: Example
 * Date: Wed, 17 Apr 2013 12:00:00 GMT
 *
 * <!DOCTYPE html>
 * <html>
 *   <head>
 *     <title>Fortunes</title>
 *   </head>
 *   <body>
 *     <table>
 *       <tr>
 *         <th>id</th>
 *         <th>message</th>
 *       </tr>
 *       <tr>
 *         <td>11</td>
 *         <td>&lt;script&gt;alert(&quot;This should not be displayed in a browser alert
 * box.&quot;);&lt;/script&gt;</td>
 *       </tr>
 *       <tr>
 *         <td>4</td>
 *         <td>A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1</td>
 *       </tr>
 *       <tr>
 *         <td>5</td>
 *         <td>A computer program does what you tell it to do, not what you want it to do.</td>
 *       </tr>
 *       <tr>
 *         <td>2</td>
 *         <td>A computer scientist is someone who fixes things that aren&apos;t broken.</td>
 *       </tr>
 *       <tr>
 *         <td>8</td>
 *         <td>A list is only as strong as its weakest link. — Donald Knuth</td>
 *       </tr>
 *       <tr>
 *         <td>0</td>
 *         <td>Additional fortune added at request time.</td>
 *       </tr>
 *       <tr>
 *         <td>3</td>
 *         <td>After enough decimal places, nobody gives a damn.</td>
 *       </tr>
 *       <tr>
 *         <td>7</td>
 *         <td>Any program that runs right is obsolete.</td>
 *       </tr>
 *       <tr>
 *         <td>10</td>
 *         <td>Computers make very fast, very accurate mistakes.</td>
 *       </tr>
 *       <tr>
 *         <td>6</td>
 *         <td>Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen</td>
 *       </tr>
 *       <tr>
 *         <td>9</td>
 *         <td>Feature: A bug with seniority.</td>
 *       </tr>
 *       <tr>
 *         <td>1</td>
 *         <td>fortune: No such file or directory</td>
 *       </tr>
 *       <tr>
 *         <td>12</td>
 *         <td>フレームワークのベンチマーク</td>
 *       </tr>
 *     </table>
 *   </body>
 * </html>
 * </pre>
 */
public class Test4FortuneHandler implements RouteHandler<RouteContext> {

	public Test4FortuneHandler(Dao dao) {
		this.dao = dao;
	}

	@Override
	public void handle(RouteContext routeContext) {

		List<Fortune> fortunes = dao.getFortunes();
		fortunes.add(new Fortune(0, "Additional fortune added at request time."));
		fortunes.sort(Comparator.comparing(o -> o.message));

		routeContext.setLocal("fortunes", fortunes);
		routeContext
			.getResponse()
			.header(BenchmarkUtils.Header.SERVER, "Pippo")
			.render("fortune");

	}

	private final Dao dao;

}