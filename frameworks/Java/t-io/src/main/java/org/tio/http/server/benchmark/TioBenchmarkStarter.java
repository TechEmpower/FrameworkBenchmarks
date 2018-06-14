package org.tio.http.server.benchmark;

import java.io.IOException;

import org.tio.http.server.benchmark.init.HttpServerInit;

/**
 * @author tanyaowu 
 * 2018年6月9日 上午10:30:45
 */
public class TioBenchmarkStarter {

	/**
	 * @param args
	 * @author tanyaowu
	 * @throws IOException
	 */
	public static void main(String[] args) throws Exception {
		HttpServerInit.init();
	}
}
