package net.officefloor.benchmark;
/*
 * OfficeFloor - http://www.officefloor.net
 * Copyright (C) 2005-2018 Daniel Sagenschneider
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

/**
 * Tests.
 */
@RunWith(Suite.class)
@SuiteClasses({ TestSuite.NettyJsonTest.class, TestSuite.NettyDbTest.class, TestSuite.NettyQueriesTest.class,
		TestSuite.NettyFortunesTest.class, TestSuite.NettyUpdateTest.class, TestSuite.NettyPlaintextTest.class })
public class TestSuite {

	private static final String SERVER_NAME = "OF Netty";

	public static class NettyJsonTest extends JsonTest {
		@Override
		protected String getServerName() {
			return SERVER_NAME;
		}
	}

	public static class NettyDbTest extends DbTest {
		@Override
		protected String getServerName() {
			return SERVER_NAME;
		}
	}

	public static class NettyQueriesTest extends QueriesTest {
		@Override
		protected String getServerName() {
			return SERVER_NAME;
		}
	}

	public static class NettyFortunesTest extends FortunesTest {
		@Override
		protected String getServerName() {
			return SERVER_NAME;
		}
	}

	public static class NettyUpdateTest extends UpdateTest {
		@Override
		protected String getServerName() {
			return SERVER_NAME;
		}
	}

	public static class NettyPlaintextTest extends PlaintextTest {
		@Override
		protected String getServerName() {
			return SERVER_NAME;
		}
	}

}