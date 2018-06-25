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
@SuiteClasses({ TestSuite.RapidoidJsonTest.class, TestSuite.RapidoidDbTest.class, TestSuite.RapidoidQueriesTest.class,
		TestSuite.RapidoidFortunesTest.class, TestSuite.RapidoidUpdateTest.class, TestSuite.RapidoidPlaintextTest.class })
public class TestSuite {

	private static final String SERVER_NAME = "OF Rapidoid";

	public static class RapidoidJsonTest extends JsonTest {
		@Override
		protected String getServerName() {
			return SERVER_NAME;
		}
	}

	public static class RapidoidDbTest extends DbTest {
		@Override
		protected String getServerName() {
			return SERVER_NAME;
		}
	}

	public static class RapidoidQueriesTest extends QueriesTest {
		@Override
		protected String getServerName() {
			return SERVER_NAME;
		}
	}

	public static class RapidoidFortunesTest extends FortunesTest {
		@Override
		protected String getServerName() {
			return SERVER_NAME;
		}
	}

	public static class RapidoidUpdateTest extends UpdateTest {
		@Override
		protected String getServerName() {
			return SERVER_NAME;
		}
	}

	public static class RapidoidPlaintextTest extends PlaintextTest {
		@Override
		protected String getServerName() {
			return SERVER_NAME;
		}
	}

}