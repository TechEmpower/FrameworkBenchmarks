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
package net.officefloor.benchmark;

import org.junit.After;
import org.junit.Before;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

/**
 * Tests.
 */
@RunWith(Suite.class)
@SuiteClasses({ TestSuite.RawJsonTest.class, TestSuite.RawPlaintextTest.class })
public class TestSuite {

	public static void start() throws Exception {
		RawOfficeFloorMain.main(new String[0]);
	}

	public static void stop() throws Exception {
		RawOfficeFloorMain.socketManager.shutdown();
	}

	public static class RawJsonTest extends JsonTest {
		@Before
		public void start() throws Exception {
			TestSuite.start();
		}

		@After
		public void stop() throws Exception {
			TestSuite.stop();
		}
	}

	public static class RawPlaintextTest extends PlaintextTest {
		@Before
		public void start() throws Exception {
			TestSuite.start();
		}

		@After
		public void stop() throws Exception {
			TestSuite.stop();
		}
	}
}