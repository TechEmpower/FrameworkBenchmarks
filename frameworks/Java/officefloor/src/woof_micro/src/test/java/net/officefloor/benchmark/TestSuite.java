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

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import net.officefloor.benchmark.DbTest;
import net.officefloor.benchmark.FortunesTest;
import net.officefloor.benchmark.JsonTest;
import net.officefloor.benchmark.PlaintextTest;
import net.officefloor.benchmark.QueriesTest;
import net.officefloor.benchmark.UpdateTest;

/**
 * Tests.
 */
@RunWith(Suite.class)
@SuiteClasses({ JsonTest.class, DbTest.class, QueriesTest.class, FortunesTest.class, UpdateTest.class,
		PlaintextTest.class })
public class TestSuite {
}