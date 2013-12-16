package com.techempower.spring.web;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.techempower.spring.domain.World;
import com.techempower.spring.repository.WorldRepository;

@RestController
final class WorldDatabaseController {

	private static final int DB_ROWS = 10000;

	@Autowired
	private WorldRepository worldRepository;

	@RequestMapping(value = "/db", produces = "application/json")
	World singleQuery() {
		final Random random = ThreadLocalRandom.current();
		return this.worldRepository.findOne(random.nextInt(DB_ROWS) + 1);
	}

	@RequestMapping(value = "/queries", produces = "application/json")
	List<World> multipleQueries(@RequestParam("queries") Integer rawQueryCount) {
		Integer queryCount = boundQueryCount(rawQueryCount);

		List<World> worlds = new ArrayList<World>(queryCount);
		Random random = ThreadLocalRandom.current();

		for (int i = 0; i < queryCount; i++) {
			worlds.add(this.worldRepository.findOne(random.nextInt(DB_ROWS) + 1));
		}

		return worlds;
	}

	@RequestMapping(value = "/updates", produces = "application/json")
	List<World> updateQueries(@RequestParam(value="queries", required=false, defaultValue="1") Integer rawQueryCount) {
		Integer queryCount = boundQueryCount(rawQueryCount);

		List<World> worlds = new ArrayList<World>(queryCount);
		Random random = ThreadLocalRandom.current();

		for (int i = 0; i < queryCount; i++) {
			World world = this.worldRepository.findOne(random.nextInt(DB_ROWS) + 1);
			world.setRandomNumber(random.nextInt(DB_ROWS) + 1);
			this.worldRepository.save(world);
			worlds.add(world);
		}

		return worlds;
	}

	private Integer boundQueryCount(Integer raw) {
		if (raw == null || raw < 1) {
			return 1;
		} else if (raw > 500) {
			return 500;
		}

		return raw;
	}

	private static class ThreadLocalRandom extends Random {
		// same constants as Random, but must be redeclared because private
		private static final long multiplier = 0x5DEECE66DL;
		private static final long addend = 0xBL;
		private static final long mask = (1L << 48) - 1;

		/**
		 * The random seed. We can't use super.seed.
		 */
		private long rnd;

		/**
		 * Initialization flag to permit calls to setSeed to succeed only while executing
		 * the Random constructor. We can't allow others since it would cause setting seed
		 * in one part of a program to unintentionally impact other usages by the thread.
		 */
		boolean initialized;

		// Padding to help avoid memory contention among seed updates in
		// different TLRs in the common case that they are located near
		// each other.
		private long pad0, pad1, pad2, pad3, pad4, pad5, pad6, pad7;

		/**
		 * The actual ThreadLocal
		 */
		private static final ThreadLocal<ThreadLocalRandom> localRandom = new ThreadLocal<ThreadLocalRandom>() {
			protected ThreadLocalRandom initialValue() {
				return new ThreadLocalRandom();
			}
		};

		/**
		 * Constructor called only by localRandom.initialValue.
		 */
		ThreadLocalRandom() {
			super();
			initialized = true;
		}

		/**
		 * Returns the current thread's {@code ThreadLocalRandom}.
		 * 
		 * @return the current thread's {@code ThreadLocalRandom}
		 */
		public static ThreadLocalRandom current() {
			return localRandom.get();
		}

		/**
		 * Throws {@code UnsupportedOperationException}. Setting seeds in this generator
		 * is not supported.
		 * 
		 * @throws UnsupportedOperationException always
		 */
		public void setSeed(long seed) {
			if (initialized)
				throw new UnsupportedOperationException();
			rnd = (seed ^ multiplier) & mask;
		}

		protected int next(int bits) {
			rnd = (rnd * multiplier + addend) & mask;
			return (int) (rnd >>> (48 - bits));
		}

		/**
		 * Returns a pseudorandom, uniformly distributed value between the given least
		 * value (inclusive) and bound (exclusive).
		 * 
		 * @param least the least value returned
		 * @param bound the upper bound (exclusive)
		 * @throws IllegalArgumentException if least greater than or equal to bound
		 * @return the next value
		 */
		public int nextInt(int least, int bound) {
			if (least >= bound)
				throw new IllegalArgumentException();
			return nextInt(bound - least) + least;
		}

		/**
		 * Returns a pseudorandom, uniformly distributed value between 0 (inclusive) and
		 * the specified value (exclusive).
		 * 
		 * @param n the bound on the random number to be returned. Must be positive.
		 * @return the next value
		 * @throws IllegalArgumentException if n is not positive
		 */
		public long nextLong(long n) {
			if (n <= 0)
				throw new IllegalArgumentException("n must be positive");
			// Divide n by two until small enough for nextInt. On each
			// iteration (at most 31 of them but usually much less),
			// randomly choose both whether to include high bit in result
			// (offset) and whether to continue with the lower vs upper
			// half (which makes a difference only if odd).
			long offset = 0;
			while (n >= Integer.MAX_VALUE) {
				int bits = next(2);
				long half = n >>> 1;
				long nextn = ((bits & 2) == 0) ? half : n - half;
				if ((bits & 1) == 0)
					offset += n - nextn;
				n = nextn;
			}
			return offset + nextInt((int) n);
		}

		/**
		 * Returns a pseudorandom, uniformly distributed value between the given least
		 * value (inclusive) and bound (exclusive).
		 * 
		 * @param least the least value returned
		 * @param bound the upper bound (exclusive)
		 * @return the next value
		 * @throws IllegalArgumentException if least greater than or equal to bound
		 */
		public long nextLong(long least, long bound) {
			if (least >= bound)
				throw new IllegalArgumentException();
			return nextLong(bound - least) + least;
		}

		/**
		 * Returns a pseudorandom, uniformly distributed {@code double} value between 0
		 * (inclusive) and the specified value (exclusive).
		 * 
		 * @param n the bound on the random number to be returned. Must be positive.
		 * @return the next value
		 * @throws IllegalArgumentException if n is not positive
		 */
		public double nextDouble(double n) {
			if (n <= 0)
				throw new IllegalArgumentException("n must be positive");
			return nextDouble() * n;
		}

		/**
		 * Returns a pseudorandom, uniformly distributed value between the given least
		 * value (inclusive) and bound (exclusive).
		 * 
		 * @param least the least value returned
		 * @param bound the upper bound (exclusive)
		 * @return the next value
		 * @throws IllegalArgumentException if least greater than or equal to bound
		 */
		public double nextDouble(double least, double bound) {
			if (least >= bound)
				throw new IllegalArgumentException();
			return nextDouble() * (bound - least) + least;
		}

		private static final long serialVersionUID = -5851777807851030925L;
	}

}
