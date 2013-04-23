# Profiling

Kohana provides a very simple way to display statistics about your application:

1. Common [Kohana] method calls, such as [Kohana::find_file()].
2. Requests. Including the main request, as well as any sub-requests.
3. [Database] queries
4. Average execution times for your application

[!!]  In order for profiling to work, the `profile` setting must be `TRUE` in your [Kohana::init()] call in your bootstrap.

## Profiling your code

You can easily add profiling to your own functions and code.  This is done using the [Profiler::start()] function.  The first parameter is the group, the second parameter is the name of the benchmark.  

	public function foobar($input)
	{
		// Be sure to only profile if it's enabled
		if (Kohana::$profiling === TRUE)
		{
			// Start a new benchmark
			$benchmark = Profiler::start('Your Category', __FUNCTION__);
		}

		// Do some stuff

		if (isset($benchmark))
		{
			// Stop the benchmark
			Profiler::stop($benchmark);
		}

		return $something;
	}

## How to read the profiling report

The benchmarks are sorted into groups.  Each benchmark will show its name, how many times it was run (show in parenthesis after the benchmark name), and then the min, max, average, and total time and memory spent on that benchmark.  The total column will have shaded backgrounds to show the relative times between benchmarks in the same group.

At the very end is a group called "Application Execution".  This keeps track of how long each execution has taken.  The number in parenthesis is how many executions are being compared.  It shows the fastest, slowest, and average time and memory usage of the last several requsets.  The last box is the time and memory usage of the current request.

((This could use a picture of a profiler with some database queries, etc. with annotations to point out each area as just described.))

## Displaying the profiler

You can display or collect the current [profiler] statistics at any time:

    <?php echo View::factory('profiler/stats') ?>

## Preview

(This is the actual profiler stats for this page.)

{{profiler/stats}}