# Fragments

Fragments are a quick and simple way to cache HTML or other output.  Fragments are not useful for caching objects or raw database results, in which case you should use a more robust caching method, which can be achieved with the [Cache module](../cache). Fragments use [Kohana::cache()] and will be placed in the cache directory (`application/cache` by default).

You should use Fragment (or any caching solution) when reading the cache is faster than reprocessing the result.  Reading and parsing a remote file, parsing a complicated template, calculating something, etc.

Fragments are typically used in view files.

## Usage

Fragments are used by calling [Fragment::load()] in an `if` statement at the beginning of what you want cached, and [Fragment::save()] at the end.  They use [output buffering](http://www.php.net/manual/en/function.ob-start.php) to capture the output between the two function calls.

You can specify the lifetime (in seconds) of the Fragment using the second parameter of [Fragment::load()].  The default lifetime is 30 seconds.  You can use the [Date] helper to make more readable times.

Fragments will store a different cache for each language (using [I18n]) if you pass `true` as the third parameter to [Fragment::load()];

You can force the deletion of a Fragment using [Fragment::delete()], or specify a lifetime of 0.

~~~
// Cache for 5 minutes, and cache each language
if ( ! Fragment::load('foobar', Date::MINUTE * 5, true))
{
    // Anything that is echo'ed here will be saved
    Fragment::save();
}
~~~

## Example: Calculating Pi

In this example we will calculate pi to 1000 places, and cache the result using a fragment.  The first time you run this it will probably take a few seconds, but subsequent loads will be much faster, until the fragment lifetime runs out.

~~~
if ( ! Fragment::load('pi1000', Date::HOUR * 4))
{   
    // Change function nesting limit
    ini_set('xdebug.max_nesting_level',1000);
    
    // Source: http://mgccl.com/2007/01/22/php-calculate-pi-revisited
    function bcfact($n)
    {
      return ($n == 0 || $n== 1) ? 1 : bcmul($n,bcfact($n-1));
    }
    function bcpi($precision)
    {
        $num = 0;$k = 0;
        bcscale($precision+3);
        $limit = ($precision+3)/14;
        while($k < $limit)
        {
            $num = bcadd($num, bcdiv(bcmul(bcadd('13591409',bcmul('545140134', $k)),bcmul(bcpow(-1, $k), bcfact(6*$k))),bcmul(bcmul(bcpow('640320',3*$k+1),bcsqrt('640320')), bcmul(bcfact(3*$k), bcpow(bcfact($k),3)))));
            ++$k;
        }
        return bcdiv(1,(bcmul(12,($num))),$precision);
    }
    
    echo bcpi(1000);
    
	Fragment::save();
}

echo View::factory('profiler/stats');

?>
~~~

## Example: Recent Wikipedia edits

In this example we will use the [Feed] class to retrieve and parse an RSS feed of recent edits to [http://en.wikipedia.org](http://en.wikipedia.org), then use Fragment to cache the results.

~~~
$feed = "http://en.wikipedia.org/w/index.php?title=Special:RecentChanges&feed=rss";
$limit = 50;

// Displayed feeds are cached for 30 seconds (default)
if ( ! Fragment::load('rss:'.$feed)):

	// Parse the feed
	$items = Feed::parse($feed, $limit);
	
	foreach ($items as $item):
	
		// Convert $item to object
		$item = (object) $item;
		
		echo HTML::anchor($item->link,$item->title);
		
		?>
		<blockquote>
			<p>author: <?php echo $item->creator ?></p>
			<p>date: <?php echo $item->pubDate ?></p>
		</blockquote>
		<?php
		
	endforeach;

	Fragment::save();

endif;

echo View::factory('profiler/stats');
~~~

## Example: Nested Fragments

You can nest fragments with different lifetimes to provide more specific control.  For example, let's say your page has lots of dynamic content so we want to cache it with a lifetime of five minutes, but one of the pieces takes much longer to generate, and only changes every hour anyways. No reason to generate it every 5 minutes, so we will use a nested fragment.

[!!] If a nested fragment has a shorter lifetime than the parent, it will only get processed when the parent has expired.

~~~
// Cache homepage for five minutes
if ( ! Fragment::load('homepage', Date::MINUTE * 5)):

	echo "<p>Home page stuff</p>";
	
	// Pretend like we are actually doing something :)
	sleep(2);
	
	// Cache this every hour since it doesn't change as often
	if ( ! Fragment::load('homepage-subfragment', Date::HOUR)):
	
		echo "<p>Home page special thingy</p>";
		
		// Pretend like this takes a long time
		sleep(5);
		
	Fragment::save(); endif;
	
	echo "<p>More home page stuff</p>";
	
	Fragment::save();

endif;

echo View::factory('profiler/stats');
~~~