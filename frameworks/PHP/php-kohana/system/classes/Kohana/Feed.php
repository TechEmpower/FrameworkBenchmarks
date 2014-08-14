<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * RSS and Atom feed helper.
 *
 * @package    Kohana
 * @category   Helpers
 * @author     Kohana Team
 * @copyright  (c) 2007-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_Feed {

	/**
	 * Parses a remote feed into an array.
	 *
	 * @param   string  $feed   remote feed URL
	 * @param   integer $limit  item limit to fetch
	 * @return  array
	 */
	public static function parse($feed, $limit = 0)
	{
		// Check if SimpleXML is installed
		if ( ! function_exists('simplexml_load_file'))
			throw new Kohana_Exception('SimpleXML must be installed!');

		// Make limit an integer
		$limit = (int) $limit;

		// Disable error reporting while opening the feed
		$error_level = error_reporting(0);

		// Allow loading by filename or raw XML string
		if (Valid::url($feed))
		{
			// Use native Request client to get remote contents
			$response = Request::factory($feed)->execute();
			$feed     = $response->body();
		}
		elseif (is_file($feed))
		{
			// Get file contents
			$feed = file_get_contents($feed);
		}

		// Load the feed
		$feed = simplexml_load_string($feed, 'SimpleXMLElement', LIBXML_NOCDATA);

		// Restore error reporting
		error_reporting($error_level);

		// Feed could not be loaded
		if ($feed === FALSE)
			return array();

		$namespaces = $feed->getNamespaces(TRUE);

		// Detect the feed type. RSS 1.0/2.0 and Atom 1.0 are supported.
		$feed = isset($feed->channel) ? $feed->xpath('//item') : $feed->entry;

		$i = 0;
		$items = array();

		foreach ($feed as $item)
		{
			if ($limit > 0 AND $i++ === $limit)
				break;
			$item_fields = (array) $item;

			// get namespaced tags
			foreach ($namespaces as $ns)
			{
				$item_fields += (array) $item->children($ns);
			}
			$items[] = $item_fields;
		}

		return $items;
	}

	/**
	 * Creates a feed from the given parameters.
	 *
	 * @param   array   $info       feed information
	 * @param   array   $items      items to add to the feed
	 * @param   string  $encoding   define which encoding to use
	 * @return  string
	 */
	public static function create($info, $items, $encoding = 'UTF-8')
	{
		$info += array('title' => 'Generated Feed', 'link' => '', 'generator' => 'KohanaPHP');

		$feed = '<?xml version="1.0" encoding="'.$encoding.'"?><rss version="2.0"><channel></channel></rss>';
		$feed = simplexml_load_string($feed);

		foreach ($info as $name => $value)
		{
			if ($name === 'image')
			{
				// Create an image element
				$image = $feed->channel->addChild('image');

				if ( ! isset($value['link'], $value['url'], $value['title']))
				{
					throw new Kohana_Exception('Feed images require a link, url, and title');
				}

				if (strpos($value['link'], '://') === FALSE)
				{
					// Convert URIs to URLs
					$value['link'] = URL::site($value['link'], 'http');
				}

				if (strpos($value['url'], '://') === FALSE)
				{
					// Convert URIs to URLs
					$value['url'] = URL::site($value['url'], 'http');
				}

				// Create the image elements
				$image->addChild('link', $value['link']);
				$image->addChild('url', $value['url']);
				$image->addChild('title', $value['title']);
			}
			else
			{
				if (($name === 'pubDate' OR $name === 'lastBuildDate') AND (is_int($value) OR ctype_digit($value)))
				{
					// Convert timestamps to RFC 822 formatted dates
					$value = date('r', $value);
				}
				elseif (($name === 'link' OR $name === 'docs') AND strpos($value, '://') === FALSE)
				{
					// Convert URIs to URLs
					$value = URL::site($value, 'http');
				}

				// Add the info to the channel
				$feed->channel->addChild($name, $value);
			}
		}

		foreach ($items as $item)
		{
			// Add the item to the channel
			$row = $feed->channel->addChild('item');

			foreach ($item as $name => $value)
			{
				if ($name === 'pubDate' AND (is_int($value) OR ctype_digit($value)))
				{
					// Convert timestamps to RFC 822 formatted dates
					$value = date('r', $value);
				}
				elseif (($name === 'link' OR $name === 'guid') AND strpos($value, '://') === FALSE)
				{
					// Convert URIs to URLs
					$value = URL::site($value, 'http');
				}

				// Add the info to the row
				$row->addChild($name, $value);
			}
		}

		if (function_exists('dom_import_simplexml'))
		{
			// Convert the feed object to a DOM object
			$feed = dom_import_simplexml($feed)->ownerDocument;

			// DOM generates more readable XML
			$feed->formatOutput = TRUE;

			// Export the document as XML
			$feed = $feed->saveXML();
		}
		else
		{
			// Export the document as XML
			$feed = $feed->asXML();
		}

		return $feed;
	}

} // End Feed
