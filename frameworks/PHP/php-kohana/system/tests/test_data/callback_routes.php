<?php
/**
 * A holding class for route callback tests
 *
 * @group kohana
 *
 * @package    Unittest
 * @author     Kohana Team
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Route_Holder
{
	/**
	 * Just an empty callback that doesn't match anything
	 */
	public static function default_callback($uri)
	{

	}

	/**
	 * Just an empty callback that matches everything
	 *
	 * @return array
	 */
	public static function default_return_callback($uri)
	{
		return array(

		);
	}

	/**
	 * Route callback for test_matches_returns_array_of_parameters_on_successful_match
	 *
	 * @return array
	 */
	public static function matches_returns_array_of_parameters_on_successful_match($uri)
	{
		return array(
			'controller' => 'welcome',
			'action' => 'index',
		);
	}

	/**
	 * Route callback for test_required_parameters_are_needed
	 *
	 * @return array
	 */
	public static function required_parameters_are_needed($uri)
	{
		if (substr($uri, 0, 5) == 'admin')
		{
			return array(
				'controller' => 'foo',
				'action' => 'bar',
			);
		}
	}

	/**
	 * Route callback for test reverse_routing_returns_routes_uri_if_route_is_static
	 *
	 * @return array
	 */
	public static function reverse_routing_returns_routes_uri_if_route_is_static($uri)
	{
		if ($uri == 'info/about_us')
		{
			return array(

			);
		}
	}

	/**
	 * Route callback for test route_filter_modify_params
	 *
	 * @return array
	 */
	public static function route_filter_modify_params_array(Route $route, $params)
	{
		$params['action'] = 'modified';

		return $params;
	}

	/**
	 * Route callback for test route_filter_modify_params
	 *
	 * @return array
	 */
	public static function route_filter_modify_params_false(Route $route, $params)
	{
		return FALSE;
	}

}