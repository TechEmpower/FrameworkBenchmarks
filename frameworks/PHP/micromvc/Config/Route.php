<?php
/**
 * URL Routing
 *
 * URLs are very important to the future usability of your site. Take
 * time to think about your structure in a way that is meaningful. Place
 * your most common page routes at the top for better performace.
 *
 * - Routes are matched from left-to-right.
 * - Regex can also be used to define routes if enclosed in "/.../"
 * - Each regex catch pattern (...) will be viewed as a parameter.
 * - The remaning (unmached) URL path will be passed as parameters.
 *
 ** Simple Example **
 * URL Path:	/forum/topic/view/45/Hello-World
 * Route:		"forum/topic/view" => 'Forum\Controller\Forum\View'
 * Result:		Forum\Controller\Forum\View->action('45', 'Hello-World');
 *
 ** Regex Example **
 * URL Path:	/John_Doe4/recent/comments/3
 * Route:		"/^(\w+)/recent/comments/' => 'Comments\Controller\Recent'
 * Result:		Comments\Controller\Recent->action($username = 'John_Doe4', $page = 3)
 */
$config = array();

$config['routes'] = array(
	''					=> '\Controller\Index',
	'404'				=> '\Controller\Page404',
	'school'			=> '\Controller\School',
    'json'			    => '\Controller\Benchmark\Json',
    'db'			    => '\Controller\Benchmark\Db',

	// Example paths
	//'example/path'		=> '\Controller\Example\Hander',
	//'example/([^/]+)'	=> '\Controller\Example\Param',
);
