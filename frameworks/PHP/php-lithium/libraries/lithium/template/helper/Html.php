<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\template\helper;

/**
 * A template helper that assists in generating HTML content. Accessible in templates via
 * `$this->html`, which will auto-load this helper into the rendering context. For examples of how
 * to use this helper, see the documentation for a specific method. For a list of the
 * template strings this helper uses, see the `$_strings` property.
 */
class Html extends \lithium\template\Helper {

	/**
	 * String templates used by this helper.
	 *
	 * @var array
	 */
	protected $_strings = array(
		'block'            => '<div{:options}>{:content}</div>',
		'block-end'        => '</div>',
		'block-start'      => '<div{:options}>',
		'charset'          => '<meta charset="{:encoding}" />',
		'image'            => '<img src="{:path}"{:options} />',
		'js-block'         => '<script type="text/javascript"{:options}>{:content}</script>',
		'js-end'           => '</script>',
		'js-start'         => '<script type="text/javascript"{:options}>',
		'link'             => '<a href="{:url}"{:options}>{:title}</a>',
		'list'             => '<ul{:options}>{:content}</ul>',
		'list-item'        => '<li{:options}>{:content}</li>',
		'meta'             => '<meta{:options}/>',
		'meta-link'        => '<link href="{:url}"{:options} />',
		'para'             => '<p{:options}>{:content}</p>',
		'para-start'       => '<p{:options}>',
		'script'           => '<script type="text/javascript" src="{:path}"{:options}></script>',
		'style'            => '<style type="text/css"{:options}>{:content}</style>',
		'style-import'     => '<style type="text/css"{:options}>@import url({:url});</style>',
		'style-link'       => '<link rel="{:type}" type="text/css" href="{:path}"{:options} />',
		'table-header'     => '<th{:options}>{:content}</th>',
		'table-header-row' => '<tr{:options}>{:content}</tr>',
		'table-cell'       => '<td{:options}>{:content}</td>',
		'table-row'        => '<tr{:options}>{:content}</tr>',
		'tag'              => '<{:name}{:options}>{:content}</{:name}>',
		'tag-end'          => '</{:name}>',
		'tag-start'        => '<{:name}{:options}>'
	);

	/**
	 * Data used for custom <meta /> links.
	 *
	 * @var array
	 */
	protected $_metaLinks = array(
		'atom' => array('type' => 'application/atom+xml', 'rel' => 'alternate'),
		'rss'  => array('type' => 'application/rss+xml', 'rel' => 'alternate'),
		'icon' => array('type' => 'image/x-icon', 'rel' => 'icon')
	);

	/**
	 * List of meta tags to cache and to output.
	 *
	 * @var array
	 * @see lithium\template\helper\Html::meta()
	 */
	protected $_metaList = array();

	/**
	 * Used by output handlers to calculate asset paths in conjunction with the `Media` class.
	 *
	 * @var array
	 * @see lithium\net\http\Media
	 */
	public $contentMap = array(
		'script' => 'js',
		'style'  => 'css',
		'image' => 'image',
		'_metaLink' => 'generic'
	);

	/**
	 * Returns a charset meta-tag for declaring the encoding of the document.
	 *
	 * The terms character set (here: charset) and character encoding (here:
	 * encoding) were historically synonymous. The terms now have related but
	 * distinct meanings. Whenever possible Lithium tries to use precise
	 * terminology. Since HTML uses the term `charset` we expose this method
	 * under the exact same name. This caters to the expectation towards a HTML
	 * helper. However the rest of the framework will use the term `encoding`
	 * when talking about character encoding.
	 *
	 * It is suggested that uppercase letters should be used when specifying
	 * the encoding. HTML specs don't require it to be uppercase and sites in
	 * the wild most often use the lowercase variant. On the other hand must
	 * XML parsers (those may not be relevant in this context anyway) not
	 * support lowercase encodings. This and the fact that IANA lists only
	 * encodings with uppercase characters led to the above suggestion.
	 *
	 * @see lithium\net\http\Response::$encoding
	 * @link http://www.iana.org/assignments/character-sets
	 * @param string $encoding The character encoding to be used in the meta tag.
	 *               Defaults to the encoding of the `Response` object attached to the
	 *               current context. The default encoding of that object is `UTF-8`.
	 *               The string given here is not manipulated in any way, so that
	 *               values are rendered literally. Also see above note about casing.
	 * @return string A meta tag containing the specified encoding (literally).
	 */
	public function charset($encoding = null) {
		if ($response = $this->_context->response()) {
			$encoding = $encoding ?: $response->encoding;
		}
		return $this->_render(__METHOD__, 'charset', compact('encoding'));
	}

	/**
	 * Creates an HTML link (`<a />`) or a document meta-link (`<link />`).
	 *
	 * If `$url` starts with `'http://'` or `'https://'`, this is treated as an external link.
	 * Otherwise, it is treated as a path to controller/action and parsed using
	 * the `Router::match()` method (where `Router` is the routing class dependency specified by
	 * the rendering context, i.e. `lithium\template\view\Renderer::$_classes`).
	 *
	 * If `$url` is empty, `$title` is used in its place.
	 *
	 * @param string $title The content to be wrapped by an `<a />` tag,
	 *               or the `title` attribute of a meta-link `<link />`.
	 * @param mixed $url Can be a string representing a URL relative to the base of your Lithium
	 *              application, an external URL (starts with `'http://'` or `'https://'`), an
	 *              anchor name starting with `'#'` (i.e. `'#top'`), or an array defining a set
	 *              of request parameters that should be matched against a route in `Router`.
	 * @param array $options The available options are:
	 *              - `'escape'` _boolean_: Whether or not the title content should be escaped.
	 *              Defaults to `true`.
	 *              - `'type'` _string_: The meta-link type, which is looked up in
	 *              `Html::$_metaLinks`. By default it accepts `atom`, `rss` and `icon`. If a `type`
	 *              is specified, this method will render a document meta-link (`<link />`),
	 *              instead of an HTML link (`<a />`).
	 *              - any other options specified are rendered as HTML attributes of the element.
	 * @return string Returns an `<a />` or `<link />` element.
	 */
	public function link($title, $url = null, array $options = array()) {
		$defaults = array('escape' => true, 'type' => null);
		list($scope, $options) = $this->_options($defaults, $options);

		if (isset($scope['type']) && $type = $scope['type']) {
			$options += compact('title');
			return $this->_metaLink($type, $url, $options);
		}

		$url = is_null($url) ? $title : $url;
		return $this->_render(__METHOD__, 'link', compact('title', 'url', 'options'), $scope);
	}

	/**
	 * Returns a JavaScript include tag (`<script />` element). If the filename is prefixed with
	 * `'/'`, the path will be relative to the base path of your application.  Otherwise, the path
	 * will be relative to your JavaScript path, usually `webroot/js`.
	 *
	 * @link http://lithify.me/docs/manual/handling-http-requests/views.wiki
	 * @param mixed $path String The name of a JavaScript file, or an array of names.
	 * @param array $options Available options are:
	 *              - `'inline'` _boolean_: Whether or not the `<script />` element should be output
	 *              inline. When set to false, the `scripts()` handler prints out the script, and
	 *              other specified scripts to be included in the layout. Defaults to `true`.
	 *              This is useful when page-specific scripts are created inline in the page, and
	 *              you'd like to place them in the `<head />` along with your other scripts.
	 *              - any other options specified are rendered as HTML attributes of the element.
	 * @return string
	 * @filter This method can be filtered.
	 */
	public function script($path, array $options = array()) {
		$defaults = array('inline' => true);
		list($scope, $options) = $this->_options($defaults, $options);

		if (is_array($path)) {
			foreach ($path as $i => $item) {
				$path[$i] = $this->script($item, $scope);
			}
			return ($scope['inline']) ? join("\n\t", $path) . "\n" : null;
		}
		$m = __METHOD__;
		$params = compact('path', 'options');

		$script = $this->_filter(__METHOD__, $params, function($self, $params, $chain) use ($m) {
			return $self->invokeMethod('_render', array($m, 'script', $params));
		});
		if ($scope['inline']) {
			return $script;
		}
		if ($this->_context) {
			$this->_context->scripts($script);
		}
	}

	/**
	 * Creates a `<link />` element for CSS stylesheets or a `<style />` tag. If the filename is
	 * prefixed with `'/'`, the path will be relative to the base path of your application.
	 * Otherwise, the path will be relative to your stylesheets path, usually `webroot/css`.
	 *
	 * @param mixed $path The name of a CSS stylesheet in `/app/webroot/css`, or an array
	 *              containing names of CSS stylesheets in that directory.
	 * @param array $options Available options are:
	 *              - `'inline'` _boolean_: Whether or not the `<style />` element should be output
	 *              inline. When set to `false`, the `styles()` handler prints out the styles,
	 *              and other specified styles to be included in the layout. Defaults to `true`.
	 *              This is useful when page-specific styles are created inline in the page, and
	 *              you'd like to place them in
	 *              the `<head />` along with your other styles.
	 *              - `'type'` _string_: By default, accepts `stylesheet` or `import`, which
	 *              respectively correspond to `style-link` and `style-import` strings templates
	 *              defined in `Html::$_strings`.
	 *              - any other options specified are rendered as HTML attributes of the element.
	 * @return string CSS <link /> or <style /> tag, depending on the type of link.
	 * @filter This method can be filtered.
	 */
	public function style($path, array $options = array()) {
		$defaults = array('type' => 'stylesheet', 'inline' => true);
		list($scope, $options) = $this->_options($defaults, $options);

		if (is_array($path)) {
			foreach ($path as $i => $item) {
				$path[$i] = $this->style($item, $scope);
			}
			return ($scope['inline']) ? join("\n\t", $path) . "\n" : null;
		}
		$method = __METHOD__;
		$type = $scope['type'];
		$params = compact('type', 'path', 'options');
		$filter = function($self, $params, $chain) use ($defaults, $method) {
			$template = ($params['type'] === 'import') ? 'style-import' : 'style-link';
			return $self->invokeMethod('_render', array($method, $template, $params));
		};
		$style = $this->_filter($method, $params, $filter);

		if ($scope['inline']) {
			return $style;
		}
		if ($this->_context) {
			$this->_context->styles($style);
		}
	}

	/**
	 * Creates a tag for the `<head>` section of your document.
	 *
	 * If there is a rendering context, then it also pushes the resulting tag to it.
	 *
	 * The `$options` must match the named parameters from `$_strings` for the
	 * given `$tag`.
	 *
	 * @param string $tag the name of a key in `$_strings`
	 * @param array $options the options required by `$_strings[$tag]`
	 * @return mixed a string if successful, otherwise `null`
	 * @filter This method can be filtered.
	 */
	public function head($tag, array $options) {
		if (!isset($this->_strings[$tag])) {
			return null;
		}
		$method = __METHOD__;
		$filter = function($self, $options, $chain) use ($method, $tag) {
			return $self->invokeMethod('_render', array($method, $tag, $options));
		};
		$head = $this->_filter($method, $options, $filter);
		if ($this->_context) {
			$this->_context->head($head);
		}
		return $head;
	}

	/**
	 * Creates a formatted `<img />` element.
	 *
	 * @param string $path Path to the image file. If the filename is prefixed with
	 *               `'/'`, the path will be relative to the base path of your application.
	 *               Otherwise the path will be relative to the images directory, usually
	 *               `app/webroot/img/`. If the name starts with `'http://'`, this is treated
	 *               as an external url used as the `src` attribute.
	 * @param array $options Array of HTML attributes.
	 * @return string Returns a formatted `<img />` tag.
	 * @filter This method can be filtered.
	 */
	public function image($path, array $options = array()) {
		$defaults = array('alt' => '');
		$options += $defaults;
		$path = is_array($path) ? $this->_context->url($path) : $path;
		$params = compact('path', 'options');
		$method = __METHOD__;

		return $this->_filter($method, $params, function($self, $params, $chain) use ($method) {
			return $self->invokeMethod('_render', array($method, 'image', $params));
		});
	}

	/**
	 * Creates a link to an external resource.
	 *
	 * @param string $type The title of the external resource
	 * @param mixed $url The address of the external resource or string for content attribute
	 * @param array $options Other attributes for the generated tag. If the type attribute
	 *              is 'html', 'rss', 'atom', or 'icon', the mime-type is returned.
	 * @return string
	 */
	protected function _metaLink($type, $url = null, array $options = array()) {
		$options += isset($this->_metaLinks[$type]) ? $this->_metaLinks[$type] : array();

		if ($type === 'icon') {
			$url = $url ?: 'favicon.ico';
			$standard = $this->_render(__METHOD__, 'meta-link', compact('url', 'options'), array(
				'handlers' => array('url' => 'path')
			));
			$options['rel'] = 'shortcut icon';
			$ieFix = $this->_render(__METHOD__, 'meta-link', compact('url', 'options'), array(
				'handlers' => array('url' => 'path')
			));
			return "{$standard}\n\t{$ieFix}";
		}
		return $this->_render(__METHOD__, 'meta-link', compact('url', 'options'), array(
			'handlers' => array()
		));
	}
}

?>