<?php

$render = function($self, $path, $parent = null) use ($base) {
	$result = array("<ul class='menu'>");

	foreach ($path as $current => $value) {
		$path = trim(str_replace("//", "/", "{$parent}/{$current}"), "/");
		$result[] = "<li>";

		if (is_string($value)) {
			$result[] =  "<a title='run {$path}' "
				. "href='{$base}/test/{$path}'>{$current}</a>";
			continue;
		}
		$result[] =  "<a class='menu-folder' title='run {$path}' "
			. "href='{$base}/test/{$path}'>{$current}</a>";
		$result[] = $self($self, $value, $path);
		$result[] = "</li>";
	}
	$result[] = "</ul>";
	return join("\n", $result);
};
echo $render($render, $menu);
?>