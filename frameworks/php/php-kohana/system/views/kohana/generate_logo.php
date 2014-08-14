<?php

// Get the latest logo contents
$data = base64_encode(file_get_contents('http://kohanaframework.org/media/img/kohana.png'));

// Create the logo file
file_put_contents('logo.php', "<?php
/**
 * Kohana Logo, base64_encoded PNG
 * 
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
return array('mime' => 'image/png', 'data' => '{$data}'); ?>");