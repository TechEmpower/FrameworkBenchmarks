<!DOCTYPE html>
<html>
<head>
	<meta charset="utf-8">
	<title>Crypto key error</title>
	<style type="text/css">
		* { margin: 0; padding: 0; }
		body { background-color: #EEE; font-family: sans-serif; font-size: 16px; line-height: 20px; margin: 40px; }
		#wrapper { padding: 30px; background: #fff; color: #333; margin: 0 auto; width: 600px; }
		a { color: #36428D; }
		h1 { color: #000; font-size: 55px; padding: 0 0 25px; line-height: 1em; }
		.intro { font-size: 22px; line-height: 30px; font-family: georgia, serif; color: #555; padding: 29px 0 20px; border-top: 1px solid #CCC; }
		h2 { margin: 50px 0 15px; padding: 0 0 10px; font-size: 18px; border-bottom: 1px dashed #ccc; }
		h2.first { margin: 10px 0 15px; }
		p { margin: 0 0 15px; line-height: 22px;}
		a { color: #666; }
		pre { border-left: 1px solid #ddd; line-height:20px; margin:20px; padding-left:1em; font-size: 14px; }
		pre, code { color:#137F80; font-family: Courier, monospace; }
		ul { margin: 15px 30px; }
		li { line-height: 24px;}
		.footer { color: #777; font-size: 12px; margin: 40px 0 0 0; }
	</style>
</head>
<body>
	<div id="wrapper">
		<h1>Crypto key error</h1>

		<p class="intro">No write access to APPPATH/config/crypt.php.</p>

		<p>
			The FuelPHP crypto functions require a set of unique and truelly random crypto keys.
			These keys are automatically generated and written to the crypto configuration
			file the first time the application accesses a crypto function.
		</p>

		<p>Please copy the following code into APPPATH/config/crypt.php manually:</p>
		<pre><code>&lt;?php
/**
 * Part of the Fuel framework.
 *
 * @package     Fuel
 * @version     1.5
 * @author      Fuel Development Team
 * @license     MIT License
 * @copyright   2010 - 2013 Fuel Development Team
 * @link        http://fuelphp.com
 */

return array (
	'crypto_key' => '<?php echo $keys['crypto_key']; ?>',
	'crypto_iv' => '<?php echo $keys['crypto_iv']; ?>',
	'crypto_hmac' => '<?php echo $keys['crypto_hmac']; ?>',
);



		<p class="footer">
			<a href="http://fuelphp.com">FuelPHP</a> is released under the MIT license.
		</p>
	</div>
</body>
</html>
