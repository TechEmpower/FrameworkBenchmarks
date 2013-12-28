<?php
/*
  +----------------------------------------------------------------------+
  | APC                                                                  |
  +----------------------------------------------------------------------+
  | Copyright (c) 2006-2011 The PHP Group                                |
  +----------------------------------------------------------------------+
  | This source file is subject to version 3.01 of the PHP license,      |
  | that is bundled with this package in the file LICENSE, and is        |
  | available through the world-wide-web at the following url:           |
  | http://www.php.net/license/3_01.txt                                  |
  | If you did not receive a copy of the PHP license and are unable to   |
  | obtain it through the world-wide-web, please send a note to          |
  | license@php.net so we can mail you a copy immediately.               |
  +----------------------------------------------------------------------+
  | Authors: Ralf Becker <beckerr@php.net>                               |
  |          Rasmus Lerdorf <rasmus@php.net>                             |
  |          Ilia Alshanetsky <ilia@prohost.org>                         |
  +----------------------------------------------------------------------+

   All other licensing and usage conditions are those of the PHP Group.

 */

$VERSION='$Id: apc.php 325483 2012-05-01 00:34:04Z rasmus $';

////////// READ OPTIONAL CONFIGURATION FILE ////////////
if (file_exists("apc.conf.php")) include("apc.conf.php");
////////////////////////////////////////////////////////

////////// BEGIN OF DEFAULT CONFIG AREA ///////////////////////////////////////////////////////////

defaults('USE_AUTHENTICATION',1);			// Use (internal) authentication - best choice if 
											// no other authentication is available
											// If set to 0:
											//  There will be no further authentication. You 
											//  will have to handle this by yourself!
											// If set to 1:
											//  You need to change ADMIN_PASSWORD to make
											//  this work!
defaults('ADMIN_USERNAME','apc'); 			// Admin Username
defaults('ADMIN_PASSWORD','password');  	// Admin Password - CHANGE THIS TO ENABLE!!!

// (beckerr) I'm using a clear text password here, because I've no good idea how to let 
//           users generate a md5 or crypt password in a easy way to fill it in above

//defaults('DATE_FORMAT', "d.m.Y H:i:s");	// German
defaults('DATE_FORMAT', 'Y/m/d H:i:s'); 	// US

defaults('GRAPH_SIZE',200);					// Image size

//defaults('PROXY', 'tcp://127.0.0.1:8080');

////////// END OF DEFAULT CONFIG AREA /////////////////////////////////////////////////////////////


// "define if not defined"
function defaults($d,$v) {
	if (!defined($d)) define($d,$v); // or just @define(...)
}

// rewrite $PHP_SELF to block XSS attacks
//
$PHP_SELF= isset($_SERVER['PHP_SELF']) ? htmlentities(strip_tags($_SERVER['PHP_SELF'],''), ENT_QUOTES, 'UTF-8') : '';
$time = time();
$host = php_uname('n');
if($host) { $host = '('.$host.')'; }
if (isset($_SERVER['SERVER_ADDR'])) {
  $host .= ' ('.$_SERVER['SERVER_ADDR'].')';
}

// operation constants
define('OB_HOST_STATS',1);
define('OB_SYS_CACHE',2);
define('OB_USER_CACHE',3);
define('OB_SYS_CACHE_DIR',4);
define('OB_VERSION_CHECK',9);

// check validity of input variables
$vardom=array(
	'OB'	=> '/^\d+$/',			// operational mode switch
	'CC'	=> '/^[01]$/',			// clear cache requested
	'DU'	=> '/^.*$/',			// Delete User Key
	'SH'	=> '/^[a-z0-9]+$/',		// shared object description

	'IMG'	=> '/^[123]$/',			// image to generate
	'LO'	=> '/^1$/',				// login requested

	'COUNT'	=> '/^\d+$/',			// number of line displayed in list
	'SCOPE'	=> '/^[AD]$/',			// list view scope
	'SORT1'	=> '/^[AHSMCDTZ]$/',	// first sort key
	'SORT2'	=> '/^[DA]$/',			// second sort key
	'AGGR'	=> '/^\d+$/',			// aggregation by dir level
	'SEARCH'	=> '~^[a-zA-Z0-9/_.-]*$~'			// aggregation by dir level
);

// default cache mode
$cache_mode='opcode';

// cache scope
$scope_list=array(
	'A' => 'cache_list',
	'D' => 'deleted_list'
);

// handle POST and GET requests
if (empty($_REQUEST)) {
	if (!empty($_GET) && !empty($_POST)) {
		$_REQUEST = array_merge($_GET, $_POST);
	} else if (!empty($_GET)) {
		$_REQUEST = $_GET;
	} else if (!empty($_POST)) {
		$_REQUEST = $_POST;
	} else {
		$_REQUEST = array();
	}
}

// check parameter syntax
foreach($vardom as $var => $dom) {
	if (!isset($_REQUEST[$var])) {
		$MYREQUEST[$var]=NULL;
	} else if (!is_array($_REQUEST[$var]) && preg_match($dom.'D',$_REQUEST[$var])) {
		$MYREQUEST[$var]=$_REQUEST[$var];
	} else {
		$MYREQUEST[$var]=$_REQUEST[$var]=NULL;
	}
}

// check parameter sematics
if (empty($MYREQUEST['SCOPE'])) $MYREQUEST['SCOPE']="A";
if (empty($MYREQUEST['SORT1'])) $MYREQUEST['SORT1']="H";
if (empty($MYREQUEST['SORT2'])) $MYREQUEST['SORT2']="D";
if (empty($MYREQUEST['OB']))	$MYREQUEST['OB']=OB_HOST_STATS;
if (!isset($MYREQUEST['COUNT'])) $MYREQUEST['COUNT']=20;
if (!isset($scope_list[$MYREQUEST['SCOPE']])) $MYREQUEST['SCOPE']='A';

$MY_SELF=
	"$PHP_SELF".
	"?SCOPE=".$MYREQUEST['SCOPE'].
	"&SORT1=".$MYREQUEST['SORT1'].
	"&SORT2=".$MYREQUEST['SORT2'].
	"&COUNT=".$MYREQUEST['COUNT'];
$MY_SELF_WO_SORT=
	"$PHP_SELF".
	"?SCOPE=".$MYREQUEST['SCOPE'].
	"&COUNT=".$MYREQUEST['COUNT'];

// authentication needed?
//
if (!USE_AUTHENTICATION) {
	$AUTHENTICATED=1;
} else {
	$AUTHENTICATED=0;
	if (ADMIN_PASSWORD!='password' && ($MYREQUEST['LO'] == 1 || isset($_SERVER['PHP_AUTH_USER']))) {

		if (!isset($_SERVER['PHP_AUTH_USER']) ||
			!isset($_SERVER['PHP_AUTH_PW']) ||
			$_SERVER['PHP_AUTH_USER'] != ADMIN_USERNAME ||
			$_SERVER['PHP_AUTH_PW'] != ADMIN_PASSWORD) {
			Header("WWW-Authenticate: Basic realm=\"APC Login\"");
			Header("HTTP/1.0 401 Unauthorized");

			echo <<<EOB
				<html><body>
				<h1>Rejected!</h1>
				<big>Wrong Username or Password!</big><br/>&nbsp;<br/>&nbsp;
				<big><a href='$PHP_SELF?OB={$MYREQUEST['OB']}'>Continue...</a></big>
				</body></html>
EOB;
			exit;
			
		} else {
			$AUTHENTICATED=1;
		}
	}
}
	
// select cache mode
if ($AUTHENTICATED && $MYREQUEST['OB'] == OB_USER_CACHE) {
	$cache_mode='user';
}
// clear cache
if ($AUTHENTICATED && isset($MYREQUEST['CC']) && $MYREQUEST['CC']) {
	apc_clear_cache($cache_mode);
}

if ($AUTHENTICATED && !empty($MYREQUEST['DU'])) {
	apc_delete($MYREQUEST['DU']);
}

if(!function_exists('apc_cache_info') || !($cache=@apc_cache_info($cache_mode))) {
	echo "No cache info available.  APC does not appear to be running.";
  exit;
}

$cache_user = apc_cache_info('user', 1);  
$mem=apc_sma_info();
if(!$cache['num_hits']) { $cache['num_hits']=1; $time++; }  // Avoid division by 0 errors on a cache clear

// don't cache this page
//
header("Cache-Control: no-store, no-cache, must-revalidate");  // HTTP/1.1
header("Cache-Control: post-check=0, pre-check=0", false);
header("Pragma: no-cache");                                    // HTTP/1.0

function duration($ts) {
    global $time;
    $years = (int)((($time - $ts)/(7*86400))/52.177457);
    $rem = (int)(($time-$ts)-($years * 52.177457 * 7 * 86400));
    $weeks = (int)(($rem)/(7*86400));
    $days = (int)(($rem)/86400) - $weeks*7;
    $hours = (int)(($rem)/3600) - $days*24 - $weeks*7*24;
    $mins = (int)(($rem)/60) - $hours*60 - $days*24*60 - $weeks*7*24*60;
    $str = '';
    if($years==1) $str .= "$years year, ";
    if($years>1) $str .= "$years years, ";
    if($weeks==1) $str .= "$weeks week, ";
    if($weeks>1) $str .= "$weeks weeks, ";
    if($days==1) $str .= "$days day,";
    if($days>1) $str .= "$days days,";
    if($hours == 1) $str .= " $hours hour and";
    if($hours>1) $str .= " $hours hours and";
    if($mins == 1) $str .= " 1 minute";
    else $str .= " $mins minutes";
    return $str;
}

// create graphics
//
function graphics_avail() {
	return extension_loaded('gd');
}
if (isset($MYREQUEST['IMG']))
{
	if (!graphics_avail()) {
		exit(0);
	}

	function fill_arc($im, $centerX, $centerY, $diameter, $start, $end, $color1,$color2,$text='',$placeindex=0) {
		$r=$diameter/2;
		$w=deg2rad((360+$start+($end-$start)/2)%360);

		
		if (function_exists("imagefilledarc")) {
			// exists only if GD 2.0.1 is avaliable
			imagefilledarc($im, $centerX+1, $centerY+1, $diameter, $diameter, $start, $end, $color1, IMG_ARC_PIE);
			imagefilledarc($im, $centerX, $centerY, $diameter, $diameter, $start, $end, $color2, IMG_ARC_PIE);
			imagefilledarc($im, $centerX, $centerY, $diameter, $diameter, $start, $end, $color1, IMG_ARC_NOFILL|IMG_ARC_EDGED);
		} else {
			imagearc($im, $centerX, $centerY, $diameter, $diameter, $start, $end, $color2);
			imageline($im, $centerX, $centerY, $centerX + cos(deg2rad($start)) * $r, $centerY + sin(deg2rad($start)) * $r, $color2);
			imageline($im, $centerX, $centerY, $centerX + cos(deg2rad($start+1)) * $r, $centerY + sin(deg2rad($start)) * $r, $color2);
			imageline($im, $centerX, $centerY, $centerX + cos(deg2rad($end-1))   * $r, $centerY + sin(deg2rad($end))   * $r, $color2);
			imageline($im, $centerX, $centerY, $centerX + cos(deg2rad($end))   * $r, $centerY + sin(deg2rad($end))   * $r, $color2);
			imagefill($im,$centerX + $r*cos($w)/2, $centerY + $r*sin($w)/2, $color2);
		}
		if ($text) {
			if ($placeindex>0) {
				imageline($im,$centerX + $r*cos($w)/2, $centerY + $r*sin($w)/2,$diameter, $placeindex*12,$color1);
				imagestring($im,4,$diameter, $placeindex*12,$text,$color1);	
				
			} else {
				imagestring($im,4,$centerX + $r*cos($w)/2, $centerY + $r*sin($w)/2,$text,$color1);
			}
		}
	} 

	function text_arc($im, $centerX, $centerY, $diameter, $start, $end, $color1,$text,$placeindex=0) {
		$r=$diameter/2;
		$w=deg2rad((360+$start+($end-$start)/2)%360);

		if ($placeindex>0) {
			imageline($im,$centerX + $r*cos($w)/2, $centerY + $r*sin($w)/2,$diameter, $placeindex*12,$color1);
			imagestring($im,4,$diameter, $placeindex*12,$text,$color1);	
				
		} else {
			imagestring($im,4,$centerX + $r*cos($w)/2, $centerY + $r*sin($w)/2,$text,$color1);
		}
	} 
	
	function fill_box($im, $x, $y, $w, $h, $color1, $color2,$text='',$placeindex='') {
		global $col_black;
		$x1=$x+$w-1;
		$y1=$y+$h-1;

		imagerectangle($im, $x, $y1, $x1+1, $y+1, $col_black);
		if($y1>$y) imagefilledrectangle($im, $x, $y, $x1, $y1, $color2);
		else imagefilledrectangle($im, $x, $y1, $x1, $y, $color2);
		imagerectangle($im, $x, $y1, $x1, $y, $color1);
		if ($text) {
			if ($placeindex>0) {
			
				if ($placeindex<16)
				{
					$px=5;
					$py=$placeindex*12+6;
					imagefilledrectangle($im, $px+90, $py+3, $px+90-4, $py-3, $color2);
					imageline($im,$x,$y+$h/2,$px+90,$py,$color2);
					imagestring($im,2,$px,$py-6,$text,$color1);	
					
				} else {
					if ($placeindex<31) {
						$px=$x+40*2;
						$py=($placeindex-15)*12+6;
					} else {
						$px=$x+40*2+100*intval(($placeindex-15)/15);
						$py=($placeindex%15)*12+6;
					}
					imagefilledrectangle($im, $px, $py+3, $px-4, $py-3, $color2);
					imageline($im,$x+$w,$y+$h/2,$px,$py,$color2);
					imagestring($im,2,$px+2,$py-6,$text,$color1);	
				}
			} else {
				imagestring($im,4,$x+5,$y1-16,$text,$color1);
			}
		}
	}


	$size = GRAPH_SIZE; // image size
	if ($MYREQUEST['IMG']==3)
		$image = imagecreate(2*$size+150, $size+10);
	else
		$image = imagecreate($size+50, $size+10);

	$col_white = imagecolorallocate($image, 0xFF, 0xFF, 0xFF);
	$col_red   = imagecolorallocate($image, 0xD0, 0x60,  0x30);
	$col_green = imagecolorallocate($image, 0x60, 0xF0, 0x60);
	$col_black = imagecolorallocate($image,   0,   0,   0);
	imagecolortransparent($image,$col_white);

	switch ($MYREQUEST['IMG']) {
	
	case 1:
		$s=$mem['num_seg']*$mem['seg_size'];
		$a=$mem['avail_mem'];
		$x=$y=$size/2;
		$fuzz = 0.000001;

		// This block of code creates the pie chart.  It is a lot more complex than you
		// would expect because we try to visualize any memory fragmentation as well.
		$angle_from = 0;
		$string_placement=array();
		for($i=0; $i<$mem['num_seg']; $i++) {	
			$ptr = 0;
			$free = $mem['block_lists'][$i];
			uasort($free, 'block_sort');
			foreach($free as $block) {
				if($block['offset']!=$ptr) {       // Used block
					$angle_to = $angle_from+($block['offset']-$ptr)/$s;
					if(($angle_to+$fuzz)>1) $angle_to = 1;
					if( ($angle_to*360) - ($angle_from*360) >= 1) {
						fill_arc($image,$x,$y,$size,$angle_from*360,$angle_to*360,$col_black,$col_red);
						if (($angle_to-$angle_from)>0.05) {
							array_push($string_placement, array($angle_from,$angle_to));
						}
					}
					$angle_from = $angle_to;
				}
				$angle_to = $angle_from+($block['size'])/$s;
				if(($angle_to+$fuzz)>1) $angle_to = 1;
				if( ($angle_to*360) - ($angle_from*360) >= 1) {
					fill_arc($image,$x,$y,$size,$angle_from*360,$angle_to*360,$col_black,$col_green);
					if (($angle_to-$angle_from)>0.05) {
						array_push($string_placement, array($angle_from,$angle_to));
					}
				}
				$angle_from = $angle_to;
				$ptr = $block['offset']+$block['size'];
			}
			if ($ptr < $mem['seg_size']) { // memory at the end 
				$angle_to = $angle_from + ($mem['seg_size'] - $ptr)/$s;
				if(($angle_to+$fuzz)>1) $angle_to = 1;
				fill_arc($image,$x,$y,$size,$angle_from*360,$angle_to*360,$col_black,$col_red);
				if (($angle_to-$angle_from)>0.05) {
					array_push($string_placement, array($angle_from,$angle_to));
				}
			}
		}
		foreach ($string_placement as $angle) {
			text_arc($image,$x,$y,$size,$angle[0]*360,$angle[1]*360,$col_black,bsize($s*($angle[1]-$angle[0])));
		}
		break;
		
	case 2: 
		$s=$cache['num_hits']+$cache['num_misses'];
		$a=$cache['num_hits'];
		
		fill_box($image, 30,$size,50,-$a*($size-21)/$s,$col_black,$col_green,sprintf("%.1f%%",$cache['num_hits']*100/$s));
		fill_box($image,130,$size,50,-max(4,($s-$a)*($size-21)/$s),$col_black,$col_red,sprintf("%.1f%%",$cache['num_misses']*100/$s));
		break;
		
	case 3:
		$s=$mem['num_seg']*$mem['seg_size'];
		$a=$mem['avail_mem'];
		$x=130;
		$y=1;
		$j=1;

		// This block of code creates the bar chart.  It is a lot more complex than you
		// would expect because we try to visualize any memory fragmentation as well.
		for($i=0; $i<$mem['num_seg']; $i++) {	
			$ptr = 0;
			$free = $mem['block_lists'][$i];
			uasort($free, 'block_sort');
			foreach($free as $block) {
				if($block['offset']!=$ptr) {       // Used block
					$h=(GRAPH_SIZE-5)*($block['offset']-$ptr)/$s;
					if ($h>0) {
                                                $j++;
						if($j<75) fill_box($image,$x,$y,50,$h,$col_black,$col_red,bsize($block['offset']-$ptr),$j);
                                                else fill_box($image,$x,$y,50,$h,$col_black,$col_red);
                                        }
					$y+=$h;
				}
				$h=(GRAPH_SIZE-5)*($block['size'])/$s;
				if ($h>0) {
                                        $j++;
					if($j<75) fill_box($image,$x,$y,50,$h,$col_black,$col_green,bsize($block['size']),$j);
					else fill_box($image,$x,$y,50,$h,$col_black,$col_green);
                                }
				$y+=$h;
				$ptr = $block['offset']+$block['size'];
			}
			if ($ptr < $mem['seg_size']) { // memory at the end 
				$h = (GRAPH_SIZE-5) * ($mem['seg_size'] - $ptr) / $s;
				if ($h > 0) {
					fill_box($image,$x,$y,50,$h,$col_black,$col_red,bsize($mem['seg_size']-$ptr),$j++);
				}
			}
		}
		break;
	case 4: 
		$s=$cache['num_hits']+$cache['num_misses'];
		$a=$cache['num_hits'];
	        	
		fill_box($image, 30,$size,50,-$a*($size-21)/$s,$col_black,$col_green,sprintf("%.1f%%",$cache['num_hits']*100/$s));
		fill_box($image,130,$size,50,-max(4,($s-$a)*($size-21)/$s),$col_black,$col_red,sprintf("%.1f%%",$cache['num_misses']*100/$s));
		break;
	
	}
	header("Content-type: image/png");
	imagepng($image);
	exit;
}

// pretty printer for byte values
//
function bsize($s) {
	foreach (array('','K','M','G') as $i => $k) {
		if ($s < 1024) break;
		$s/=1024;
	}
	return sprintf("%5.1f %sBytes",$s,$k);
}

// sortable table header in "scripts for this host" view
function sortheader($key,$name,$extra='') {
	global $MYREQUEST, $MY_SELF_WO_SORT;
	
	if ($MYREQUEST['SORT1']==$key) {
		$MYREQUEST['SORT2'] = $MYREQUEST['SORT2']=='A' ? 'D' : 'A';
	}
	return "<a class=sortable href=\"$MY_SELF_WO_SORT$extra&SORT1=$key&SORT2=".$MYREQUEST['SORT2']."\">$name</a>";

}

// create menu entry 
function menu_entry($ob,$title) {
	global $MYREQUEST,$MY_SELF;
	if ($MYREQUEST['OB']!=$ob) {
		return "<li><a href=\"$MY_SELF&OB=$ob\">$title</a></li>";
	} else if (empty($MYREQUEST['SH'])) {
		return "<li><span class=active>$title</span></li>";
	} else {
		return "<li><a class=\"child_active\" href=\"$MY_SELF&OB=$ob\">$title</a></li>";	
	}
}

function put_login_link($s="Login")
{
	global $MY_SELF,$MYREQUEST,$AUTHENTICATED;
	// needs ADMIN_PASSWORD to be changed!
	//
	if (!USE_AUTHENTICATION) {
		return;
	} else if (ADMIN_PASSWORD=='password')
	{
		print <<<EOB
			<a href="#" onClick="javascript:alert('You need to set a password at the top of apc.php before this will work!');return false";>$s</a>
EOB;
	} else if ($AUTHENTICATED) {
		print <<<EOB
			'{$_SERVER['PHP_AUTH_USER']}'&nbsp;logged&nbsp;in!
EOB;
	} else{
		print <<<EOB
			<a href="$MY_SELF&LO=1&OB={$MYREQUEST['OB']}">$s</a>
EOB;
	}
}

function block_sort($array1, $array2)
{
	if ($array1['offset'] > $array2['offset']) {
		return 1;
	} else {
		return -1;
	}
}


?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head><title>APC INFO <?php echo $host ?></title>
<style><!--
body { background:white; font-size:100.01%; margin:0; padding:0; }
body,p,td,th,input,submit { font-size:0.8em;font-family:arial,helvetica,sans-serif; }
* html body   {font-size:0.8em}
* html p      {font-size:0.8em}
* html td     {font-size:0.8em}
* html th     {font-size:0.8em}
* html input  {font-size:0.8em}
* html submit {font-size:0.8em}
td { vertical-align:top }
a { color:black; font-weight:none; text-decoration:none; }
a:hover { text-decoration:underline; }
div.content { padding:1em 1em 1em 1em; position:absolute; width:97%; z-index:100; }


div.head div.login {
	position:absolute;
	right: 1em;
	top: 1.2em;
	color:white;
	width:6em;
	}
div.head div.login a {
	position:absolute;
	right: 0em;
	background:rgb(119,123,180);
	border:solid rgb(102,102,153) 2px;
	color:white;
	font-weight:bold;
	padding:0.1em 0.5em 0.1em 0.5em;
	text-decoration:none;
	}
div.head div.login a:hover {
	background:rgb(193,193,244);
	}

h1.apc { background:rgb(153,153,204); margin:0; padding:0.5em 1em 0.5em 1em; }
* html h1.apc { margin-bottom:-7px; }
h1.apc a:hover { text-decoration:none; color:rgb(90,90,90); }
h1.apc div.logo span.logo {
	background:rgb(119,123,180);
	color:black;
	border-right: solid black 1px;
	border-bottom: solid black 1px;
	font-style:italic;
	font-size:1em;
	padding-left:1.2em;
	padding-right:1.2em;
	text-align:right;
	}
h1.apc div.logo span.name { color:white; font-size:0.7em; padding:0 0.8em 0 2em; }
h1.apc div.nameinfo { color:white; display:inline; font-size:0.4em; margin-left: 3em; }
h1.apc div.copy { color:black; font-size:0.4em; position:absolute; right:1em; }
hr.apc {
	background:white;
	border-bottom:solid rgb(102,102,153) 1px;
	border-style:none;
	border-top:solid rgb(102,102,153) 10px;
	height:12px;
	margin:0;
	margin-top:1px;
	padding:0;
}

ol,menu { margin:1em 0 0 0; padding:0.2em; margin-left:1em;}
ol.menu li { display:inline; margin-right:0.7em; list-style:none; font-size:85%}
ol.menu a {
	background:rgb(153,153,204);
	border:solid rgb(102,102,153) 2px;
	color:white;
	font-weight:bold;
	margin-right:0em;
	padding:0.1em 0.5em 0.1em 0.5em;
	text-decoration:none;
	margin-left: 5px;
	}
ol.menu a.child_active {
	background:rgb(153,153,204);
	border:solid rgb(102,102,153) 2px;
	color:white;
	font-weight:bold;
	margin-right:0em;
	padding:0.1em 0.5em 0.1em 0.5em;
	text-decoration:none;
	border-left: solid black 5px;
	margin-left: 0px;
	}
ol.menu span.active {
	background:rgb(153,153,204);
	border:solid rgb(102,102,153) 2px;
	color:black;
	font-weight:bold;
	margin-right:0em;
	padding:0.1em 0.5em 0.1em 0.5em;
	text-decoration:none;
	border-left: solid black 5px;
	}
ol.menu span.inactive {
	background:rgb(193,193,244);
	border:solid rgb(182,182,233) 2px;
	color:white;
	font-weight:bold;
	margin-right:0em;
	padding:0.1em 0.5em 0.1em 0.5em;
	text-decoration:none;
	margin-left: 5px;
	}
ol.menu a:hover {
	background:rgb(193,193,244);
	text-decoration:none;
	}
	
	
div.info {
	background:rgb(204,204,204);
	border:solid rgb(204,204,204) 1px;
	margin-bottom:1em;
	}
div.info h2 {
	background:rgb(204,204,204);
	color:black;
	font-size:1em;
	margin:0;
	padding:0.1em 1em 0.1em 1em;
	}
div.info table {
	border:solid rgb(204,204,204) 1px;
	border-spacing:0;
	width:100%;
	}
div.info table th {
	background:rgb(204,204,204);
	color:white;
	margin:0;
	padding:0.1em 1em 0.1em 1em;
	}
div.info table th a.sortable { color:black; }
div.info table tr.tr-0 { background:rgb(238,238,238); }
div.info table tr.tr-1 { background:rgb(221,221,221); }
div.info table td { padding:0.3em 1em 0.3em 1em; }
div.info table td.td-0 { border-right:solid rgb(102,102,153) 1px; white-space:nowrap; }
div.info table td.td-n { border-right:solid rgb(102,102,153) 1px; }
div.info table td h3 {
	color:black;
	font-size:1.1em;
	margin-left:-0.3em;
	}

div.graph { margin-bottom:1em }
div.graph h2 { background:rgb(204,204,204);; color:black; font-size:1em; margin:0; padding:0.1em 1em 0.1em 1em; }
div.graph table { border:solid rgb(204,204,204) 1px; color:black; font-weight:normal; width:100%; }
div.graph table td.td-0 { background:rgb(238,238,238); }
div.graph table td.td-1 { background:rgb(221,221,221); }
div.graph table td { padding:0.2em 1em 0.4em 1em; }

div.div1,div.div2 { margin-bottom:1em; width:35em; }
div.div3 { position:absolute; left:40em; top:1em; width:580px; }
//div.div3 { position:absolute; left:37em; top:1em; right:1em; }

div.sorting { margin:1.5em 0em 1.5em 2em }
.center { text-align:center }
.aright { position:absolute;right:1em }
.right { text-align:right }
.ok { color:rgb(0,200,0); font-weight:bold}
.failed { color:rgb(200,0,0); font-weight:bold}

span.box {
	border: black solid 1px;
	border-right:solid black 2px;
	border-bottom:solid black 2px;
	padding:0 0.5em 0 0.5em;
	margin-right:1em;
}
span.green { background:#60F060; padding:0 0.5em 0 0.5em}
span.red { background:#D06030; padding:0 0.5em 0 0.5em }

div.authneeded {
	background:rgb(238,238,238);
	border:solid rgb(204,204,204) 1px;
	color:rgb(200,0,0);
	font-size:1.2em;
	font-weight:bold;
	padding:2em;
	text-align:center;
	}
	
input {
	background:rgb(153,153,204);
	border:solid rgb(102,102,153) 2px;
	color:white;
	font-weight:bold;
	margin-right:1em;
	padding:0.1em 0.5em 0.1em 0.5em;
	}
//-->
</style>
</head>
<body>
<div class="head">
	<h1 class="apc">
		<div class="logo"><span class="logo"><a href="http://pecl.php.net/package/APC">APC</a></span></div>
		<div class="nameinfo">Opcode Cache</div>
	</h1>
	<div class="login">
	<?php put_login_link(); ?>
	</div>
	<hr class="apc">
</div>
<?php


// Display main Menu
echo <<<EOB
	<ol class=menu>
	<li><a href="$MY_SELF&OB={$MYREQUEST['OB']}&SH={$MYREQUEST['SH']}">Refresh Data</a></li>
EOB;
echo
	menu_entry(1,'View Host Stats'),
	menu_entry(2,'System Cache Entries');
if ($AUTHENTICATED) {
	echo menu_entry(4,'Per-Directory Entries');
}
echo
	menu_entry(3,'User Cache Entries'),
	menu_entry(9,'Version Check');
	
if ($AUTHENTICATED) {
	echo <<<EOB
		<li><a class="aright" href="$MY_SELF&CC=1&OB={$MYREQUEST['OB']}" onClick="javascript:return confirm('Are you sure?');">Clear $cache_mode Cache</a></li>
EOB;
}
echo <<<EOB
	</ol>
EOB;


// CONTENT
echo <<<EOB
	<div class=content>
EOB;

// MAIN SWITCH STATEMENT 

switch ($MYREQUEST['OB']) {





// -----------------------------------------------
// Host Stats
// -----------------------------------------------
case OB_HOST_STATS:
	$mem_size = $mem['num_seg']*$mem['seg_size'];
	$mem_avail= $mem['avail_mem'];
	$mem_used = $mem_size-$mem_avail;
	$seg_size = bsize($mem['seg_size']);
	$req_rate = sprintf("%.2f",($cache['num_hits']+$cache['num_misses'])/($time-$cache['start_time']));
	$hit_rate = sprintf("%.2f",($cache['num_hits'])/($time-$cache['start_time']));
	$miss_rate = sprintf("%.2f",($cache['num_misses'])/($time-$cache['start_time']));
	$insert_rate = sprintf("%.2f",($cache['num_inserts'])/($time-$cache['start_time']));
	$req_rate_user = sprintf("%.2f",($cache_user['num_hits']+$cache_user['num_misses'])/($time-$cache_user['start_time']));
	$hit_rate_user = sprintf("%.2f",($cache_user['num_hits'])/($time-$cache_user['start_time']));
	$miss_rate_user = sprintf("%.2f",($cache_user['num_misses'])/($time-$cache_user['start_time']));
	$insert_rate_user = sprintf("%.2f",($cache_user['num_inserts'])/($time-$cache_user['start_time']));
	$apcversion = phpversion('apc');
	$phpversion = phpversion();
	$number_files = $cache['num_entries']; 
    $size_files = bsize($cache['mem_size']);
	$number_vars = $cache_user['num_entries'];
    $size_vars = bsize($cache_user['mem_size']);
	$i=0;
	echo <<< EOB
		<div class="info div1"><h2>General Cache Information</h2>
		<table cellspacing=0><tbody>
		<tr class=tr-0><td class=td-0>APC Version</td><td>$apcversion</td></tr>
		<tr class=tr-1><td class=td-0>PHP Version</td><td>$phpversion</td></tr>
EOB;

	if(!empty($_SERVER['SERVER_NAME']))
		echo "<tr class=tr-0><td class=td-0>APC Host</td><td>{$_SERVER['SERVER_NAME']} $host</td></tr>\n";
	if(!empty($_SERVER['SERVER_SOFTWARE']))
		echo "<tr class=tr-1><td class=td-0>Server Software</td><td>{$_SERVER['SERVER_SOFTWARE']}</td></tr>\n";

	echo <<<EOB
		<tr class=tr-0><td class=td-0>Shared Memory</td><td>{$mem['num_seg']} Segment(s) with $seg_size 
    <br/> ({$cache['memory_type']} memory, {$cache['locking_type']} locking)
    </td></tr>
EOB;
	echo   '<tr class=tr-1><td class=td-0>Start Time</td><td>',date(DATE_FORMAT,$cache['start_time']),'</td></tr>';
	echo   '<tr class=tr-0><td class=td-0>Uptime</td><td>',duration($cache['start_time']),'</td></tr>';
	echo   '<tr class=tr-1><td class=td-0>File Upload Support</td><td>',$cache['file_upload_progress'],'</td></tr>';
	echo <<<EOB
		</tbody></table>
		</div>

		<div class="info div1"><h2>File Cache Information</h2>
		<table cellspacing=0><tbody>
		<tr class=tr-0><td class=td-0>Cached Files</td><td>$number_files ($size_files)</td></tr>
		<tr class=tr-1><td class=td-0>Hits</td><td>{$cache['num_hits']}</td></tr>
		<tr class=tr-0><td class=td-0>Misses</td><td>{$cache['num_misses']}</td></tr>
		<tr class=tr-1><td class=td-0>Request Rate (hits, misses)</td><td>$req_rate cache requests/second</td></tr>
		<tr class=tr-0><td class=td-0>Hit Rate</td><td>$hit_rate cache requests/second</td></tr>
		<tr class=tr-1><td class=td-0>Miss Rate</td><td>$miss_rate cache requests/second</td></tr>
		<tr class=tr-0><td class=td-0>Insert Rate</td><td>$insert_rate cache requests/second</td></tr>
		<tr class=tr-1><td class=td-0>Cache full count</td><td>{$cache['expunges']}</td></tr>
		</tbody></table>
		</div>

		<div class="info div1"><h2>User Cache Information</h2>
		<table cellspacing=0><tbody>
    <tr class=tr-0><td class=td-0>Cached Variables</td><td>$number_vars ($size_vars)</td></tr>
		<tr class=tr-1><td class=td-0>Hits</td><td>{$cache_user['num_hits']}</td></tr>
		<tr class=tr-0><td class=td-0>Misses</td><td>{$cache_user['num_misses']}</td></tr>
		<tr class=tr-1><td class=td-0>Request Rate (hits, misses)</td><td>$req_rate_user cache requests/second</td></tr>
		<tr class=tr-0><td class=td-0>Hit Rate</td><td>$hit_rate_user cache requests/second</td></tr>
		<tr class=tr-1><td class=td-0>Miss Rate</td><td>$miss_rate_user cache requests/second</td></tr>
		<tr class=tr-0><td class=td-0>Insert Rate</td><td>$insert_rate_user cache requests/second</td></tr>
		<tr class=tr-1><td class=td-0>Cache full count</td><td>{$cache_user['expunges']}</td></tr>

		</tbody></table>
		</div>

		<div class="info div2"><h2>Runtime Settings</h2><table cellspacing=0><tbody>
EOB;

	$j = 0;
	foreach (ini_get_all('apc') as $k => $v) {
		echo "<tr class=tr-$j><td class=td-0>",$k,"</td><td>",str_replace(',',',<br />',$v['local_value']),"</td></tr>\n";
		$j = 1 - $j;
	}

	if($mem['num_seg']>1 || $mem['num_seg']==1 && count($mem['block_lists'][0])>1)
		$mem_note = "Memory Usage<br /><font size=-2>(multiple slices indicate fragments)</font>";
	else
		$mem_note = "Memory Usage";

	echo <<< EOB
		</tbody></table>
		</div>

		<div class="graph div3"><h2>Host Status Diagrams</h2>
		<table cellspacing=0><tbody>
EOB;
	$size='width='.(GRAPH_SIZE+50).' height='.(GRAPH_SIZE+10);
	echo <<<EOB
		<tr>
		<td class=td-0>$mem_note</td>
		<td class=td-1>Hits &amp; Misses</td>
		</tr>
EOB;

	echo
		graphics_avail() ? 
			  '<tr>'.
			  "<td class=td-0><img alt=\"\" $size src=\"$PHP_SELF?IMG=1&$time\"></td>".
			  "<td class=td-1><img alt=\"\" $size src=\"$PHP_SELF?IMG=2&$time\"></td></tr>\n"
			: "",
		'<tr>',
		'<td class=td-0><span class="green box">&nbsp;</span>Free: ',bsize($mem_avail).sprintf(" (%.1f%%)",$mem_avail*100/$mem_size),"</td>\n",
		'<td class=td-1><span class="green box">&nbsp;</span>Hits: ',$cache['num_hits'].sprintf(" (%.1f%%)",$cache['num_hits']*100/($cache['num_hits']+$cache['num_misses'])),"</td>\n",
		'</tr>',
		'<tr>',
		'<td class=td-0><span class="red box">&nbsp;</span>Used: ',bsize($mem_used ).sprintf(" (%.1f%%)",$mem_used *100/$mem_size),"</td>\n",
		'<td class=td-1><span class="red box">&nbsp;</span>Misses: ',$cache['num_misses'].sprintf(" (%.1f%%)",$cache['num_misses']*100/($cache['num_hits']+$cache['num_misses'])),"</td>\n";
	echo <<< EOB
		</tr>
		</tbody></table>

		<br/>
		<h2>Detailed Memory Usage and Fragmentation</h2>
		<table cellspacing=0><tbody>
		<tr>
		<td class=td-0 colspan=2><br/>
EOB;

	// Fragementation: (freeseg - 1) / total_seg
	$nseg = $freeseg = $fragsize = $freetotal = 0;
	for($i=0; $i<$mem['num_seg']; $i++) {
		$ptr = 0;
		foreach($mem['block_lists'][$i] as $block) {
			if ($block['offset'] != $ptr) {
				++$nseg;
			}
			$ptr = $block['offset'] + $block['size'];
                        /* Only consider blocks <5M for the fragmentation % */
                        if($block['size']<(5*1024*1024)) $fragsize+=$block['size'];
                        $freetotal+=$block['size'];
		}
		$freeseg += count($mem['block_lists'][$i]);
	}
	
	if ($freeseg > 1) {
		$frag = sprintf("%.2f%% (%s out of %s in %d fragments)", ($fragsize/$freetotal)*100,bsize($fragsize),bsize($freetotal),$freeseg);
	} else {
		$frag = "0%";
	}

	if (graphics_avail()) {
		$size='width='.(2*GRAPH_SIZE+150).' height='.(GRAPH_SIZE+10);
		echo <<<EOB
			<img alt="" $size src="$PHP_SELF?IMG=3&$time">
EOB;
	}
	echo <<<EOB
		</br>Fragmentation: $frag
		</td>
		</tr>
EOB;
        if(isset($mem['adist'])) {
          foreach($mem['adist'] as $i=>$v) {
            $cur = pow(2,$i); $nxt = pow(2,$i+1)-1;
            if($i==0) $range = "1";
            else $range = "$cur - $nxt";
            echo "<tr><th align=right>$range</th><td align=right>$v</td></tr>\n";
          }
        }
        echo <<<EOB
		</tbody></table>
		</div>
EOB;
		
	break;


// -----------------------------------------------
// User Cache Entries
// -----------------------------------------------
case OB_USER_CACHE:
	if (!$AUTHENTICATED) {
    echo '<div class="error">You need to login to see the user values here!<br/>&nbsp;<br/>';
		put_login_link("Login now!");
		echo '</div>';
		break;
	}
	$fieldname='info';
	$fieldheading='User Entry Label';
	$fieldkey='info';

// -----------------------------------------------
// System Cache Entries		
// -----------------------------------------------
case OB_SYS_CACHE:	
	if (!isset($fieldname))
	{
		$fieldname='filename';
		$fieldheading='Script Filename';
		if(ini_get("apc.stat")) $fieldkey='inode';
		else $fieldkey='filename'; 
	}
	if (!empty($MYREQUEST['SH']))
	{
		echo <<< EOB
			<div class="info"><table cellspacing=0><tbody>
			<tr><th>Attribute</th><th>Value</th></tr>
EOB;

		$m=0;
		foreach($scope_list as $j => $list) {
			foreach($cache[$list] as $i => $entry) {
				if (md5($entry[$fieldkey])!=$MYREQUEST['SH']) continue;
				foreach($entry as $k => $value) {
					if (!$AUTHENTICATED) {
						// hide all path entries if not logged in
						$value=preg_replace('/^.*(\\/|\\\\)/','<i>&lt;hidden&gt;</i>/',$value);
					}

					if ($k == "num_hits") {
						$value=sprintf("%s (%.2f%%)",$value,$value*100/$cache['num_hits']);
					}
					if ($k == 'deletion_time') {
						if(!$entry['deletion_time']) $value = "None";
					}
					echo
						"<tr class=tr-$m>",
						"<td class=td-0>",ucwords(preg_replace("/_/"," ",$k)),"</td>",
						"<td class=td-last>",(preg_match("/time/",$k) && $value!='None') ? date(DATE_FORMAT,$value) : htmlspecialchars($value, ENT_QUOTES, 'UTF-8'),"</td>",
						"</tr>";
					$m=1-$m;
				}
				if($fieldkey=='info') {
					echo "<tr class=tr-$m><td class=td-0>Stored Value</td><td class=td-last><pre>";
					$output = var_export(apc_fetch($entry[$fieldkey]),true);
					echo htmlspecialchars($output, ENT_QUOTES, 'UTF-8');
					echo "</pre></td></tr>\n";
				}
				break;
			}
		}

		echo <<<EOB
			</tbody></table>
			</div>
EOB;
		break;
	}

	$cols=6;
	echo <<<EOB
		<div class=sorting><form>Scope:
		<input type=hidden name=OB value={$MYREQUEST['OB']}>
		<select name=SCOPE>
EOB;
	echo 
		"<option value=A",$MYREQUEST['SCOPE']=='A' ? " selected":"",">Active</option>",
		"<option value=D",$MYREQUEST['SCOPE']=='D' ? " selected":"",">Deleted</option>",
		"</select>",
		", Sorting:<select name=SORT1>",
		"<option value=H",$MYREQUEST['SORT1']=='H' ? " selected":"",">Hits</option>",
		"<option value=Z",$MYREQUEST['SORT1']=='Z' ? " selected":"",">Size</option>",
		"<option value=S",$MYREQUEST['SORT1']=='S' ? " selected":"",">$fieldheading</option>",
		"<option value=A",$MYREQUEST['SORT1']=='A' ? " selected":"",">Last accessed</option>",
		"<option value=M",$MYREQUEST['SORT1']=='M' ? " selected":"",">Last modified</option>",
		"<option value=C",$MYREQUEST['SORT1']=='C' ? " selected":"",">Created at</option>",
		"<option value=D",$MYREQUEST['SORT1']=='D' ? " selected":"",">Deleted at</option>";
	if($fieldname=='info') echo
		"<option value=D",$MYREQUEST['SORT1']=='T' ? " selected":"",">Timeout</option>";
	echo 
		'</select>',
		'<select name=SORT2>',
		'<option value=D',$MYREQUEST['SORT2']=='D' ? ' selected':'','>DESC</option>',
		'<option value=A',$MYREQUEST['SORT2']=='A' ? ' selected':'','>ASC</option>',
		'</select>',
		'<select name=COUNT onChange="form.submit()">',
		'<option value=10 ',$MYREQUEST['COUNT']=='10' ? ' selected':'','>Top 10</option>',
		'<option value=20 ',$MYREQUEST['COUNT']=='20' ? ' selected':'','>Top 20</option>',
		'<option value=50 ',$MYREQUEST['COUNT']=='50' ? ' selected':'','>Top 50</option>',
		'<option value=100',$MYREQUEST['COUNT']=='100'? ' selected':'','>Top 100</option>',
		'<option value=150',$MYREQUEST['COUNT']=='150'? ' selected':'','>Top 150</option>',
		'<option value=200',$MYREQUEST['COUNT']=='200'? ' selected':'','>Top 200</option>',
		'<option value=500',$MYREQUEST['COUNT']=='500'? ' selected':'','>Top 500</option>',
		'<option value=0  ',$MYREQUEST['COUNT']=='0'  ? ' selected':'','>All</option>',
		'</select>',
    '&nbsp; Search: <input name=SEARCH value="',$MYREQUEST['SEARCH'],'" type=text size=25/>',
		'&nbsp;<input type=submit value="GO!">',
		'</form></div>';

  if (isset($MYREQUEST['SEARCH'])) {
   // Don't use preg_quote because we want the user to be able to specify a
   // regular expression subpattern.
   $MYREQUEST['SEARCH'] = '/'.str_replace('/', '\\/', $MYREQUEST['SEARCH']).'/i';
   if (preg_match($MYREQUEST['SEARCH'], 'test') === false) {
     echo '<div class="error">Error: enter a valid regular expression as a search query.</div>';
     break;
   }
  }

  echo
		'<div class="info"><table cellspacing=0><tbody>',
		'<tr>',
		'<th>',sortheader('S',$fieldheading,  "&OB=".$MYREQUEST['OB']),'</th>',
		'<th>',sortheader('H','Hits',         "&OB=".$MYREQUEST['OB']),'</th>',
		'<th>',sortheader('Z','Size',         "&OB=".$MYREQUEST['OB']),'</th>',
		'<th>',sortheader('A','Last accessed',"&OB=".$MYREQUEST['OB']),'</th>',
		'<th>',sortheader('M','Last modified',"&OB=".$MYREQUEST['OB']),'</th>',
		'<th>',sortheader('C','Created at',   "&OB=".$MYREQUEST['OB']),'</th>';

	if($fieldname=='info') {
		$cols+=2;
		 echo '<th>',sortheader('T','Timeout',"&OB=".$MYREQUEST['OB']),'</th>';
	}
	echo '<th>',sortheader('D','Deleted at',"&OB=".$MYREQUEST['OB']),'</th></tr>';

	// builds list with alpha numeric sortable keys
	//
	$list = array();
	foreach($cache[$scope_list[$MYREQUEST['SCOPE']]] as $i => $entry) {
		switch($MYREQUEST['SORT1']) {
			case 'A': $k=sprintf('%015d-',$entry['access_time']); 	break;
			case 'H': $k=sprintf('%015d-',$entry['num_hits']); 		break;
			case 'Z': $k=sprintf('%015d-',$entry['mem_size']); 		break;
			case 'M': $k=sprintf('%015d-',$entry['mtime']);			break;
			case 'C': $k=sprintf('%015d-',$entry['creation_time']);	break;
			case 'T': $k=sprintf('%015d-',$entry['ttl']);			break;
			case 'D': $k=sprintf('%015d-',$entry['deletion_time']);	break;
			case 'S': $k='';										break;
		}
		if (!$AUTHENTICATED) {
			// hide all path entries if not logged in
			$list[$k.$entry[$fieldname]]=preg_replace('/^.*(\\/|\\\\)/','*hidden*/',$entry);
		} else {
			$list[$k.$entry[$fieldname]]=$entry;
		}
	}

	if ($list) {
		
		// sort list
		//
		switch ($MYREQUEST['SORT2']) {
			case "A":	krsort($list);	break;
			case "D":	ksort($list);	break;
		}
		
		// output list
		$i=0;
		foreach($list as $k => $entry) {
      if(!$MYREQUEST['SEARCH'] || preg_match($MYREQUEST['SEARCH'], $entry[$fieldname]) != 0) {  
        $field_value = htmlentities(strip_tags($entry[$fieldname],''), ENT_QUOTES, 'UTF-8');
        echo
          '<tr class=tr-',$i%2,'>',
          "<td class=td-0><a href=\"$MY_SELF&OB=",$MYREQUEST['OB'],"&SH=",md5($entry[$fieldkey]),"\">",$field_value,'</a></td>',
          '<td class="td-n center">',$entry['num_hits'],'</td>',
          '<td class="td-n right">',$entry['mem_size'],'</td>',
          '<td class="td-n center">',date(DATE_FORMAT,$entry['access_time']),'</td>',
          '<td class="td-n center">',date(DATE_FORMAT,$entry['mtime']),'</td>',
          '<td class="td-n center">',date(DATE_FORMAT,$entry['creation_time']),'</td>';

        if($fieldname=='info') {
          if($entry['ttl'])
            echo '<td class="td-n center">'.$entry['ttl'].' seconds</td>';
          else
            echo '<td class="td-n center">None</td>';
        }
        if ($entry['deletion_time']) {

          echo '<td class="td-last center">', date(DATE_FORMAT,$entry['deletion_time']), '</td>';
        } else if ($MYREQUEST['OB'] == OB_USER_CACHE) {

          echo '<td class="td-last center">';
          echo '[<a href="', $MY_SELF, '&OB=', $MYREQUEST['OB'], '&DU=', urlencode($entry[$fieldkey]), '">Delete Now</a>]';
          echo '</td>';
        } else {
          echo '<td class="td-last center"> &nbsp; </td>';
        }
        echo '</tr>';
        $i++;
        if ($i == $MYREQUEST['COUNT'])
          break;
      }
		}
		
	} else {
		echo '<tr class=tr-0><td class="center" colspan=',$cols,'><i>No data</i></td></tr>';
	}
	echo <<< EOB
		</tbody></table>
EOB;

	if ($list && $i < count($list)) {
		echo "<a href=\"$MY_SELF&OB=",$MYREQUEST['OB'],"&COUNT=0\"><i>",count($list)-$i,' more available...</i></a>';
	}

	echo <<< EOB
		</div>
EOB;
	break;


// -----------------------------------------------
// Per-Directory System Cache Entries
// -----------------------------------------------
case OB_SYS_CACHE_DIR:	
	if (!$AUTHENTICATED) {
		break;
	}

	echo <<<EOB
		<div class=sorting><form>Scope:
		<input type=hidden name=OB value={$MYREQUEST['OB']}>
		<select name=SCOPE>
EOB;
	echo 
		"<option value=A",$MYREQUEST['SCOPE']=='A' ? " selected":"",">Active</option>",
		"<option value=D",$MYREQUEST['SCOPE']=='D' ? " selected":"",">Deleted</option>",
		"</select>",
		", Sorting:<select name=SORT1>",
		"<option value=H",$MYREQUEST['SORT1']=='H' ? " selected":"",">Total Hits</option>",
		"<option value=Z",$MYREQUEST['SORT1']=='Z' ? " selected":"",">Total Size</option>",
		"<option value=T",$MYREQUEST['SORT1']=='T' ? " selected":"",">Number of Files</option>",
		"<option value=S",$MYREQUEST['SORT1']=='S' ? " selected":"",">Directory Name</option>",
		"<option value=A",$MYREQUEST['SORT1']=='A' ? " selected":"",">Avg. Size</option>",
		"<option value=C",$MYREQUEST['SORT1']=='C' ? " selected":"",">Avg. Hits</option>",
		'</select>',
		'<select name=SORT2>',
		'<option value=D',$MYREQUEST['SORT2']=='D' ? ' selected':'','>DESC</option>',
		'<option value=A',$MYREQUEST['SORT2']=='A' ? ' selected':'','>ASC</option>',
		'</select>',
		'<select name=COUNT onChange="form.submit()">',
		'<option value=10 ',$MYREQUEST['COUNT']=='10' ? ' selected':'','>Top 10</option>',
		'<option value=20 ',$MYREQUEST['COUNT']=='20' ? ' selected':'','>Top 20</option>',
		'<option value=50 ',$MYREQUEST['COUNT']=='50' ? ' selected':'','>Top 50</option>',
		'<option value=100',$MYREQUEST['COUNT']=='100'? ' selected':'','>Top 100</option>',
		'<option value=150',$MYREQUEST['COUNT']=='150'? ' selected':'','>Top 150</option>',
		'<option value=200',$MYREQUEST['COUNT']=='200'? ' selected':'','>Top 200</option>',
		'<option value=500',$MYREQUEST['COUNT']=='500'? ' selected':'','>Top 500</option>',
		'<option value=0  ',$MYREQUEST['COUNT']=='0'  ? ' selected':'','>All</option>',
		'</select>',
		", Group By Dir Level:<select name=AGGR>",
		"<option value='' selected>None</option>";
		for ($i = 1; $i < 10; $i++)
			echo "<option value=$i",$MYREQUEST['AGGR']==$i ? " selected":"",">$i</option>";
		echo '</select>',
		'&nbsp;<input type=submit value="GO!">',
		'</form></div>',

		'<div class="info"><table cellspacing=0><tbody>',
		'<tr>',
		'<th>',sortheader('S','Directory Name',	"&OB=".$MYREQUEST['OB']),'</th>',
		'<th>',sortheader('T','Number of Files',"&OB=".$MYREQUEST['OB']),'</th>',
		'<th>',sortheader('H','Total Hits',	"&OB=".$MYREQUEST['OB']),'</th>',
		'<th>',sortheader('Z','Total Size',	"&OB=".$MYREQUEST['OB']),'</th>',
		'<th>',sortheader('C','Avg. Hits',	"&OB=".$MYREQUEST['OB']),'</th>',
		'<th>',sortheader('A','Avg. Size',	"&OB=".$MYREQUEST['OB']),'</th>',
		'</tr>';

	// builds list with alpha numeric sortable keys
	//
	$tmp = $list = array();
	foreach($cache[$scope_list[$MYREQUEST['SCOPE']]] as $entry) {
		$n = dirname($entry['filename']);
		if ($MYREQUEST['AGGR'] > 0) {
			$n = preg_replace("!^(/?(?:[^/\\\\]+[/\\\\]){".($MYREQUEST['AGGR']-1)."}[^/\\\\]*).*!", "$1", $n);
		}
		if (!isset($tmp[$n])) {
			$tmp[$n] = array('hits'=>0,'size'=>0,'ents'=>0);
		}
		$tmp[$n]['hits'] += $entry['num_hits'];
		$tmp[$n]['size'] += $entry['mem_size'];
		++$tmp[$n]['ents'];
	}

	foreach ($tmp as $k => $v) {
		switch($MYREQUEST['SORT1']) {
			case 'A': $kn=sprintf('%015d-',$v['size'] / $v['ents']);break;
			case 'T': $kn=sprintf('%015d-',$v['ents']);		break;
			case 'H': $kn=sprintf('%015d-',$v['hits']);		break;
			case 'Z': $kn=sprintf('%015d-',$v['size']);		break;
			case 'C': $kn=sprintf('%015d-',$v['hits'] / $v['ents']);break;
			case 'S': $kn = $k;					break;
		}
		$list[$kn.$k] = array($k, $v['ents'], $v['hits'], $v['size']);
	}

	if ($list) {
		
		// sort list
		//
		switch ($MYREQUEST['SORT2']) {
			case "A":	krsort($list);	break;
			case "D":	ksort($list);	break;
		}
		
		// output list
		$i = 0;
		foreach($list as $entry) {
			echo
				'<tr class=tr-',$i%2,'>',
				"<td class=td-0>",$entry[0],'</a></td>',
				'<td class="td-n center">',$entry[1],'</td>',
				'<td class="td-n center">',$entry[2],'</td>',
				'<td class="td-n center">',$entry[3],'</td>',
				'<td class="td-n center">',round($entry[2] / $entry[1]),'</td>',
				'<td class="td-n center">',round($entry[3] / $entry[1]),'</td>',
				'</tr>';

			if (++$i == $MYREQUEST['COUNT']) break;
		}
		
	} else {
		echo '<tr class=tr-0><td class="center" colspan=6><i>No data</i></td></tr>';
	}
	echo <<< EOB
		</tbody></table>
EOB;

	if ($list && $i < count($list)) {
		echo "<a href=\"$MY_SELF&OB=",$MYREQUEST['OB'],"&COUNT=0\"><i>",count($list)-$i,' more available...</i></a>';
	}

	echo <<< EOB
		</div>
EOB;
	break;

// -----------------------------------------------
// Version check
// -----------------------------------------------
case OB_VERSION_CHECK:
	echo <<<EOB
		<div class="info"><h2>APC Version Information</h2>
		<table cellspacing=0><tbody>
		<tr>
		<th></th>
		</tr>
EOB;
  if (defined('PROXY')) {
    $ctxt = stream_context_create( array( 'http' => array( 'proxy' => PROXY, 'request_fulluri' => True ) ) );
    $rss = @file_get_contents("http://pecl.php.net/feeds/pkg_apc.rss", False, $ctxt);
  } else {
    $rss = @file_get_contents("http://pecl.php.net/feeds/pkg_apc.rss");
  }
	if (!$rss) {
		echo '<tr class="td-last center"><td>Unable to fetch version information.</td></tr>';
	} else {
		$apcversion = phpversion('apc');

		preg_match('!<title>APC ([0-9.]+)</title>!', $rss, $match);
		echo '<tr class="tr-0 center"><td>';
		if (version_compare($apcversion, $match[1], '>=')) {
			echo '<div class="ok">You are running the latest version of APC ('.$apcversion.')</div>';
			$i = 3;
		} else {
			echo '<div class="failed">You are running an older version of APC ('.$apcversion.'), 
				newer version '.$match[1].' is available at <a href="http://pecl.php.net/package/APC/'.$match[1].'">
				http://pecl.php.net/package/APC/'.$match[1].'</a>
				</div>';
			$i = -1;
		}
		echo '</td></tr>';
		echo '<tr class="tr-0"><td><h3>Change Log:</h3><br/>';

		preg_match_all('!<(title|description)>([^<]+)</\\1>!', $rss, $match);
		next($match[2]); next($match[2]);

		while (list(,$v) = each($match[2])) {
			list(,$ver) = explode(' ', $v, 2);
			if ($i < 0 && version_compare($apcversion, $ver, '>=')) {
				break;
			} else if (!$i--) {
				break;
			}
			echo "<b><a href=\"http://pecl.php.net/package/APC/$ver\">".htmlspecialchars($v, ENT_QUOTES, 'UTF-8')."</a></b><br><blockquote>";
			echo nl2br(htmlspecialchars(current($match[2]), ENT_QUOTES, 'UTF-8'))."</blockquote>";
			next($match[2]);
		}
		echo '</td></tr>';
	}
	echo <<< EOB
		</tbody></table>
		</div>
EOB;
	break;

}

echo <<< EOB
	</div>
EOB;

?>

<!-- <?php echo "\nBased on APCGUI By R.Becker\n$VERSION\n"?> -->
</body>
</html>
