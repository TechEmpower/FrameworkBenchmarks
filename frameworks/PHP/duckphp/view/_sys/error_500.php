<?php declare(strict_types=1);
use DuckPhp\Helper\ViewHelper as V;

// change this file if you can.
//var_dump(get_defined_vars());
if ($is_debug) {

    $is_debug = V::IsDebug();
    $class = get_class($ex);

    $code = $ex->getCode();
    $message = $ex->getMessage();
    $file = $ex->getFile();
    $line = $ex->getLine();
    
    $trace = '';
    try{
        $trace = $ex->getTraceString();
    }catch(\Throwable $e){}
    ?>
<fieldset>
    <legend>Exception(<?=$class ?>:<?=$code?>)</legend>
    <?=$message ?>
    <div><?=$file?> : <?=$line?></div>";

<pre>
--
<?=$trace?>
</pre>
</fieldset>
<?php
} else {
        ?>
    500
<?php
}
