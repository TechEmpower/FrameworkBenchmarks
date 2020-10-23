<?php declare(strict_types=1);
use DuckPhp\Helper\ViewHelper as V;

// change me if you can
////var_dump(get_defined_vars());

$is_debug = V::IsDebug();
?>
404
<?php
    if ($is_debug) {
        ?>
Developing!
<pre>
<?php debug_print_backtrace(); ?>
</pre>
<?php
    }
?>