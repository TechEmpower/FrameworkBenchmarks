<?php

use CodeIgniter\CLI\CLI;

// The main Exception
CLI::newLine();
CLI::write('[' . get_class($exception) . ']', 'light_gray', 'red');
CLI::newLine();
CLI::write($message);
CLI::newLine();
CLI::write('at ' . CLI::color(clean_path($exception->getFile()) . ':' . $exception->getLine(), 'green'));
CLI::newLine();

// The backtrace
if (defined('SHOW_DEBUG_BACKTRACE') && SHOW_DEBUG_BACKTRACE) {
    $backtraces = $exception->getTrace();

    if ($backtraces) {
        CLI::write('Backtrace:', 'green');
    }

    foreach ($backtraces as $i => $error) {
        $padFile  = '    '; // 4 spaces
        $padClass = '       '; // 7 spaces
        $c        = str_pad($i + 1, 3, ' ', STR_PAD_LEFT);

        if (isset($error['file'])) {
            $filepath = clean_path($error['file']) . ':' . $error['line'];

            CLI::write($c . $padFile . CLI::color($filepath, 'yellow'));
        } else {
            CLI::write($c . $padFile . CLI::color('[internal function]', 'yellow'));
        }

        $function = '';

        if (isset($error['class'])) {
            $type = ($error['type'] === '->') ? '()' . $error['type'] : $error['type'];
            $function .= $padClass . $error['class'] . $type . $error['function'];
        } elseif (! isset($error['class']) && isset($error['function'])) {
            $function .= $padClass . $error['function'];
        }

        $args = implode(', ', array_map(static function ($value) {
            switch (true) {
                case is_object($value):
                    return 'Object(' . get_class($value) . ')';

                case is_array($value):
                    return count($value) ? '[...]' : '[]';

                case $value === null:
                    return 'null'; // return the lowercased version

                default:
                    return var_export($value, true);
            }
        }, array_values($error['args'] ?? [])));

        $function .= '(' . $args . ')';

        CLI::write($function);
        CLI::newLine();
    }
}
