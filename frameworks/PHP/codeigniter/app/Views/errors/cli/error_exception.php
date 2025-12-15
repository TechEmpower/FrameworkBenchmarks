<?php

use CodeIgniter\CLI\CLI;

// The main Exception
CLI::write('[' . $exception::class . ']', 'light_gray', 'red');
CLI::write($message);
CLI::write('at ' . CLI::color(clean_path($exception->getFile()) . ':' . $exception->getLine(), 'green'));
CLI::newLine();

$last = $exception;

while ($prevException = $last->getPrevious()) {
    $last = $prevException;

    CLI::write('  Caused by:');
    CLI::write('  [' . $prevException::class . ']', 'red');
    CLI::write('  ' . $prevException->getMessage());
    CLI::write('  at ' . CLI::color(clean_path($prevException->getFile()) . ':' . $prevException->getLine(), 'green'));
    CLI::newLine();
}

// The backtrace
if (defined('SHOW_DEBUG_BACKTRACE') && SHOW_DEBUG_BACKTRACE) {
    $backtraces = $last->getTrace();

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

        $args = implode(', ', array_map(static fn ($value): string => match (true) {
            is_object($value) => 'Object(' . $value::class . ')',
            is_array($value)  => $value !== [] ? '[...]' : '[]',
            $value === null   => 'null', // return the lowercased version
            default           => var_export($value, true),
        }, array_values($error['args'] ?? [])));

        $function .= '(' . $args . ')';

        CLI::write($function);
        CLI::newLine();
    }
}
