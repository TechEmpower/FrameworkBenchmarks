<?php

namespace Cyber;

class Utility
{
    /**
     * Check if required PHP extensions are installed
     *
     * @throws Exception Throws an exception when required extensions are not installed
     * @return bool Returns true if all extensions check passed
     */
    public static function checkPHPenv(): bool
    {
        /* Check PHP version */
        if (version_compare(PHP_VERSION, '8.0', '<')) {
            throw new \Exception('Current PHP (' . PHP_VERSION . ') version is too low! Minimum required PHP version is (8.0)');
        }
        // List of required extensions
        $requiredExtensions =  [
            'json' => 'Handling JSON data',
            'mbstring' => 'Handling multibyte strings',
            'pdo' => 'Database connection',
            'pdo_mysql' => 'MySQL database support',
            'openssl' => 'Encryption and HTTPS support'
        ];
        // Check required extensions
        $missingExtensions = [];
        foreach ($requiredExtensions as $extension => $purpose) {
            if (!extension_loaded($extension)) {
                $missingExtensions[] = sprintf(
                    "- %s (%s)",
                    $extension,
                    $purpose
                );
            }
        }
        // If there are missing required extensions, throw an exception
        if (!empty($missingExtensions)) {
            throw new \Exception(sprintf(
                "Missing required PHP extensions:\n%s\nPlease install these extensions before running the program.",
                implode("\n", $missingExtensions)
            ));
        }
        return true;
    }
}
