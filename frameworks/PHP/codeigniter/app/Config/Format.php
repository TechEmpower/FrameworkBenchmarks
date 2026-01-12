<?php

namespace Config;

use CodeIgniter\Config\BaseConfig;
use CodeIgniter\Format\JSONFormatter;
use CodeIgniter\Format\XMLFormatter;

class Format extends BaseConfig
{
    /**
     * --------------------------------------------------------------------------
     * Available Response Formats
     * --------------------------------------------------------------------------
     *
     * When you perform content negotiation with the request, these are the
     * available formats that your application supports. This is currently
     * only used with the API\ResponseTrait. A valid Formatter must exist
     * for the specified format.
     *
     * These formats are only checked when the data passed to the respond()
     * method is an array.
     *
     * @var list<string>
     */
    public array $supportedResponseFormats = [
        'application/json',
        'application/xml', // machine-readable XML
        'text/xml', // human-readable XML
    ];

    /**
     * --------------------------------------------------------------------------
     * Formatters
     * --------------------------------------------------------------------------
     *
     * Lists the class to use to format responses with of a particular type.
     * For each mime type, list the class that should be used. Formatters
     * can be retrieved through the getFormatter() method.
     *
     * @var array<string, string>
     */
    public array $formatters = [
        'application/json' => JSONFormatter::class,
        'application/xml'  => XMLFormatter::class,
        'text/xml'         => XMLFormatter::class,
    ];

    /**
     * --------------------------------------------------------------------------
     * Formatters Options
     * --------------------------------------------------------------------------
     *
     * Additional Options to adjust default formatters behaviour.
     * For each mime type, list the additional options that should be used.
     *
     * @var array<string, int>
     */
    public array $formatterOptions = [
        'application/json' => JSON_UNESCAPED_UNICODE | JSON_UNESCAPED_SLASHES,
        'application/xml'  => 0,
        'text/xml'         => 0,
    ];
}
