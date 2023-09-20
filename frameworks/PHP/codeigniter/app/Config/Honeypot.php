<?php

namespace Config;

use CodeIgniter\Config\BaseConfig;

class Honeypot extends BaseConfig
{
    /**
     * Makes Honeypot visible or not to human
     */
    public bool $hidden = true;

    /**
     * Honeypot Label Content
     */
    public string $label = 'Fill This Field';

    /**
     * Honeypot Field Name
     */
    public string $name = 'honeypot';

    /**
     * Honeypot HTML Template
     */
    public string $template = '<label>{label}</label><input type="text" name="{name}" value="">';

    /**
     * Honeypot container
     *
     * If you enabled CSP, you can remove `style="display:none"`.
     */
    public string $container = '<div style="display:none">{template}</div>';

    /**
     * The id attribute for Honeypot container tag
     *
     * Used when CSP is enabled.
     */
    public string $containerId = 'hpc';
}
