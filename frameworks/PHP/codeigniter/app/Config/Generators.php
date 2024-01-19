<?php

namespace Config;

use CodeIgniter\Config\BaseConfig;

class Generators extends BaseConfig
{
    /**
     * --------------------------------------------------------------------------
     * Generator Commands' Views
     * --------------------------------------------------------------------------
     *
     * This array defines the mapping of generator commands to the view files
     * they are using. If you need to customize them for your own, copy these
     * view files in your own folder and indicate the location here.
     *
     * You will notice that the views have special placeholders enclosed in
     * curly braces `{...}`. These placeholders are used internally by the
     * generator commands in processing replacements, thus you are warned
     * not to delete them or modify the names. If you will do so, you may
     * end up disrupting the scaffolding process and throw errors.
     *
     * YOU HAVE BEEN WARNED!
     *
     * @var array<string, string>
     */
    public array $views = [
        'make:cell'         => 'CodeIgniter\Commands\Generators\Views\cell.tpl.php',
        'make:cell_view'    => 'CodeIgniter\Commands\Generators\Views\cell_view.tpl.php',
        'make:command'      => 'CodeIgniter\Commands\Generators\Views\command.tpl.php',
        'make:config'       => 'CodeIgniter\Commands\Generators\Views\config.tpl.php',
        'make:controller'   => 'CodeIgniter\Commands\Generators\Views\controller.tpl.php',
        'make:entity'       => 'CodeIgniter\Commands\Generators\Views\entity.tpl.php',
        'make:filter'       => 'CodeIgniter\Commands\Generators\Views\filter.tpl.php',
        'make:migration'    => 'CodeIgniter\Commands\Generators\Views\migration.tpl.php',
        'make:model'        => 'CodeIgniter\Commands\Generators\Views\model.tpl.php',
        'make:seeder'       => 'CodeIgniter\Commands\Generators\Views\seeder.tpl.php',
        'make:validation'   => 'CodeIgniter\Commands\Generators\Views\validation.tpl.php',
        'session:migration' => 'CodeIgniter\Commands\Generators\Views\migration.tpl.php',
    ];
}
