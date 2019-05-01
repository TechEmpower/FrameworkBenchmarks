<?php
/**
 * CakePHP(tm) : Rapid Development Framework (https://cakephp.org)
 * Copyright (c) Cake Software Foundation, Inc. (https://cakefoundation.org)
 *
 * Licensed under The MIT License
 * For full copyright and license information, please see the LICENSE.txt
 * Redistributions of files must retain the above copyright notice.
 *
 * @copyright     Copyright (c) Cake Software Foundation, Inc. (https://cakefoundation.org)
 * @link          https://cakephp.org CakePHP(tm) Project
 * @since         0.10.0
 * @license       https://opensource.org/licenses/mit-license.php MIT License
 */
use Cake\Cache\Cache;
use Cake\Core\Configure;
use Cake\Core\Plugin;
use Cake\Datasource\ConnectionManager;
use Cake\Error\Debugger;
use Cake\Http\Exception\NotFoundException;

$this->layout = false;

if (!Configure::read('debug')) :
    throw new NotFoundException(
        'Please replace src/Template/Pages/home.ctp with your own version or re-enable debug mode.'
    );
endif;

$cakeDescription = 'CakePHP: the rapid development PHP framework';
?>
<!DOCTYPE html>
<html>
<head>
    <?= $this->Html->charset() ?>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>
        <?= $cakeDescription ?>
    </title>

    <?= $this->Html->meta('icon') ?>
    <?= $this->Html->css('base.css') ?>
    <?= $this->Html->css('style.css') ?>
    <?= $this->Html->css('home.css') ?>
    <link href="https://fonts.googleapis.com/css?family=Raleway:500i|Roboto:300,400,700|Roboto+Mono" rel="stylesheet">
</head>
<body class="home">

<header class="row">
    <div class="header-image"><?= $this->Html->image('cake.logo.svg') ?></div>
    <div class="header-title">
        <h1>Welcome to CakePHP <?= Configure::version() ?> Red Velvet. Build fast. Grow solid.</h1>
    </div>
</header>

<div class="row">
    <div class="columns large-12">
        <div class="ctp-warning alert text-center">
            <p>Please be aware that this page will not be shown if you turn off debug mode unless you replace src/Template/Pages/home.ctp with your own version.</p>
        </div>
        <div id="url-rewriting-warning" class="alert url-rewriting">
            <ul>
                <li class="bullet problem">
                    URL rewriting is not properly configured on your server.<br />
                    1) <a target="_blank" href="https://book.cakephp.org/3.0/en/installation.html#url-rewriting">Help me configure it</a><br />
                    2) <a target="_blank" href="https://book.cakephp.org/3.0/en/development/configuration.html#general-configuration">I don't / can't use URL rewriting</a>
                </li>
            </ul>
        </div>
        <?php Debugger::checkSecurityKeys(); ?>
    </div>
</div>

<div class="row">
    <div class="columns large-6">
        <h4>Environment</h4>
        <ul>
        <?php if (version_compare(PHP_VERSION, '5.6.0', '>=')) : ?>
            <li class="bullet success">Your version of PHP is 5.6.0 or higher (detected <?= PHP_VERSION ?>).</li>
        <?php else : ?>
            <li class="bullet problem">Your version of PHP is too low. You need PHP 5.6.0 or higher to use CakePHP (detected <?= PHP_VERSION ?>).</li>
        <?php endif; ?>

        <?php if (extension_loaded('mbstring')) : ?>
            <li class="bullet success">Your version of PHP has the mbstring extension loaded.</li>
        <?php else : ?>
            <li class="bullet problem">Your version of PHP does NOT have the mbstring extension loaded.</li>
        <?php endif; ?>

        <?php if (extension_loaded('openssl')) : ?>
            <li class="bullet success">Your version of PHP has the openssl extension loaded.</li>
        <?php elseif (extension_loaded('mcrypt')) : ?>
            <li class="bullet success">Your version of PHP has the mcrypt extension loaded.</li>
        <?php else : ?>
            <li class="bullet problem">Your version of PHP does NOT have the openssl or mcrypt extension loaded.</li>
        <?php endif; ?>

        <?php if (extension_loaded('intl')) : ?>
            <li class="bullet success">Your version of PHP has the intl extension loaded.</li>
        <?php else : ?>
            <li class="bullet problem">Your version of PHP does NOT have the intl extension loaded.</li>
        <?php endif; ?>
        </ul>
    </div>
    <div class="columns large-6">
        <h4>Filesystem</h4>
        <ul>
        <?php if (is_writable(TMP)) : ?>
            <li class="bullet success">Your tmp directory is writable.</li>
        <?php else : ?>
            <li class="bullet problem">Your tmp directory is NOT writable.</li>
        <?php endif; ?>

        <?php if (is_writable(LOGS)) : ?>
            <li class="bullet success">Your logs directory is writable.</li>
        <?php else : ?>
            <li class="bullet problem">Your logs directory is NOT writable.</li>
        <?php endif; ?>

        <?php $settings = Cache::getConfig('_cake_core_'); ?>
        <?php if (!empty($settings)) : ?>
            <li class="bullet success">The <em><?= $settings['className'] ?>Engine</em> is being used for core caching. To change the config edit config/app.php</li>
        <?php else : ?>
            <li class="bullet problem">Your cache is NOT working. Please check the settings in config/app.php</li>
        <?php endif; ?>
        </ul>
    </div>
    <hr />
</div>

<div class="row">
    <div class="columns large-6">
        <h4>Database</h4>
        <?php
        try {
            $connection = ConnectionManager::get('default');
            $connected = $connection->connect();
        } catch (Exception $connectionError) {
            $connected = false;
            $errorMsg = $connectionError->getMessage();
            if (method_exists($connectionError, 'getAttributes')) :
                $attributes = $connectionError->getAttributes();
                if (isset($errorMsg['message'])) :
                    $errorMsg .= '<br />' . $attributes['message'];
                endif;
            endif;
        }
        ?>
        <ul>
        <?php if ($connected) : ?>
            <li class="bullet success">CakePHP is able to connect to the database.</li>
        <?php else : ?>
            <li class="bullet problem">CakePHP is NOT able to connect to the database.<br /><?= $errorMsg ?></li>
        <?php endif; ?>
        </ul>
    </div>
    <div class="columns large-6">
        <h4>DebugKit</h4>
        <ul>
        <?php if (Plugin::isLoaded('DebugKit')) : ?>
            <li class="bullet success">DebugKit is loaded.</li>
        <?php else : ?>
            <li class="bullet problem">DebugKit is NOT loaded. You need to either install pdo_sqlite, or define the "debug_kit" connection name.</li>
        <?php endif; ?>
        </ul>
    </div>
    <hr />
</div>

<div class="row">
    <div class="columns large-6">
        <h3>Editing this Page</h3>
        <ul>
            <li class="bullet cutlery">To change the content of this page, edit: src/Template/Pages/home.ctp.</li>
            <li class="bullet cutlery">You can also add some CSS styles for your pages at: webroot/css/.</li>
        </ul>
    </div>
    <div class="columns large-6">
        <h3>Getting Started</h3>
        <ul>
            <li class="bullet book"><a target="_blank" href="https://book.cakephp.org/3.0/en/">CakePHP 3.0 Docs</a></li>
            <li class="bullet book"><a target="_blank" href="https://book.cakephp.org/3.0/en/tutorials-and-examples/cms/installation.html">The 20 min CMS Tutorial</a></li>
        </ul>
    </div>
</div>

<div class="row">
    <div class="columns large-12 text-center">
        <h3 class="more">More about Cake</h3>
        <p>
            CakePHP is a rapid development framework for PHP which uses commonly known design patterns like Front Controller and MVC.<br />
            Our primary goal is to provide a structured framework that enables PHP users at all levels to rapidly develop robust web applications, without any loss to flexibility.
        </p>
    </div>
    <hr/>
</div>

<div class="row">
    <div class="columns large-4">
        <i class="icon support">P</i>
        <h3>Help and Bug Reports</h3>
        <ul>
            <li class="bullet cutlery">
                <a href="irc://irc.freenode.net/cakephp">irc.freenode.net #cakephp</a>
                <ul><li>Live chat about CakePHP</li></ul>
            </li>
            <li class="bullet cutlery">
                <a href="http://cakesf.herokuapp.com/">Slack</a>
                <ul><li>CakePHP Slack support</li></ul>
            </li>
            <li class="bullet cutlery">
                <a href="https://github.com/cakephp/cakephp/issues">CakePHP Issues</a>
                <ul><li>CakePHP issues and pull requests</li></ul>
            </li>
            <li class="bullet cutlery">
                <a href="http://discourse.cakephp.org/">CakePHP Forum</a>
                <ul><li>CakePHP official discussion forum</li></ul>
            </li>
        </ul>
    </div>
    <div class="columns large-4">
        <i class="icon docs">r</i>
        <h3>Docs and Downloads</h3>
        <ul>
            <li class="bullet cutlery">
                <a href="https://api.cakephp.org/3.0/">CakePHP API</a>
                <ul><li>Quick Reference</li></ul>
            </li>
            <li class="bullet cutlery">
                <a href="https://book.cakephp.org/3.0/en/">CakePHP Documentation</a>
                <ul><li>Your Rapid Development Cookbook</li></ul>
            </li>
            <li class="bullet cutlery">
                <a href="https://bakery.cakephp.org">The Bakery</a>
                <ul><li>Everything CakePHP</li></ul>
            </li>
            <li class="bullet cutlery">
                <a href="https://plugins.cakephp.org">CakePHP plugins repo</a>
                <ul><li>A comprehensive list of all CakePHP plugins created by the community</li></ul>
            </li>
            <li class="bullet cutlery">
                <a href="https://github.com/cakephp/">CakePHP Code</a>
                <ul><li>For the Development of CakePHP Git repository, Downloads</li></ul>
            </li>
            <li class="bullet cutlery">
                <a href="https://github.com/FriendsOfCake/awesome-cakephp">CakePHP Awesome List</a>
                <ul><li>A curated list of amazingly awesome CakePHP plugins, resources and shiny things.</li></ul>
            </li>
            <li class="bullet cutlery">
                <a href="https://www.cakephp.org">CakePHP</a>
                <ul><li>The Rapid Development Framework</li></ul>
            </li>
        </ul>
    </div>
    <div class="columns large-4">
        <i class="icon training">s</i>
        <h3>Training and Certification</h3>
        <ul>
            <li class="bullet cutlery">
                <a href="https://cakefoundation.org/">Cake Software Foundation</a>
                <ul><li>Promoting development related to CakePHP</li></ul>
            </li>
            <li class="bullet cutlery">
                <a href="https://training.cakephp.org/">CakePHP Training</a>
                <ul><li>Learn to use the CakePHP framework</li></ul>
            </li>
            <li class="bullet cutlery">
                <a href="https://certification.cakephp.org/">CakePHP Certification</a>
                <ul><li>Become a certified CakePHP developer</li></ul>
            </li>
        </ul>
    </div>
</div>

</body>
</html>
