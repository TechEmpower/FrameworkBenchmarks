<?php

namespace PHPSTORM_META {

    // Reflect
    override(\Psr\Container\ContainerInterface::get(0), map('@'));
    override(\Hyperf\Utils\Context::get(0), map('@'));
    override(\make(0), map('@'));
    override(\di(0), map('@'));

}