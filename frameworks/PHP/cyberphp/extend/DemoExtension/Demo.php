<?php

namespace DemoExtension;

class Demo {
    public static function run() {
        // This is a demonstration of an extension library loaded without Composer
        // It can be used in the controller like this: \DemoExtension\Demo::run();
        echo 'I am the DemoExtension extension library loaded without Composer';
        return false;
    }
}