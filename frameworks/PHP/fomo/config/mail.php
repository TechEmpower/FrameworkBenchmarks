<?php

return [
    'host'     => env('MAIL_HOST' , 'smtp.mailtrap.io'),
    'username' => env('MAIL_USERNAME'),
    'password' => env('MAIL_PASSWORD'),
    'port'     => env('MAIL_PORT' , 2525)
];