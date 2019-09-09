<?php

use App\services\TestService;

return [
    'single' => [
        ['GET', '/json', [TestService::class, 'json']],
        ['GET', '/db', [TestService::class, 'db']],
        ['GET', '/queries', [TestService::class, 'queries']],
        ['GET', '/fortunes', [TestService::class, 'fortunes']],
        ['GET', '/updates', [TestService::class, 'updates']],
        ['GET', '/plaintext', [TestService::class, 'plaintext']],
    ],
    'group' => [],
];
