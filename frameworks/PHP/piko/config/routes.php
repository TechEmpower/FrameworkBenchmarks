<?php
/**
 *  Routes definitions is a key-value paired array where
 *  keys are request uris and values are internal routes following this format:
 *
 *  '{moduleId}/{controllerId}/{actionId}'
 *  '{moduleId}/{subModuleId}/.../{controllerId}/{actionId}'
 */

return [
    '/plaintext' => 'site/default/plaintext',
    '/json' => 'site/default/json',
    '/db' => 'site/database/query',
    '/queries' => 'site/database/queries',
    '/fortunes' => 'site/database/fortunes',
    '/updates' => 'site/database/updates',
];
