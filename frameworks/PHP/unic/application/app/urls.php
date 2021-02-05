<?php
//Include views
require_once 'view.php';

//URLs routing
$urlpatterns = [
  '/plaintext' => 'view.plaintext',
  '/json' => 'view.json',
  '/db' => 'view.db',
  '/queries/{queries}' => 'view.queries',
  '/updates/{queries}' => 'view.updates',
  '/fortunes' => 'view.fortunes',
];
