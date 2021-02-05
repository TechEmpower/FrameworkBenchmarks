<?php
//Include views
require_once 'view.php';

//URLs routing
$urlpatterns = [
  '/plaintext' => 'view.plaintext',
  '/json' => 'view.json',
  '/db' => 'view.db',
  '/queries' => 'view.queries',
  '/updates' => 'view.updates',
  '/fortunes' => 'view.fortunes',
];
