<?php
return array(
  new \Pimf\Route('/', array('controller' => 'hello', 'action' => 'index')),
  new \Pimf\Route('/json', array('controller' => 'hello', 'action' => 'json')),
  new \Pimf\Route('/queries', array('controller' => 'hello', 'action' => 'queries')),
  new \Pimf\Route('/db', array('controller' => 'hello', 'action' => 'db')),
  new \Pimf\Route('/fortunes', array('controller' => 'hello', 'action' => 'fortunes')),
  new \Pimf\Route('/updates', array('controller' => 'hello', 'action' => 'updates')),
  new \Pimf\Route('/plaintext', array('controller' => 'hello', 'action' => 'plaintext')),
);