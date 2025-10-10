<?php
/**
 * @var Piko\View $this
 * @var string $content
 */
?>
<!DOCTYPE html>
  <html>
  <head>
  <meta charset="utf-8">
  <title><?= $this->escape($this->title) ?></title>
  <?= $this->head() ?>
</head>
<body>
  <?= $content ?>
</body>
</html>
