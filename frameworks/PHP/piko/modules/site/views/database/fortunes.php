<?php
/**
 * @var \Piko\View $this
 * @var array<\app\modules\site\models\Fortune> $fortunes
 */

 $this->title = 'Fortunes';
?>

<table>
<tr><th>id</th><th>message</th></tr>
<?php foreach($fortunes as $fortune): ?>
<tr><td><?= (int) $fortune->id ?></td><td><?= $this->escape((string) $fortune->message) ?></td></tr>
<?php endforeach ?>
</table>
