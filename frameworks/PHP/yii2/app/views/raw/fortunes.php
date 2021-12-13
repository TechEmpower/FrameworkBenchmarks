<?php
/** @var $fortunes */

use yii\helpers\Html;

?><table><tr><th>id</th><th>message</th></tr><?php foreach ($fortunes as $id => $fortune): ?><tr><td><?=$id?></td><td><?=Html::encode($fortune)?></td></tr><?php endforeach?></table>
