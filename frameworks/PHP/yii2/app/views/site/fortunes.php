<?php
/** @var $fortunes */

use yii\helpers\Html;

?><table><tr><th>id</th><th>message</th></tr><?php foreach ($fortunes as $fortune): ?><tr><td><?=$fortune->id?></td><td><?=Html::encode($fortune->message)?></td></tr><?php endforeach?></table>
