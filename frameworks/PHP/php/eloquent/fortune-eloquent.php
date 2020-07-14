<?php
//
// Database Test
//

require_once __DIR__ . '/boot-eloquent.php';

class Fortune extends \Illuminate\Database\Eloquent\Model {
    protected $table = 'fortune';
    public $timestamps = false;
    protected $primaryKey = 'id';
}

// Define query and store result in array.
$rows = Fortune::all();

$insert = new Fortune();
$insert->id = 0;
$insert->message = "Additional fortune added at request time.";

$rows->add($insert);
$rows = $rows->sortBy("message");

?>
<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
<?php foreach ( $rows as $fortune ) : ?>
<tr><td><?= $fortune->id ?></td><td><?= htmlspecialchars($fortune->message, ENT_QUOTES, 'UTF-8') ?></td></tr>
<?php endforeach ?></table></body></html>