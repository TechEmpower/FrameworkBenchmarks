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
$arr = Fortune::all()->pluck('message', 'id')->all();

$arr[0] = 'Additional fortune added at request time.';

asort($arr);
?>
<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
<?php foreach ( $arr as $id => $fortune ) : ?>
<tr><td><?= $id ?></td><td><?= htmlspecialchars($fortune, ENT_QUOTES, 'UTF-8') ?></td></tr>
<?php endforeach ?></table></body></html>