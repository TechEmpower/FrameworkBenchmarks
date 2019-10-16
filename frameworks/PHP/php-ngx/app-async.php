<?php
// Future app to use Mysql async pool 

require_once "/ngx_php7/t/lib/mysql.php";
define("DB_HOST", gethostbyname("tfb-database"));
define("DB_PORT", "3306");
define("DB_USER", "benchmarkdbuser");
define("DB_PASS", "benchmarkdbpass");
define("DB_NAME", "hello_world");

function fortune()
{
    $my = new php\ngx\mysql();
    yield from $my->connect(DB_HOST, DB_PORT, DB_USER, DB_PASS, DB_NAME);
    $ret = yield from $my->query("SELECT id, message FROM Fortune");
    
    $arr = [];
    foreach ($ret as $row) {
            $arr[$row["id"]] = $row["message"];
    }
    $arr[0] = "Additional fortune added at request time.";
    asort($arr);
    
    $html = "";
    foreach ($arr as $id => $message) {
        $message = htmlspecialchars($message, ENT_QUOTES, "UTF-8");
        $html .= "<tr><td>$id</td><td>$message</td></tr>";
    }

    echo    "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>",
            $html,
            "</table></body></html>";

    yield from $my->close();
}