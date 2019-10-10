<?php
// Future app to use Mysql async pool 

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