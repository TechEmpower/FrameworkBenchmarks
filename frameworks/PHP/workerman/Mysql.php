<?php

class Mysql
{

    protected PDO $pdo;
    protected PDOStatement $world;
    protected PDOStatement $fortune;
    protected PDOStatement $update;

    public function __construct()
    {
        $this->pdo = new PDO(
            'mysql:host=tfb-database;dbname=hello_world',
            'benchmarkdbuser',
            'benchmarkdbpass',
            [
                PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC,
                PDO::ATTR_EMULATE_PREPARES => false
            ]
        );
        $this->world = $this->pdo->prepare('SELECT id,randomNumber FROM World WHERE id=?');
        $this->fortune = $this->pdo->prepare('SELECT id,message FROM Fortune');
        $this->update = $this->pdo->prepare('UPDATE World SET randomNumber=? WHERE id=?');
    }

    function db(): array
    {
        $this->world->execute([mt_rand(1, 10000)]);
        return $this->world->fetch();
    }

    function query($request): array
    {
        $count = min(max((int) $request->get('q'), 1), 500);
        $arr = [];
        while ($count--) {
            $this->world->execute([mt_rand(1, 10000)]);
            $arr[] = $this->world->fetch();
        }
        return $arr;
    }

    function update($request): array
    {
        $count = min(max((int) $request->get('q'), 1), 500);
        $arr = [];
        while ($count--) {
            $id = mt_rand(1, 10000);
            $this->world->execute([$id]);
            $item = $this->world->fetch();
            $this->update->execute(
                [$item['randomNumber'] = mt_rand(1, 10000), $id]
            );
            $arr[] = $item;
        }
        return $arr;
    }

    function fortune(): string
    {
        $this->fortune->execute();
        $arr = $this->fortune->fetchAll(PDO::FETCH_KEY_PAIR);
        $arr[0] = 'Additional fortune added at request time.';
        asort($arr);
        $html = '';
        foreach ($arr as $id => $message) {
            $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
            $html .= "<tr><td>$id</td><td>$message</td></tr>";
        }
        return "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>$html</table></body></html>";
    }

}