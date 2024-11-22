<?php

class Pgsql extends Mysql
{
    protected PDO $pdo;
    protected PDOStatement $world;
    protected PDOStatement $fortune;
    protected PDOStatement $update;
    protected PDOStatement $random;
    protected array $updates = [];

    public function __construct()
    {
        $this->pdo = new PDO(
            'pgsql:host=tfb-database;dbname=hello_world',
            'benchmarkdbuser',
            'benchmarkdbpass',
            [
                PDO::ATTR_DEFAULT_FETCH_MODE  => PDO::FETCH_ASSOC,
                PDO::ATTR_ERRMODE             => PDO::ERRMODE_EXCEPTION,
                PDO::ATTR_EMULATE_PREPARES    => false
            ]
        );
        $this->world = $this->random = $this->pdo->prepare('SELECT id,randomNumber FROM World WHERE id=?');
        $this->fortune = $this->pdo->prepare('SELECT id,message FROM Fortune');
        $this->update = $this->pdo->prepare('UPDATE World SET randomNumber=? WHERE id=?');
    }

    function update($request): array
    {
        $query_count = 1;
        $q = (int)$request->get('q');
        if ($q > 1) {
            $query_count = min($q, 500);
        }
        $worlds = [];
        while ($query_count--) {
            $this->random->execute([\mt_rand(1, 10000)]);
            $world = $this->random->fetch();
            $world['randomNumber'] = \mt_rand(1, 10000);
            $worlds[] = $world;
        }
        $rows = count($worlds);

        if (!isset($this->updates[$rows])) {
            $sql = 'UPDATE world SET randomNumber = CASE id'
                . str_repeat(' WHEN ?::INTEGER THEN ?::INTEGER ', $rows)
                . 'END WHERE id IN ('
                . str_repeat('?::INTEGER,', $rows - 1) . '?::INTEGER)';

            $this->updates[$rows] = $this->pdo->prepare($sql);
        }

        $val = [];
        $keys = [];
        foreach ($worlds as $world) {
            $val[] = $keys[] = $world['id'];
            $val[] = $world['randomNumber'];
        }

        $this->updates[$rows]->execute([...$val, ...$keys]);
        return $worlds;
    }

}