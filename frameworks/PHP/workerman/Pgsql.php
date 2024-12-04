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
        $queries = $request->get('q');
        $worlds = $keys = $values = [];
        $count = min(max((int) $queries, 1), 500);
        for ($i = 0; $i < $count; ++ $i) {
            $values[] = $keys[] = $id = mt_rand(1, 10000);
            $this->random->execute([$id]);
            $row = $this->random->fetch();
            $values[] = $row['randomNumber'] = mt_rand(1, 10000);
            $worlds[] = $row;
        }
        if (!isset($this->updates[$count])) {
            $sql = 'UPDATE World SET randomNumber = CASE id' . str_repeat(' WHEN ?::INTEGER THEN ?::INTEGER ', $count) . 'END WHERE id IN (' . str_repeat('?::INTEGER,', $count - 1) . '?::INTEGER)';
            $this->updates[$count] = $this->pdo->prepare($sql);
        }
        $this->updates[$count]->execute([...$values, ...$keys]);
        return $worlds;
    }

}