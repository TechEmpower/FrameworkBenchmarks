<?php
namespace App\Controller;

use Cake\Controller\Controller;
use Cake\Datasource\ConnectionManager;

/**
 * Benchmark controller
 */
class BenchmarkController extends Controller
{
    public function plaintext()
    {
        return $this->response
            ->withType('text')
            ->withStringBody('Hello, World!');
    }

    public function json()
    {
        return $this->jsonResponse(['message' => 'Hello, World!']);
    }

    public function db()
    {
        $queries = $this->request->getQuery('queries', 1);
        $queries = min(max($queries, 1), 500);

        $worlds = [];
        $repo = $this->getTableLocator()->get('World');

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = $repo->find('randomId')->first();
        }
        if ($queries == 1 && !$this->request->getQuery('queries')) {
            $worlds = $worlds[0];
        }

        return $this->jsonResponse($worlds);
    }

    public function dbRaw()
    {
        $queries = $this->request->getQuery('queries', 1);
        $queries = min(max($queries, 1), 500);

        $worlds = [];
        $conn = ConnectionManager::get('default');
        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = $conn
                ->execute(
                    'SELECT * FROM world WHERE id = :id',
                    ['id' => mt_rand(1, 10000)]
                )
                ->fetch('assoc');
        }

        if ($queries == 1 && !$this->request->getQuery('queries')) {
            $worlds = $worlds[0];
        }

        return $this->jsonResponse($worlds);
    }

    public function update()
    {
        $queries = $this->request->getQuery('queries', 1);
        $queries = min(500, max(1, $queries));

        $worlds = [];
        $repo = $this->getTableLocator()->get('World');
        for ($i = 0; $i < $queries; ++$i) {
            $world = $repo->find('randomId')
                ->enableHydration(false)
                ->first();

            $world->randomNumber = mt_rand(1, 10000);
            $repo->save($world);

            $worlds[] = $world;
        }

        return $this->jsonResponse($worlds);
    }

    public function updateRaw()
    {
        $queries = $this->request->getQuery('queries', 1);
        $queries = min(500, max(1, $queries));

        $worlds = [];
        $conn = ConnectionManager::get('default');
        for ($i = 0; $i < $queries; ++$i) {
            $id = mt_rand(1, 10000);
            $randomNumber = mt_rand(1, 10000);
            $conn->execute(
                'UPDATE world SET randomNumber=:num WHERE id=:id',
                ['num' => $randomNumber, 'id' => $id]
            );

            $worlds[] = ['id' => $id, 'randomNumber' => $randomNumber];
        }

        return $this->jsonResponse($worlds);
    }

    public function fortunes()
    {
        $repo = $this->getTableLocator()->get('Fortune');

        $fortunes = $repo->find()->all();

        $entity = $repo->newEntity();
        $entity->id = 0;
        $entity->message = 'Additional fortune added at request time.';

        $fortunes = $fortunes->appendItem($entity);

        $this->set('fortunes', $fortunes);
    }

    protected function jsonResponse(array $data)
    {
        return $this->response
            ->withType('json')
            ->withStringBody(json_encode($data));
    }
}
