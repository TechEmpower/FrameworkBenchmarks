<?php
namespace ImiApp\ApiServer\Controller;

use Imi\App;
use Imi\Db\Db;
use Imi\RequestContext;
use ImiApp\Model\World;
use ImiApp\Model\Fortune;
use Imi\Db\Interfaces\IDb;
use Imi\Redis\RedisManager;
use Imi\Util\Stream\MemoryStream;
use Imi\Server\View\Annotation\View;
use Imi\Server\Http\Route\Annotation\Route;
use Imi\Server\Http\Route\Annotation\Action;
use Imi\Server\Http\Controller\HttpController;
use Imi\Server\Http\Route\Annotation\Controller;
use Imi\Server\Http\Message\Contract\IHttpResponse;

/**
 * @Controller("/")
 */
class IndexController extends HttpController
{
    /**
     * @Action
     */
    public function json(): array
    {
        return ['message' => 'Hello, World!'];
    }

    /**
     * @Action
     */
    public function plaintext(): IHttpResponse
    {
        $response = $this->response;
        $response->setHeader('Content-Type', 'text/plain; charset=utf-8')
                 ->getBody()
                 ->write('Hello, World!');
        return $response;
    }

    /**
     * @Action
     */
    public function dbModel(): ?World
    {
        return World::find(\mt_rand(1, 10000));
    }

    /**
     * @Action
     */
    public function dbRaw(): array
    {
        $db = Db::getInstance();
        $stmt = $db->prepare('SELECT id, randomNumber FROM World WHERE id = ? LIMIT 1');
        $stmt->execute([\mt_rand(1, 10000)]);
        return $stmt->fetch();
    }

    /**
     * @Action
     */
    public function queryModel($queries): array
    {
        $queries = (int)$queries;
        if($queries > 1)
        {
            $queryCount = \min($queries, 500);
        }
        else
        {
            $queryCount = 1;
        }
        $list = [];
        while ($queryCount--)
        {
            $list[] = World::find(\mt_rand(1, 10000));
        }
        return $list;
    }

    /**
     * @Action
     *
     * @return void
     */
    public function queryRaw($queries): array
    {
        $queries = (int)$queries;
        if($queries > 1)
        {
            $queryCount = \min($queries, 500);
        }
        else
        {
            $queryCount = 1;
        }
        $list = [];
        $db = Db::getInstance();
        $stmt = $db->prepare('SELECT id, randomNumber FROM World WHERE id = ? LIMIT 1');
        while ($queryCount--)
        {
            $stmt->execute([\mt_rand(1, 10000)]);
            $list[] = $stmt->fetch();
        }
        return $list;
    }

    /**
     * @Action
     * @View(renderType="html")
     */
    public function fortunes(): array
    {
        $this->response->setHeader('Content-Type', 'text/html; charset=UTF-8');
        $list = Fortune::select();
        $rows = [];
        foreach($list as $item)
        {
            $rows[$item->id] = $item->message;
        }
        $rows[0] = 'Additional fortune added at request time.';
        \asort($rows);
        return [
            'rows'  =>  $rows,
        ];
    }

    /**
     * @Action
     * @View(renderType="html")
     *
     * @return void
     */
    public function fortunesRaw(): IHttpResponse
    {
        $rows = [];
        foreach(Db::getInstance()->query('SELECT id, message FROM Fortune')->fetchAll() as $item)
        {
            $rows[$item['id']] = $item['message'];
        }
        $rows[0] = 'Additional fortune added at request time.';
        \asort($rows);

        $html = '';
        foreach ($rows as $id => $message)
        {
            $message = \htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
            $html .= "<tr><td>{$id}</td><td>{$message}</td></tr>";
        }

        return $this->response->setHeader('Content-Type', 'text/html; charset=UTF-8')
                              ->setBody(new MemoryStream("<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>{$html}</table></body></html>"));
    }

    /**
     * @Action
     */
    public function updateModel($queries): array
    {
        $queries = (int)$queries;
        if($queries > 1)
        {
            $queryCount = \min($queries, 500);
        }
        else
        {
            $queryCount = 1;
        }
        $list = [];
        while ($queryCount--)
        {
            $list[] = $row = World::find(\mt_rand(1, 10000));
            $row->randomNumber = \mt_rand(1, 10000);
            $row->update();
        }
        return $list;
    }

    /**
     * @Action
     */
    public function updateRaw($queries): array
    {
        $queries = (int)$queries;
        if($queries > 1)
        {
            $queryCount = \min($queries, 500);
        }
        else
        {
            $queryCount = 1;
        }
        $list = [];
        $db = Db::getInstance();
        $stmtSelect = $db->prepare('SELECT id, randomNumber FROM World WHERE id = ? LIMIT 1');
        $stmtUpdate = $db->prepare('UPDATE World SET randomNumber = ? WHERE id = ? LIMIT 1');
        while ($queryCount--)
        {
            $id = \mt_rand(1, 10000);
            $stmtSelect->execute([$id]);
            $row = $stmtSelect->fetch();
            $row['randomNumber'] = $randomNumber = \mt_rand(1, 10000);
            $stmtUpdate->execute([$randomNumber, $id]);
            $list[] = $row;
        }

        return $list;
    }

    /**
     * @Action
     * @Route("cached-worlds")
     */
    public function cachedWorlds($count): array
    {
        $count = (int)$count;
        if($count > 1)
        {
            $queryCount = \min($count, 500);
        }
        else
        {
            $queryCount = 1;
        }

        $list = App::get('worlds');
        $result = [];
        $keys = \array_rand($list, $queryCount);
        foreach ((array) $keys as $key)
        {
            if (!isset($list[$key]))
            {
                break;
            }
            $result[] = $list[$key];
        }

        return $result;
    }

}
