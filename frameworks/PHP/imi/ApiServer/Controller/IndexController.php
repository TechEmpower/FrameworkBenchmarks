<?php
namespace ImiApp\ApiServer\Controller;

use Imi\Db\Db;
use Imi\RequestContext;
use ImiApp\Model\World;
use ImiApp\Model\Fortune;
use Imi\Redis\RedisManager;
use Imi\Util\Stream\MemoryStream;
use Imi\Controller\HttpController;
use Imi\Server\View\Annotation\View;
use Imi\Server\Route\Annotation\Route;
use Imi\Server\Route\Annotation\Action;
use Imi\Server\Route\Annotation\Controller;

/**
 * @Controller("/")
 */
class IndexController extends HttpController
{
    /**
     * @Action
     *
     * @return void
     */
    public function json()
    {
        return ['message' => 'Hello, World!'];
    }

    /**
     * @Action
     * @View(renderType="html")
     *
     * @return void
     */
    public function plaintext()
    {
        return RequestContext::get('response')->withHeader('Content-Type', 'text/plain; charset=utf-8')->write('Hello, World!');
    }

    /**
     * @Action
     *
     * @return void
     */
    public function dbModel()
    {
        return World::find(\mt_rand(1, 10000));
    }

    /**
     * @Action
     *
     * @return void
     */
    public function dbQueryBuilder()
    {
        return Db::query()->from('World')->field('id', 'randomNumber')->where('id', '=', \mt_rand(1, 10000))->limit(1)->select()->get();
    }

    /**
     * @Action
     *
     * @return void
     */
    public function dbRaw()
    {
        $db = Db::getInstance();
        $stmt = $db->prepare('SELECT id, randomNumber FROM World WHERE id = ? LIMIT 1');
        $stmt->execute([\mt_rand(1, 10000)]);
        return $stmt->fetch();
    }

    /**
     * @Action
     *
     * @return void
     */
    public function queryModel($queries)
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
    public function queryQueryBuilder($queries)
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
            $list[] = Db::query()->from('World')->field('id', 'randomNumber')->where('id', '=', \mt_rand(1, 10000))->limit(1)->select()->get();
        }
        return $list;
    }

    /**
     * @Action
     *
     * @return void
     */
    public function queryRaw($queries)
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
     *
     * @return void
     */
    public function fortunes()
    {
        RequestContext::use(function(&$context){
            $context['response'] = $context['response']->withHeader('Content-Type', 'text/html; charset=UTF-8');
        });
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
    public function fortunesRaw()
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

        return RequestContext::get('response')->withHeader('Content-Type', 'text/html; charset=UTF-8')
                                              ->withBody(new MemoryStream("<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>{$html}</table></body></html>"));
    }

    /**
     * @Action
     *
     * @return void
     */
    public function updateModel($queries)
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
     *
     * @return void
     */
    public function updateQueryBuilder($queries)
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
            $id = \mt_rand(1, 10000);
            $row = Db::query()->from('World')->field('id', 'randomNumber')->where('id', '=', $id)->limit(1)->select()->get();
            $row['randomNumber'] = $randomNumber = \mt_rand(1, 10000);
            Db::query()->from('World')->where('id', '=', $id)->limit(1)->update([
                'randomNumber'  =>  $randomNumber,
            ]);
            $list[] = $row;
        }
        return $list;
    }

    /**
     * @Action
     *
     * @return void
     */
    public function updateRaw($queries)
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
     *
     * @return void
     */
    public function cachedWorlds($count)
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
        $ids = [];
        while ($queryCount--)
        {
            $ids[] = 'world:' . \mt_rand(1, 10000);
        }
        return RedisManager::getInstance()->mget($ids);
    }

}
