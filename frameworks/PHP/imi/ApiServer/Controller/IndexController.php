<?php
namespace ImiApp\ApiServer\Controller;

use ImiApp\Model\World;
use ImiApp\Model\Fortune;
use Imi\Controller\HttpController;
use Imi\Db\Db;
use Imi\RequestContext;
use Imi\Server\View\Annotation\View;
use Imi\Server\Route\Annotation\Action;
use Imi\Server\Route\Annotation\Controller;
use Imi\Util\Stream\MemoryStream;

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
        return World::find(mt_rand(1, 10000));
    }

    /**
     * @Action
     *
     * @return void
     */
    public function dbQueryBuilder()
    {
        return Db::query()->from('World')->field('id', 'randomNumber')->where('id', '=', mt_rand(1, 10000))->select()->get();
    }

    /**
     * @Action
     *
     * @return void
     */
    public function dbRaw()
    {
        $db = Db::getInstance();
        $stmt = $db->prepare('SELECT id, randomNumber FROM World WHERE id = ?');
        $stmt->execute([mt_rand(1, 10000)]);
        return $stmt->fetch();
    }

    /**
     * @Action
     *
     * @return void
     */
    public function queryModel($queries)
    {
        if($queries > 1)
        {
            $queryCount = min($queries, 500);
        }
        else
        {
            $queryCount = 1;
        }
        $list = [];
        while ($queryCount--)
        {
            $list[] = World::find(mt_rand(1, 10000));
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
        if($queries > 1)
        {
            $queryCount = min($queries, 500);
        }
        else
        {
            $queryCount = 1;
        }
        $list = [];
        while ($queryCount--)
        {
            $list[] = Db::query()->from('World')->field('id', 'randomNumber')->where('id', '=', mt_rand(1, 10000))->select()->get();
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
        if($queries > 1)
        {
            $queryCount = min($queries, 500);
        }
        else
        {
            $queryCount = 1;
        }
        $list = [];
        $db = Db::getInstance();
        $stmt = $db->prepare('SELECT id, randomNumber FROM World WHERE id = ?');
        while ($queryCount--)
        {
            $stmt->execute([mt_rand(1, 10000)]);
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
        asort($rows);
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
        asort($rows);

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
        if($queries > 1)
        {
            $queryCount = min($queries, 500);
        }
        else
        {
            $queryCount = 1;
        }
        $list = [];
        while ($queryCount--)
        {
            $list[] = $row = World::find(mt_rand(1, 10000));
            $row->randomNumber = mt_rand(1, 10000);
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
        if($queries > 1)
        {
            $queryCount = min($queries, 500);
        }
        else
        {
            $queryCount = 1;
        }
        $list = [];
        while ($queryCount--)
        {
            $id = mt_rand(1, 10000);
            $row = Db::query()->from('World')->field('id', 'randomNumber')->where('id', '=', $id)->select()->get();
            $row['randomNumber'] = mt_rand(1, 10000);
            Db::query()->from('World')->where('id', '=', $row['id'])->update([
                'randomNumber'  =>  $row['randomNumber'],
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
        if($queries > 1)
        {
            $queryCount = min($queries, 500);
        }
        else
        {
            $queryCount = 1;
        }
        $list = [];
        $db = Db::getInstance();
        $stmtSelect = $db->prepare('SELECT id, randomNumber FROM World WHERE id = ?');
        $stmtUpdate = $db->prepare('UPDATE World SET randomNumber = :randomNumber WHERE id = :id');
        while ($queryCount--)
        {
            $id = mt_rand(1, 10000);
            $stmtSelect->execute([$id]);
            $row = $stmtSelect->fetch();
            $row['randomNumber'] = mt_rand(1, 10000);
            $stmtUpdate->execute([
                'id'            =>  $row['id'],
                'randomNumber'  =>  $row['randomNumber'],
            ]);
            $list[] = $row;
        }
        return $list;
    }

}
