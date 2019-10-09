<?php
namespace ImiApp\ApiServer\Controller;

use ImiApp\Model\World;
use ImiApp\Model\Fortune;
use Imi\Controller\HttpController;
use Imi\Db\Annotation\Transaction;
use Imi\Server\View\Annotation\View;
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
        return $this->response->withHeader('Content-Type', 'text/plain')->write('Hello, World!');
    }

    /**
     * @Action
     *
     * @return void
     */
    public function db()
    {
        return World::find(mt_rand(1, 10000));
    }

    /**
     * @Action
     *
     * @return void
     */
    public function query($queries)
    {
        $queryCount = 1;
        if($queries > 1)
        {
            $queryCount = min($queries, 500);
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
     * @View(renderType="html")
     *
     * @return void
     */
    public function fortunes()
    {
        $this->response = $this->response->withHeader('Content-Type', 'text/html; charset=UTF-8');
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
     * @Transaction
     *
     * @return void
     */
    public function update($queries)
    {
        $queryCount = 1;
        if($queries > 1)
        {
            $queryCount = min($queries, 500);
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

}
