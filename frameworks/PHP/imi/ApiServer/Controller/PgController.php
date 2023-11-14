<?php
namespace ImiApp\ApiServer\Controller;

use Imi\Db\Db;
use Imi\Db\Interfaces\IDb;
use Imi\RequestContext;
use Imi\Redis\RedisManager;
use ImiApp\Model\PgSql\World;
use ImiApp\Model\PgSql\Fortune;
use Imi\Util\Stream\MemoryStream;
use Imi\Server\View\Annotation\View;
use Imi\Server\View\Annotation\HtmlView;
use Imi\Server\Http\Route\Annotation\Route;
use Imi\Server\Http\Route\Annotation\Action;
use Imi\Server\Http\Controller\HttpController;
use Imi\Server\Http\Route\Annotation\Controller;
use Imi\Server\Http\Message\Contract\IHttpResponse;

/**
 * @Controller("/")
 */
class PgController extends HttpController
{
    const POOL_NAME = 'pgsql';

    /**
     * @Action
     */
    public function pgDbModel(): ?World
    {
        return World::find(\mt_rand(1, 10000));
    }

    /**
     * @Action
     */
    public function pgDbRaw(): array
    {
        $db = Db::getInstance(self::POOL_NAME);
        $stmt = $db->prepare('SELECT id, randomnumber FROM World WHERE id = ? LIMIT 1');
        $stmt->execute([\mt_rand(1, 10000)]);
        return $stmt->fetch();
    }

    /**
     * @Action
     */
    public function pgQueryModel($queries): array
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
    public function pgQueryRaw($queries): array
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
        $db = Db::getInstance(self::POOL_NAME);
        $stmt = $db->prepare('SELECT id, randomnumber FROM World WHERE id = ? LIMIT 1');
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
     * @HtmlView("fortunes")
     */
    public function pgFortunes(): array
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
    public function pgFortunesRaw(): IHttpResponse
    {
        $rows = [];
        foreach(Db::getInstance(self::POOL_NAME)->query('SELECT id, message FROM Fortune')->fetchAll() as $item)
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
    public function pgUpdateModel($queries): array
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
    public function pgUpdateRaw($queries): array
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
        $db = Db::getInstance(self::POOL_NAME);
        $stmtSelect = $db->prepare('SELECT id, randomnumber FROM World WHERE id = ? LIMIT 1');
        $stmtUpdate = $db->prepare('UPDATE World SET randomNumber = CASE id' . \str_repeat(' WHEN ?::INTEGER THEN ?::INTEGER ', $queryCount) . 'END WHERE id IN (' . \str_repeat('?::INTEGER,', $queryCount - 1) . '?::INTEGER)');
        $list = [];
        $keys = $values = [];
        while ($queryCount--)
        {
            $values[] = $keys[] = $id = \mt_rand(1, 10000);
            $stmtSelect->execute([$id]);
            $row = $stmtSelect->fetch();

            $values[] = $row['randomNumber'] = \mt_rand(1, 10000);
            $list[] = $row;
        }
        $stmtUpdate->execute([
            ...$values,
            ...$keys
        ]);

        return $list;
    }
}