<?php
namespace app\modules\site\controllers;

use Psr\Http\Message\ResponseInterface;
use PDO;
use app\modules\site\models\World;
use app\modules\site\models\Fortune;

class DatabaseController extends \Piko\Controller
{
    public function __construct(private PDO $db)
    {
    }

    /**
     * Returns as Json response a random record form the World table
     *
     * @return ResponseInterface
     */
    public function queryAction(): ResponseInterface
    {
        $world = new World($this->db);
        $id = mt_rand(1, 10000);
        $world->load($id);

        return $this->jsonResponse($world->toArray());
    }

    /**
     * Return as Json response a set of random records form the World table
     *
     * @param string $queries The number of queries requested to fetch records
     *
     * @return ResponseInterface
     */
    public function queriesAction(string $queries = ''): ResponseInterface
    {
        // Bound and sanitize $queries parameter
        $queries = is_numeric($queries) ? (int) $queries : 1;

        if ($queries < 1) {
            $queries = 1;
        } elseif ($queries > 500) {
            $queries = 500;
        }

        $rows = [];
        $world = new World($this->db);

        while ($queries) {
            $id = mt_rand(1, 10000);
            $rows[] = $world->load($id)->toArray();
            $queries--;
        }

        return $this->jsonResponse($rows);
    }

    /**
     * Retrieves, sorts, and renders the list of fortunes.
     *
     * Fetches all fortunes from the database, adds an additional fortune at request time,
     * sorts the list of fortunes by message text, and renders the result using the "fortunes" view.
     *
     * @return string The rendered Fortune page
     */
    public function fortunesAction(): string
    {
        $st = $this->db->prepare('SELECT * FROM fortune');
        $st->execute();
        $rows = $st->fetchAll(PDO::FETCH_CLASS, Fortune::class, [$this->db]);

        // Add a new record without saving it in the db
        $fortune = new Fortune($this->db);
        $fortune->message = 'Additional fortune added at request time.';
        $rows[] = $fortune;

        // Sort messages
        usort($rows, function($a, $b) {
            return strcmp($a->message, $b->message);
        });

        return $this->render('fortunes', [
            'fortunes' => $rows
        ]);
    }

    /**
     * Updates a set of random records in the World table and returns them as a Json response.
     *
     * Fetches the specified number of World records,
     * assigns each a new randomNumber, saves, and returns the modified records.
     *
     * @param string $queries The number of records to update and return
     *
     * @return ResponseInterface
     */
    public function updatesAction(string $queries = ''): ResponseInterface
    {
        // Bound and sanitize $queries parameter
        $queries = is_numeric($queries) ? (int) $queries : 1;

        if ($queries < 1) {
            $queries = 1;
        } elseif ($queries > 500) {
            $queries = 500;
        }

        $rows = [];
        $world = new World($this->db);

        while ($queries) {
            $id = mt_rand(1, 10000);
            $world = $world->load($id);
            $world->randomnumber = mt_rand(1, 10000);
            $world->save();
            $rows[] = $world->toArray();
            $queries--;
        }

        return $this->jsonResponse($rows);
    }
}
