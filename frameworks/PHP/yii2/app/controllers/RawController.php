<?php

namespace app\controllers;

use app\helpers\Query;
use Yii;
use yii\web\Controller;

class RawController extends Controller
{
    /**
     * Test #2: Single Database Query
     */
    public function actionDb()
    {
        $statement = Yii::$app->db->createCommand('SELECT id, randomNumber FROM World WHERE id = :id');
        $world = $statement->bindValue(':id', mt_rand(1, 10000))->queryOne();
        $world['id'] = (int)$world['id'];
        $world['randomNumber'] = (int)$world['randomNumber'];

        return $this->asJson($world);
    }

    /**
     * Test #3: Multiple Database Queries
     */
    public function actionQueries($queries)
    {
        $queries = Query::clamp($queries);

        $statement = Yii::$app->db->createCommand('SELECT id, randomNumber FROM World WHERE id = :id');

        $worlds = [];
        while ($queries--) {
            $result = $statement->bindValue(':id', mt_rand(1, 10000))->queryOne();
            $result['id'] = (int)$result['id'];
            $result['randomNumber'] = (int)$result['randomNumber'];
            $worlds[] = $result;
        }

        return $this->asJson($worlds);
    }

    /**
     * Test #4: Fortunes
     */
    public function actionFortunes()
    {
        $fortunes = Yii::$app->db->createCommand('SELECT id, message FROM Fortune')->queryAll(\PDO::FETCH_KEY_PAIR );
        $fortunes[0] = 'Additional fortune added at request time.';

        asort($fortunes);

        $this->view->title = 'Fortunes';

        return $this->render('fortunes', ['fortunes' => $fortunes]);
    }

    /**
     * Test #5: Database Updates
     */
    public function actionUpdates($queries)
    {
        $queries = Query::clamp($queries);

        $selectCommand = Yii::$app->db->createCommand('SELECT id,randomNumber FROM World WHERE id = :id');
        $updateCommand = Yii::$app->db->createCommand('UPDATE World SET randomNumber = :num WHERE id = :id');

        $worlds = [];

        while ($queries--) {
            $id = mt_rand(1, 10000);
            $randomNumber = mt_rand(1, 1000);
            $selectCommand->bindParam(':id', $id)->queryScalar();
            $updateCommand->bindValues([':id' => $id, ':num' => $randomNumber])->execute();

            $worlds[] = ['id' => $id, 'randomNumber' => $randomNumber];
        }

        return $this->asJson($worlds);
    }
}
