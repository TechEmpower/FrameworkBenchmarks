<?php

namespace app\controllers;

use app\helpers\Query;
use app\models\Fortune;
use app\models\World;
use Yii;
use yii\web\Controller;
use yii\web\Response;

class SiteController extends Controller
{
    /**
     * Test #1: JSON Serialization
     */
    public function actionJson()
    {
        return $this->asJson(['message' => 'Hello, World!']);
    }

    /**
     * Test #2: Single Database Query
     */
    public function actionDb()
    {
        return $this->asJson(World::findOne(random_int(1, 10000)));
    }

    /**
     * Test #3: Multiple Database Queries
     */
    public function actionQueries($queries)
    {
        $queries = Query::clamp($queries);

        $worlds = [];

        while ($queries--) {
            $world = World::findOne(random_int(1, 10000));
            $worlds[] = $world;
        }

        return $this->asJson($worlds);
    }

    /**
     * Test #4: Fortunes
     */
    public function actionFortunes()
    {
        $fortunes = Fortune::find()->all();

        $runtimeFortune = new Fortune();
        $runtimeFortune->id = 0;
        $runtimeFortune->message = 'Additional fortune added at request time.';

        $fortunes[] = $runtimeFortune;

        usort($fortunes, [Fortune::class, 'cmp']);

        $this->view->title = 'Fortunes';

        return $this->render('fortunes', ['fortunes' => $fortunes]);
    }

    /**
     * Test #5: Database Updates
     */
    public function actionUpdates($queries)
    {
        $queries = Query::clamp($queries);

        $worlds = [];

        while ($queries--) {
            $world = World::findOne(random_int(1, 10000));
            $world->randomNumber = random_int(1, 10000);
            $world->save(false);

            $worlds[] = $world;
        }

        return $this->asJson($worlds);
    }

    /**
     * Test #6: Plaintext
     */
    public function actionPlaintext()
    {
        Yii::$app->response->format = Response::FORMAT_RAW;
        Yii::$app->response->getHeaders()->add('Content-Type', 'text/plain');
        Yii::$app->response->content = 'Hello, World!';

        return Yii::$app->response;
    }
}
