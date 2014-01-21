<?php

namespace app\controllers;

use Yii;
use yii\web\Controller;

class SiteController extends Controller
{
    private function resJson($data) {
        header('Content-type: application/json');
        echo  json_encode($data);
    }

    public function actionJson() {
        return $this->resJson(array('message'=>'Hello, World!'));
    }

    public function actionDb($queries = 1) {
        // Set up for Test
//        $cmd = Yii::$app->db->createCommand('insert into World (randomNumber) values (:v)');
//        for($i = 1; $i <=10000 ; $i ++ ) {
//            $cmd->bindValue(':v',mt_rand(1, 10000))->execute();
//        }

        $statement =  Yii::$app->db->createCommand('select id,randomNumber from World where id = :id');

        if ($queries == 1) {
            $arr = $statement->bindValue(':id',mt_rand(1, 10000))->queryOne();
        } else {
            // Create an array with the response string.
            $arr = array();
            // For each query, store the result set values in the response array
            while (0 < $queries--) {
                // Store result in array.
                $arr[] = $statement->bindValue(':id',mt_rand(1, 10000))->queryOne();
            }
        }

        return $this->resJson($arr);
    }

    public function actionFortunes() {
        $arr = Yii::$app->db->createCommand('select id, message from Fortune')->queryAll();
        $arr[0] = 'Additional fortune added at request time.';
        asort($arr);
        header("Content-Type: text/html; charset=utf-8");
        echo <<<EOM
            <!DOCTYPE html>
            <html>
            <head>
            <title>Fortunes</title>
            </head>
            <body>
            <table>
            <tr>
            <th>id</th>
            <th>message</th>
            </tr>
EOM;
        foreach ( $arr as $id => $fortune ) {
            echo '<tr>';
            echo '<td>'.$id.'</td>';
            echo '<td>'.htmlspecialchars($fortune, ENT_QUOTES, 'utf-8').'</td>';
            echo '</tr>';
        }
        echo <<<EOM
        </table>
        </body>
        </html>
EOM;

    }

    public function actionUpdates($queries = 1) {
        if ($queries > 500) $queries = 500;
        elseif ($queries < 0 ) $queries = 1;
        $selectCommand = Yii::$app->db->createCommand('select randomNumber from World where id = :id');
        $updateCommand = Yii::$app->db->createCommand('update World set randomNumber = :num where id = :id');
        $arr = [];
        while (0 < $queries--) {
            // Store result in array.
            $id = mt_rand(1,10000);
            $randomNumber = mt_rand(1, 1000);
            $selectCommand->bindParam(':id',$id)->queryScalar();
            $updateCommand->bindValues([':id'=>$id,':num'=>$randomNumber])->execute();
            $arr[] = array('id' => $id, 'randomNumber' => $randomNumber);
        }

        return $this->resJson($arr);
    }

    public function actionPlaintext() {
        header("Content-Type: text/plain;");
        echo 'Hello, World!';
    }
}
