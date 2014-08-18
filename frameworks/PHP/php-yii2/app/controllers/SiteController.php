<?php

namespace app\controllers;

use Yii;
use yii\helpers\Html;
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
            if ($queries > 500) $queries = 500;
            elseif ($queries <= 0 ) $queries = 1;
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

    private static function cmp($a, $b) {

	return ($a["message"] < $b["message"]) ? -1 : 1;

    }

    public function actionFortunes() {
        // Test Data
//        $arr = [
//            11=>'<script>alert("This should not be displayed in a browser alert box");</script>',
//            4=>'A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1',
//            5=>'A computer program does what you tell it to do, not what you want it to do.',
//            2=>'A computer scientist is someone who fixes things that aren\'t broken.',
//            8=>'A list is only as strong as its weakest link. — Donald Knuth',
//            //0=>'Additional fortune added at request time.',
//            //0=>'Additional fortune added at request time.',
//            3=>'After enough decimal places, nobody gives a damn.',
//            7=>'Any program that runs right is obsolete.',
//            10=>'Computers make very fast, very accurate mistakes.',
//            6=>'Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen',
//            9=>'Feature: A bug with seniority.',
//            1=>'fortune: No such file or directory',
//            12=>'フレームワークのベンチマーク'
//        ];
//        foreach($arr as $k=>$v) {
//            Yii::$app->db->createCommand('insert into Fortune (id,message) values (:id,:message)',[':id'=>$k,':message'=>$v])->execute();
//        }


        $arr = Yii::$app->db->createCommand('select id, message from Fortune')->queryAll();
        $arr[] = ['id'=>0,'message'=>'Additional fortune added at request time.'];

	usort($arr, array($this, 'cmp'));	       

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
        foreach ( $arr as $val ) {
            echo '<tr>';
            echo '<td>'.$val['id'].'</td>';
            echo '<td>'.Html::encode($val['message']).'</td>';
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
        elseif ($queries <= 0 ) $queries = 1;
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
