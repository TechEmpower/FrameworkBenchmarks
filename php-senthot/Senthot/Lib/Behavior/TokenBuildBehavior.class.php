<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2010 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

defined('SEN_PATH') or exit();
/**
 * System behavior extension : Form token generation
 * @category	Sen
 * @package		Sen
 * @subpackage  Behavior
 * @author		ms134n <ms134n@gmail.com>
 */
class TokenBuildBehavior extends Behavior {
    // Parameter defines the behavior
    protected $options   =  array(
        'TOKEN_ON'       => false,     // Open Token Authentication
        'TOKEN_NAME'     => '__hash__',    // Token authentication hidden form field names
        'TOKEN_TYPE'     => 'md5',   // Token authentication hash rule
        'TOKEN_RESET'    => true, // Tokens are reset after an error
    );

    public function run(&$content){
        if(C('TOKEN_ON')) {
            if(strpos($content,'{__TOKEN__}')) {
                // Token hidden form fields specified location
                $content = str_replace('{__TOKEN__}',$this->buildToken(),$content);
            }elseif(preg_match('/<\/form(\s*)>/is',$content,$match)) {
                // Smart token generated form hidden fields
                $content = str_replace($match[0],$this->buildToken().$match[0],$content);
            }
        }else{
            $content = str_replace('{__TOKEN__}','',$content);
        }
    }

    // Create a form token
    private function buildToken() {
        $tokenName  = C('TOKEN_NAME');
        $tokenType  = C('TOKEN_TYPE');
        if(!isset($_SESSION[$tokenName])) {
            $_SESSION[$tokenName]  = array();
        }
        // Uniqueness identifies the current page
        $tokenKey   =  md5($_SERVER['REQUEST_URI']);
        if(isset($_SESSION[$tokenName][$tokenKey])) {// Repeat the same page does not generate session
            $tokenValue = $_SESSION[$tokenName][$tokenKey];
        }else{
            $tokenValue = $tokenType(microtime(TRUE));
            $_SESSION[$tokenName][$tokenKey]   =  $tokenValue;
        }
        $token      =  '<input type="hidden" name="'.$tokenName.'" value="'.$tokenKey.'_'.$tokenValue.'" />';
        return $token;
    }
}