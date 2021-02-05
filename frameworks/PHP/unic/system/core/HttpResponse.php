<?php
/**
* HTTP Response
* Response middleware is system default middleware, this middleware handle application response.
*
* @package : HttpResponse
* @category : System Middleware
* @author : Unic Framework
* @link : https://github.com/unicframework/unic
*/

class HttpResponse {
  /**
  * Store templates array
  *
  * @var array
  */
  private $templates;

  /**
  * Response Header
  * Set HTTP Response header.
  *
  * @param string $header
  * @param boolean $replace
  * @param integer $http_response_code
  * @return void
  */
  protected function header(string $header, bool $replace=TRUE, int $http_response_code=NULL) {
    //Set HTTP header
    if(isset($http_response_code)) {
      header($header, $replace, $http_response_code);
    } else {
      header($header, $replace);
    }
  }

  /**
  * Response Code
  * Set HTTP response status code.
  *
  * @param integer $http_response_code
  * @return void
  */
  protected function response_code(int $http_response_code) {
    //Set HTTP Response code
    http_response_code($http_response_code);
  }

  /**
  * Redirect URLs
  * Redirect users to another page.
  *
  * @param string $url
  * @param string $method
  * @param integer $http_response_code
  * @return void
  */
  protected function redirect(string $url, string $method=NULL, int $http_response_code=NULL) {
    //Set http response code
    if(isset($http_response_code)) {
      http_response_code($http_response_code);
    }
    //IIS environment use 'refresh' for better compatibility
    if(isset($method) && isset($_SERVER['SERVER_SOFTWARE']) && strpos($_SERVER['SERVER_SOFTWARE'], 'Microsoft-IIS') !== FALSE) {
      $method = 'refresh';
    }
    //Redirect URLs
    if(isset($method)) {
      if(strtolower($method) === 'refresh') {
        header("Refresh: 0; url=$url");
      }
    } else {
      header("Location: $url");
    }
  }

  /**
  * Response
  * Render string data with HTTP Response status code.
  *
  * @param mixed $string
  * @param integer $http_response_code
  * @return void
  */
  protected function response($string, int $http_response_code=NULL) {
    if(isset($http_response_code)) {
      //Set http response code
      http_response_code($http_response_code);
    }
    //Render string data.
    //Check string data type.
    if(is_array($string)) {
      print_r($string);
    } else {
      echo $string;
    }
  }
  
  /**
  * Json Data
  * Check Json format is valid or not.
  *
  * @param mixed $data
  * @return boolean
  */
  protected function is_json($data) {
    return is_array($data) ? false : is_array(json_decode($data, true));
  }
  
  /**
  * JSON Response
  * Render json data with HTTP Response status code.
  *
  * @param mixed $data
  * @param integer $http_response_code
  * @return void
  */
  protected function response_json($data, int $http_response_code=NULL) {
    //Set header content type for json response.
    header('Content-type: application/json');
    if(isset($http_response_code)) {
      //Set http response code
      http_response_code($http_response_code);
    }
    //Render json data.
    //Check string data type.
    if(is_array($data)) {
      echo json_encode($data);
    } else if($this->is_json($data)) {
      echo $data;
    } else {
      echo json_encode(array($data));
    }
  }

  /**
  * Render Templates
  * Render the HTML templates.
  *
  * @param string $template
  * @param array $user_variable
  * @return void
  */
  protected function render(string $template, array $user_variable=NULL) {
    //Get template full path
    $template_file = $this->get_template($template);
    //Render html templates.
    if(isset($template_file)) {
      //Set variables of array.
      if(isset($user_variable) && is_array($user_variable)) {
        foreach($user_variable as $variable => $value) {
          ${$variable} = $value;
        }
      }
      require_once($template_file);
    } else {
      http_response_code(500);
      exit("Error : '$template' template not found");
    }
  }
  
  /**
  * Render File
  * Render files with HTTP Response status code.
  *
  * @param string $file_path
  * @param string $mime_type
  * @param integer $http_response_code
  * @return void
  */
  protected function render_file(string $file_path, string $mime_type=NULL, int $http_response_code=NULL) {
    //Render files.
    if(is_file($file_path)) {
      //Set header content type.
      if(isset($mime_type)) {
        header('Content-type: '.$mime_type);
      } else {
        header('Content-type: '.mime_content_type($file_path));
      }
      header('Content-Length: '.$this->get_filesize($file_path));
      if(isset($http_response_code)) {
        //Set http response code
        http_response_code($http_response_code);
      }
      return readfile($file_path);
    } else {
      http_response_code(500);
      exit("Error : '$file_path' file not found");
    }
  }

  /**
  * Send File
  * Send files to the client.
  *
  * @param string $file_path
  * @param integer $http_response_code
  * @return void
  */
  protected function send_file(string $file_path, int $http_response_code=NULL) {
    if(is_file($file_path)) {
      if(isset($http_response_code)) {
        //Set http response code
        http_response_code($http_response_code);
      }
      header('Content-Description: File Transfer');
      header('Content-Type: application/octet-stream');
      header('Content-Disposition: attachment; filename="'.basename($file_path).'"');
      header('Expires: 0');
      header('Cache-Control: must-revalidate');
      header('Pragma: public');
      header('Content-Length: '.$this->get_filesize($file_path));
      flush(); //Flush system output buffer
      return readfile($file_path);
    } else {
      http_response_code(500);
      exit("Error : '$file_path' file not found");
    }
  }

  /**
  * Get Template
  * get html template full path.
  *
  * @param string $template
  * @return string|void
  */
  private function get_template(string $template) {
    global $templates;

    //Get application template directory.
    if(isset($templates) && !empty($templates)) {
      if(!is_array($templates)) {
        $templates = array($templates);
      }
    } else {
      $templates = array();
    }

    //Check template exists or not.
    foreach($templates as $template_dir) {
      //Get template directory path
      $template_path = BASEPATH.'/application/'.trim($template_dir, '/');
      //Get template path.
      if(file_exists($template_path.'/'.$template)) {
        return $template_path.'/'.$template;
      } else if(file_exists($template_path.'/'.$template.'.php')) {
        return $template_path.'/'.$template.'.php';
      } else if(file_exists($template_path.'/'.$template.'.html')) {
        return $template_path.'/'.$template.'.html';
      }
    }
  }

  /**
  * Get File Size
  * get file size of any file. it support larger then 4 GB file size.
  *
  * @param string $file_path
  * @return integer
  */
  private function get_filesize(string $file_path) {
    $size = filesize($file_path);
    if ($size < 0) {
      if (!(strtoupper(substr(PHP_OS, 0, 3))=='WIN')) {
        $size = trim(`stat -c%s $file_path`);
      } else {
        $fsobj = new COM("Scripting.FileSystemObject");
        $f = $fsobj->GetFile($file_path);
        $size = $f->Size;
      }
    }
    return $size;
  }
}
