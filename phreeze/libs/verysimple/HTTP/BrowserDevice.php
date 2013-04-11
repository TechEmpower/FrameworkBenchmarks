<?php
/** @package    verysimple::HTTP */

/**
 * BrowserDevice represents a device used when browsing (or executing)
 * the current code.  This is a Singleton pattern, cannot be 
 * instantiated directly.
 * 
 * TODO: this only has minimal support and basically only supplies
 * information about whether the device is mobile or not, and if so
 * it supplies the major vendor name.
 * 
 * Usage:
 * $browser = BrowserDevice::GetInstance();
 * if ($device->IsMobile()) dosomething();
 *
 * @package    verysimple::HTTP 
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc. http://www.verysimple.com
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
class BrowserDevice
{

	/**
	 * patters to search for devices
	 * @var Array
	 */
	static $DESKTOP_DEVICE_PATTERNS = Array(
		'AdobeAIR'=>'AdobeAIR',
		'chrome'=>'google',
		'firefox'=>'mozilla',
		'MSIE'=>'microsoft',
		'safari'=>'apple'
	);
	
	/**
	 * patters to search for devices
	 * @var Array
	 */
	static $MOBILE_DEVICE_PATTERNS = Array(
		'(ipad|ipod|iphone)'=>'apple',
		'android'=>'android',
		'opera mini'=>'opera',
		'blackberry'=>'blackberry',
		'(pre\/|palm os|palm|hiptop|avantgo|plucker|xiino|blazer|elaine)'=>'palm',
		'(iris|3g_t|windows ce|opera mobi|windows ce; smartphone;|windows ce; iemobile)'=>'windows',
		'(mini 9.5|vx1000|lge |m800|e860|u940|ux840|compal|wireless| mobi|ahong|lg380|lgku|lgu900|lg210|lg47|lg920|lg840|lg370|sam-r|mg50|s55|g83|t66|vx400|mk99|d615|d763|el370|sl900|mp500|samu3|samu4|vx10|xda_|samu5|samu6|samu7|samu9|a615|b832|m881|s920|n210|s700|c-810|_h797|mob-x|sk16d|848b|mowser|s580|r800|471x|v120|rim8|c500foma:|160x|x160|480x|x640|t503|w839|i250|sprint|w398samr810|m5252|c7100|mt126|x225|s5330|s820|htil-g1|fly v71|s302|-x113|novarra|k610i|-three|8325rc|8352rc|sanyo|vx54|c888|nx250|n120|mtk |c5588|s710|t880|c5005|i;458x|p404i|s210|c5100|teleca|s940|c500|s590|foma|samsu|vx8|vx9|a1000|_mms|myx|a700|gu1100|bc831|e300|ems100|me701|me702m-three|sd588|s800|8325rc|ac831|mw200|brew |d88|htc\/|htc_touch|355x|m50|km100|d736|p-9521|telco|sl74|ktouch|m4u\/|me702|8325rc|kddi|phone|lg |sonyericsson|samsung|240x|x320|vx10|nokia|sony cmd|motorola|up.browser|up.link|mmp|symbian|smartphone|midp|wap|vodafone|o2|pocket|kindle|mobile|psp|treo)'=>'unknown'
	);
	
    private static $instance;
    
    public $UserAgent;
    public $IsWAP;
    public $IsMobile;
    public $IsTablet;
	public $IsConsole;
    public $Vendor;
	public $SupportsJS;
	public $SupportsCSS;
	public $IsNativeDevice;
	public $NativeClientVersion;
   
    /**
     * Private constructor enforces the Singleton pattern
     */
    private function __construct() 
    {
    	// do nothing
    }
    
    /**
     * Parse the user agent and detect the browser, populating
     * all internal properties accordingly
     */
    public function Detect()
    {
    	// RESET DEFAULT VALUES
    	$this->IsWAP = false;
	    $this->IsMobile = false;
	    $this->IsTablet = false;
		$this->IsConsole = false;
	    $this->Vendor = 'unknown';
		$this->SupportsJS = true;
		$this->SupportsCSS = true;
	
    	$this->UserAgent = isset($_SERVER['HTTP_USER_AGENT']) ? $_SERVER['HTTP_USER_AGENT'] : "";
    	$wap = isset($_SERVER['HTTP_X_WAP_PROFILE']) ? $_SERVER['HTTP_X_WAP_PROFILE'] : "";
    

    	if (!$this->UserAgent)
    	{
    		$this->IsConsole = true;
    	}
    	else
    	{
    		foreach (BrowserDevice::$MOBILE_DEVICE_PATTERNS as $key => $val )
    		{
    			if (preg_match('/'.$key.'/i',$this->UserAgent))
    			{
    				$this->IsMobile = true;
    				$this->Vendor = $val;
    				break;
    			}
    		}
				
			if ($this->IsMobile == false) {
				
				foreach (BrowserDevice::$DESKTOP_DEVICE_PATTERNS as $key => $val )
	    		{
	    			if (preg_match('/'.$key.'/i',$this->UserAgent))
	    			{
	    				$this->Vendor = $val;
	    				break;
	    			}
	    		}
	
			}


    	}
    }

	/**
	 * Returns an instance of a BrowserDevice populated based on the
	 * server variables provided
	 * @return BrowserDevice
	 */
    public static function GetInstance() 
    {
        if (!isset(self::$instance)) 
        {
            $c = __CLASS__;
            self::$instance = new $c;
            self::$instance->Detect();
        }

		return self::$instance;
    }
	
}

?>