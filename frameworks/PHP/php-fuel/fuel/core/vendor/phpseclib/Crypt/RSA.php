<?php
/* vim: set expandtab tabstop=4 shiftwidth=4 softtabstop=4: */

namespace PHPSecLib;

/**
 * Pure-PHP PKCS#1 (v2.1) compliant implementation of RSA.
 *
 * PHP versions 4 and 5
 *
 * Here's an example of how to encrypt and decrypt text with this library:
 * <code>
 * <?php
 *    include('Crypt/RSA.php');
 *
 *    $rsa = new Crypt_RSA();
 *    extract($rsa->createKey());
 *
 *    $plaintext = 'terrafrost';
 *
 *    $rsa->loadKey($privatekey);
 *    $ciphertext = $rsa->encrypt($plaintext);
 *
 *    $rsa->loadKey($publickey);
 *    echo $rsa->decrypt($ciphertext);
 * ?>
 * </code>
 *
 * Here's an example of how to create signatures and verify signatures with this library:
 * <code>
 * <?php
 *    include('Crypt/RSA.php');
 *
 *    $rsa = new Crypt_RSA();
 *    extract($rsa->createKey());
 *
 *    $plaintext = 'terrafrost';
 *
 *    $rsa->loadKey($privatekey);
 *    $signature = $rsa->sign($plaintext);
 *
 *    $rsa->loadKey($publickey);
 *    echo $rsa->verify($plaintext, $signature) ? 'verified' : 'unverified';
 * ?>
 * </code>
 *
 * LICENSE: Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 * @category   Crypt
 * @package    Crypt_RSA
 * @author     Jim Wigginton <terrafrost@php.net>
 * @copyright  MMIX Jim Wigginton
 * @license    http://www.opensource.org/licenses/mit-license.html  MIT License
 * @version    $Id: RSA.php,v 1.19 2010/09/12 21:58:54 terrafrost Exp $
 * @link       http://phpseclib.sourceforge.net
 */

/**#@+
 * Crypt random global function
 *
 * @see Crypt/Random.php
 */
require_once (__DIR__.DS.'../Crypt/Random.php');

/**#@+
 * @access public
 * @see Crypt_RSA::encrypt()
 * @see Crypt_RSA::decrypt()
 */
/**
 * Use {@link http://en.wikipedia.org/wiki/Optimal_Asymmetric_Encryption_Padding Optimal Asymmetric Encryption Padding}
 * (OAEP) for encryption / decryption.
 *
 * Uses sha1 by default.
 *
 * @see Crypt_RSA::setHash()
 * @see Crypt_RSA::setMGFHash()
 */
define('CRYPT_RSA_ENCRYPTION_OAEP',  1);
/**
 * Use PKCS#1 padding.
 *
 * Although CRYPT_RSA_ENCRYPTION_OAEP offers more security, including PKCS#1 padding is necessary for purposes of backwards
 * compatability with protocols (like SSH-1) written before OAEP's introduction.
 */
define('CRYPT_RSA_ENCRYPTION_PKCS1', 2);
/**#@-*/

/**#@+
 * @access public
 * @see Crypt_RSA::sign()
 * @see Crypt_RSA::verify()
 * @see Crypt_RSA::setHash()
 */
/**
 * Use the Probabilistic Signature Scheme for signing
 *
 * Uses sha1 by default.
 *
 * @see Crypt_RSA::setSaltLength()
 * @see Crypt_RSA::setMGFHash()
 */
define('CRYPT_RSA_SIGNATURE_PSS',  1);
/**
 * Use the PKCS#1 scheme by default.
 *
 * Although CRYPT_RSA_SIGNATURE_PSS offers more security, including PKCS#1 signing is necessary for purposes of backwards
 * compatability with protocols (like SSH-2) written before PSS's introduction.
 */
define('CRYPT_RSA_SIGNATURE_PKCS1', 2);
/**#@-*/

/**#@+
 * @access private
 * @see Crypt_RSA::createKey()
 */
/**
 * ASN1 Integer
 */
define('CRYPT_RSA_ASN1_INTEGER',   2);
/**
 * ASN1 Sequence (with the constucted bit set)
 */
define('CRYPT_RSA_ASN1_SEQUENCE', 48);
/**#@-*/

/**#@+
 * @access private
 * @see Crypt_RSA::Crypt_RSA()
 */
/**
 * To use the pure-PHP implementation
 */
define('CRYPT_RSA_MODE_INTERNAL', 1);
/**
 * To use the OpenSSL library
 *
 * (if enabled; otherwise, the internal implementation will be used)
 */
define('CRYPT_RSA_MODE_OPENSSL', 2);
/**#@-*/

/**#@+
 * @access public
 * @see Crypt_RSA::createKey()
 * @see Crypt_RSA::setPrivateKeyFormat()
 */
/**
 * PKCS#1 formatted private key
 *
 * Used by OpenSSH
 */
define('CRYPT_RSA_PRIVATE_FORMAT_PKCS1', 0);
/**
 * PuTTY formatted private key
 */
define('CRYPT_RSA_PRIVATE_FORMAT_PUTTY', 1);
/**
 * XML formatted private key
 */
define('CRYPT_RSA_PRIVATE_FORMAT_XML', 2);
/**#@-*/

/**#@+
 * @access public
 * @see Crypt_RSA::createKey()
 * @see Crypt_RSA::setPublicKeyFormat()
 */
/**
 * Raw public key
 *
 * An array containing two Math_BigInteger objects.
 *
 * The exponent can be indexed with any of the following:
 *
 * 0, e, exponent, publicExponent
 *
 * The modulus can be indexed with any of the following:
 *
 * 1, n, modulo, modulus
 */
define('CRYPT_RSA_PUBLIC_FORMAT_RAW', 3);
/**
 * PKCS#1 formatted public key
 */
define('CRYPT_RSA_PUBLIC_FORMAT_PKCS1', 4);
/**
 * XML formatted public key
 */
define('CRYPT_RSA_PUBLIC_FORMAT_XML', 5);
/**
 * OpenSSH formatted public key
 *
 * Place in $HOME/.ssh/authorized_keys
 */
define('CRYPT_RSA_PUBLIC_FORMAT_OPENSSH', 6);
/**#@-*/

/**
 * Pure-PHP PKCS#1 compliant implementation of RSA.
 *
 * @author  Jim Wigginton <terrafrost@php.net>
 * @version 0.1.0
 * @access  public
 * @package Crypt_RSA
 */
class Crypt_RSA {
    /**
     * Precomputed Zero
     *
     * @var Array
     * @access private
     */
    var $zero;

    /**
     * Precomputed One
     *
     * @var Array
     * @access private
     */
    var $one;

    /**
     * Private Key Format
     *
     * @var Integer
     * @access private
     */
    var $privateKeyFormat = CRYPT_RSA_PRIVATE_FORMAT_PKCS1;

    /**
     * Public Key Format
     *
     * @var Integer
     * @access public
     */
    var $publicKeyFormat = CRYPT_RSA_PUBLIC_FORMAT_PKCS1;

    /**
     * Modulus (ie. n)
     *
     * @var Math_BigInteger
     * @access private
     */
    var $modulus;

    /**
     * Modulus length
     *
     * @var Math_BigInteger
     * @access private
     */
    var $k;

    /**
     * Exponent (ie. e or d)
     *
     * @var Math_BigInteger
     * @access private
     */
    var $exponent;

    /**
     * Primes for Chinese Remainder Theorem (ie. p and q)
     *
     * @var Array
     * @access private
     */
    var $primes;

    /**
     * Exponents for Chinese Remainder Theorem (ie. dP and dQ)
     *
     * @var Array
     * @access private
     */
    var $exponents;

    /**
     * Coefficients for Chinese Remainder Theorem (ie. qInv)
     *
     * @var Array
     * @access private
     */
    var $coefficients;

    /**
     * Hash name
     *
     * @var String
     * @access private
     */
    var $hashName;

    /**
     * Hash function
     *
     * @var Crypt_Hash
     * @access private
     */
    var $hash;

    /**
     * Length of hash function output
     *
     * @var Integer
     * @access private
     */
    var $hLen;

    /**
     * Length of salt
     *
     * @var Integer
     * @access private
     */
    var $sLen;

    /**
     * Hash function for the Mask Generation Function
     *
     * @var Crypt_Hash
     * @access private
     */
    var $mgfHash;

    /**
     * Length of MGF hash function output
     *
     * @var Integer
     * @access private
     */
    var $mgfHLen;

    /**
     * Encryption mode
     *
     * @var Integer
     * @access private
     */
    var $encryptionMode = CRYPT_RSA_ENCRYPTION_OAEP;

    /**
     * Signature mode
     *
     * @var Integer
     * @access private
     */
    var $signatureMode = CRYPT_RSA_SIGNATURE_PSS;

    /**
     * Public Exponent
     *
     * @var Mixed
     * @access private
     */
    var $publicExponent = false;

    /**
     * Password
     *
     * @var String
     * @access private
     */
    var $password = '';

    /**
     * Components
     *
     * For use with parsing XML formatted keys.  PHP's XML Parser functions use utilized - instead of PHP's DOM functions -
     * because PHP's XML Parser functions work on PHP4 whereas PHP's DOM functions - although surperior - don't.
     *
     * @see Crypt_RSA::_start_element_handler()
     * @var Array
     * @access private
     */
    var $components = array();

    /**
     * Current String
     *
     * For use with parsing XML formatted keys.
     *
     * @see Crypt_RSA::_character_handler()
     * @see Crypt_RSA::_stop_element_handler()
     * @var Mixed
     * @access private
     */
    var $current;

    /**
     * The constructor
     *
     * If you want to make use of the openssl extension, you'll need to set the mode manually, yourself.  The reason
     * Crypt_RSA doesn't do it is because OpenSSL doesn't fail gracefully.  openssl_pkey_new(), in particular, requires
     * openssl.cnf be present somewhere and, unfortunately, the only real way to find out is too late.
     *
     * @return Crypt_RSA
     * @access public
     */
    function __construct()
    {
        if ( !defined('CRYPT_RSA_MODE') ) {
            switch (true) {
                //case extension_loaded('openssl') && version_compare(PHP_VERSION, '4.2.0', '>='):
                //    define('CRYPT_RSA_MODE', CRYPT_RSA_MODE_OPENSSL);
                //    break;
                default:
                    define('CRYPT_RSA_MODE', CRYPT_RSA_MODE_INTERNAL);
            }
        }

        $this->zero = new Math_BigInteger();
        $this->one = new Math_BigInteger(1);

        $this->hash = new Crypt_Hash('sha1');
        $this->hLen = $this->hash->getLength();
        $this->hashName = 'sha1';
        $this->mgfHash = new Crypt_Hash('sha1');
        $this->mgfHLen = $this->mgfHash->getLength();
    }

    /**
     * Create public / private key pair
     *
     * Returns an array with the following three elements:
     *  - 'privatekey': The private key.
     *  - 'publickey':  The public key.
     *  - 'partialkey': A partially computed key (if the execution time exceeded $timeout).
     *                  Will need to be passed back to Crypt_RSA::createKey() as the third parameter for further processing.
     *
     * @access public
     * @param optional Integer $bits
     * @param optional Integer $timeout
     * @param optional Math_BigInteger $p
     */
    function createKey($bits = 1024, $timeout = false, $partial = array())
    {
        if ( CRYPT_RSA_MODE == CRYPT_RSA_MODE_OPENSSL ) {
            $rsa = openssl_pkey_new(array('private_key_bits' => $bits));
            openssl_pkey_export($rsa, $privatekey);
            $publickey = openssl_pkey_get_details($rsa);
            $publickey = $publickey['key'];

            if ($this->privateKeyFormat != CRYPT_RSA_PRIVATE_FORMAT_PKCS1) {
                $privatekey = call_user_func_array(array($this, '_convertPrivateKey'), array_values($this->_parseKey($privatekey, CRYPT_RSA_PRIVATE_FORMAT_PKCS1)));
                $publickey = call_user_func_array(array($this, '_convertPublicKey'), array_values($this->_parseKey($publickey, CRYPT_RSA_PUBLIC_FORMAT_PKCS1)));
            }

            return array(
                'privatekey' => $privatekey,
                'publickey' => $publickey,
                'partialkey' => false
            );
        }

        static $e;
        if (!isset($e)) {
            if (!defined('CRYPT_RSA_EXPONENT')) {
                // http://en.wikipedia.org/wiki/65537_%28number%29
                define('CRYPT_RSA_EXPONENT', '65537');
            }
            if (!defined('CRYPT_RSA_COMMENT')) {
                define('CRYPT_RSA_COMMENT', 'phpseclib-generated-key');
            }
            // per <http://cseweb.ucsd.edu/~hovav/dist/survey.pdf#page=5>, this number ought not result in primes smaller
            // than 256 bits.
            if (!defined('CRYPT_RSA_SMALLEST_PRIME')) {
                define('CRYPT_RSA_SMALLEST_PRIME', 4096);
            }

            $e = new Math_BigInteger(CRYPT_RSA_EXPONENT);
        }

        extract($this->_generateMinMax($bits));
        $absoluteMin = $min;
        $temp = $bits >> 1;
        if ($temp > CRYPT_RSA_SMALLEST_PRIME) {
            $num_primes = floor($bits / CRYPT_RSA_SMALLEST_PRIME);
            $temp = CRYPT_RSA_SMALLEST_PRIME;
        } else {
            $num_primes = 2;
        }
        extract($this->_generateMinMax($temp + $bits % $temp));
        $finalMax = $max;
        extract($this->_generateMinMax($temp));

        $generator = new Math_BigInteger();
        $generator->setRandomGenerator('crypt_random');

        $n = $this->one->copy();
        if (!empty($partial)) {
            extract(unserialize($partial));
        } else {
            $exponents = $coefficients = $primes = array();
            $lcm = array(
                'top' => $this->one->copy(),
                'bottom' => false
            );
        }

        $start = time();
        $i0 = count($primes) + 1;

        do {
            for ($i = $i0; $i <= $num_primes; $i++) {
                if ($timeout !== false) {
                    $timeout-= time() - $start;
                    $start = time();
                    if ($timeout <= 0) {
                        return array(
                            'privatekey' => '',
                            'publickey'  => '',
                            'partialkey' => serialize(array(
                                'primes' => $primes,
                                'coefficients' => $coefficients,
                                'lcm' => $lcm,
                                'exponents' => $exponents
                            ))
                        );
                    }
                }

                if ($i == $num_primes) {
                    list($min, $temp) = $absoluteMin->divide($n);
                    if (!$temp->equals($this->zero)) {
                        $min = $min->add($this->one); // ie. ceil()
                    }
                    $primes[$i] = $generator->randomPrime($min, $finalMax, $timeout);
                } else {
                    $primes[$i] = $generator->randomPrime($min, $max, $timeout);
                }

                if ($primes[$i] === false) { // if we've reached the timeout
                    if (count($primes) > 1) {
                        $partialkey = '';
                    } else {
                        array_pop($primes);
                        $partialkey = serialize(array(
                            'primes' => $primes,
                            'coefficients' => $coefficients,
                            'lcm' => $lcm,
                            'exponents' => $exponents
                        ));
                    }

                    return array(
                        'privatekey' => '',
                        'publickey'  => '',
                        'partialkey' => $partialkey
                    );
                }

                // the first coefficient is calculated differently from the rest
                // ie. instead of being $primes[1]->modInverse($primes[2]), it's $primes[2]->modInverse($primes[1])
                if ($i > 2) {
                    $coefficients[$i] = $n->modInverse($primes[$i]);
                }

                $n = $n->multiply($primes[$i]);

                $temp = $primes[$i]->subtract($this->one);

                // textbook RSA implementations use Euler's totient function instead of the least common multiple.
                // see http://en.wikipedia.org/wiki/Euler%27s_totient_function
                $lcm['top'] = $lcm['top']->multiply($temp);
                $lcm['bottom'] = $lcm['bottom'] === false ? $temp : $lcm['bottom']->gcd($temp);

                $exponents[$i] = $e->modInverse($temp);
            }

            list($lcm) = $lcm['top']->divide($lcm['bottom']);
            $gcd = $lcm->gcd($e);
            $i0 = 1;
        } while (!$gcd->equals($this->one));

        $d = $e->modInverse($lcm);

        $coefficients[2] = $primes[2]->modInverse($primes[1]);

        // from <http://tools.ietf.org/html/rfc3447#appendix-A.1.2>:
        // RSAPrivateKey ::= SEQUENCE {
        //     version           Version,
        //     modulus           INTEGER,  -- n
        //     publicExponent    INTEGER,  -- e
        //     privateExponent   INTEGER,  -- d
        //     prime1            INTEGER,  -- p
        //     prime2            INTEGER,  -- q
        //     exponent1         INTEGER,  -- d mod (p-1)
        //     exponent2         INTEGER,  -- d mod (q-1)
        //     coefficient       INTEGER,  -- (inverse of q) mod p
        //     otherPrimeInfos   OtherPrimeInfos OPTIONAL
        // }

        return array(
            'privatekey' => $this->_convertPrivateKey($n, $e, $d, $primes, $exponents, $coefficients),
            'publickey'  => $this->_convertPublicKey($n, $e),
            'partialkey' => false
        );
    }

    /**
     * Convert a private key to the appropriate format.
     *
     * @access private
     * @see setPrivateKeyFormat()
     * @param String $RSAPrivateKey
     * @return String
     */
    function _convertPrivateKey($n, $e, $d, $primes, $exponents, $coefficients)
    {
        $num_primes = count($primes);
        $raw = array(
            'version' => $num_primes == 2 ? chr(0) : chr(1), // two-prime vs. multi
            'modulus' => $n->toBytes(true),
            'publicExponent' => $e->toBytes(true),
            'privateExponent' => $d->toBytes(true),
            'prime1' => $primes[1]->toBytes(true),
            'prime2' => $primes[2]->toBytes(true),
            'exponent1' => $exponents[1]->toBytes(true),
            'exponent2' => $exponents[2]->toBytes(true),
            'coefficient' => $coefficients[2]->toBytes(true)
        );

        // if the format in question does not support multi-prime rsa and multi-prime rsa was used,
        // call _convertPublicKey() instead.
        switch ($this->privateKeyFormat) {
            default: // eg. CRYPT_RSA_PRIVATE_FORMAT_PKCS1
                $components = array();
                foreach ($raw as $name => $value) {
                    $components[$name] = pack('Ca*a*', CRYPT_RSA_ASN1_INTEGER, $this->_encodeLength(strlen($value)), $value);
                }

                $RSAPrivateKey = implode('', $components);

                if ($num_primes > 2) {
                    $OtherPrimeInfos = '';
                    for ($i = 3; $i <= $num_primes; $i++) {
                        // OtherPrimeInfos ::= SEQUENCE SIZE(1..MAX) OF OtherPrimeInfo
                        //
                        // OtherPrimeInfo ::= SEQUENCE {
                        //     prime             INTEGER,  -- ri
                        //     exponent          INTEGER,  -- di
                        //     coefficient       INTEGER   -- ti
                        // }
                        $OtherPrimeInfo = pack('Ca*a*', CRYPT_RSA_ASN1_INTEGER, $this->_encodeLength(strlen($primes[$i]->toBytes(true))), $primes[$i]->toBytes(true));
                        $OtherPrimeInfo.= pack('Ca*a*', CRYPT_RSA_ASN1_INTEGER, $this->_encodeLength(strlen($exponents[$i]->toBytes(true))), $exponents[$i]->toBytes(true));
                        $OtherPrimeInfo.= pack('Ca*a*', CRYPT_RSA_ASN1_INTEGER, $this->_encodeLength(strlen($coefficients[$i]->toBytes(true))), $coefficients[$i]->toBytes(true));
                        $OtherPrimeInfos.= pack('Ca*a*', CRYPT_RSA_ASN1_SEQUENCE, $this->_encodeLength(strlen($OtherPrimeInfo)), $OtherPrimeInfo);
                    }
                    $RSAPrivateKey.= pack('Ca*a*', CRYPT_RSA_ASN1_SEQUENCE, $this->_encodeLength(strlen($OtherPrimeInfos)), $OtherPrimeInfos);
                }

                $RSAPrivateKey = pack('Ca*a*', CRYPT_RSA_ASN1_SEQUENCE, $this->_encodeLength(strlen($RSAPrivateKey)), $RSAPrivateKey);

                if (!empty($this->password)) {
                    $iv = $this->_random(8);
                    $symkey = pack('H*', md5($this->password . $iv)); // symkey is short for symmetric key
                    $symkey.= substr(pack('H*', md5($symkey . $this->password . $iv)), 0, 8);
                    $des = new Crypt_TripleDES();
                    $des->setKey($symkey);
                    $des->setIV($iv);
                    $iv = strtoupper(bin2hex($iv));
                    $RSAPrivateKey = "-----BEGIN RSA PRIVATE KEY-----\r\n" .
                                     "Proc-Type: 4,ENCRYPTED\r\n" .
                                     "DEK-Info: DES-EDE3-CBC,$iv\r\n" .
                                     "\r\n" .
                                     chunk_split(base64_encode($des->encrypt($RSAPrivateKey))) .
                                     '-----END RSA PRIVATE KEY-----';
                } else {
                    $RSAPrivateKey = "-----BEGIN RSA PRIVATE KEY-----\r\n" .
                                     chunk_split(base64_encode($RSAPrivateKey)) .
                                     '-----END RSA PRIVATE KEY-----';
                }

                return $RSAPrivateKey;
        }
    }

    /**
     * Convert a public key to the appropriate format
     *
     * @access private
     * @see setPublicKeyFormat()
     * @param String $RSAPrivateKey
     * @return String
     */
    function _convertPublicKey($n, $e)
    {
        $modulus = $n->toBytes(true);
        $publicExponent = $e->toBytes(true);

        switch ($this->publicKeyFormat) {
            case CRYPT_RSA_PUBLIC_FORMAT_RAW:
                return array('e' => $e->copy(), 'n' => $n->copy());
            case CRYPT_RSA_PUBLIC_FORMAT_OPENSSH:
                // from <http://tools.ietf.org/html/rfc4253#page-15>:
                // string    "ssh-rsa"
                // mpint     e
                // mpint     n
                $RSAPublicKey = pack('Na*Na*Na*', strlen('ssh-rsa'), 'ssh-rsa', strlen($publicExponent), $publicExponent, strlen($modulus), $modulus);
                $RSAPublicKey = 'ssh-rsa ' . base64_encode($RSAPublicKey) . ' ' . CRYPT_RSA_COMMENT;

                return $RSAPublicKey;
            default: // eg. CRYPT_RSA_PUBLIC_FORMAT_PKCS1
                // from <http://tools.ietf.org/html/rfc3447#appendix-A.1.1>:
                // RSAPublicKey ::= SEQUENCE {
                //     modulus           INTEGER,  -- n
                //     publicExponent    INTEGER   -- e
                // }
                $components = array(
                    'modulus' => pack('Ca*a*', CRYPT_RSA_ASN1_INTEGER, $this->_encodeLength(strlen($modulus)), $modulus),
                    'publicExponent' => pack('Ca*a*', CRYPT_RSA_ASN1_INTEGER, $this->_encodeLength(strlen($publicExponent)), $publicExponent)
                );

                $RSAPublicKey = pack('Ca*a*a*',
                    CRYPT_RSA_ASN1_SEQUENCE, $this->_encodeLength(strlen($components['modulus']) + strlen($components['publicExponent'])),
                    $components['modulus'], $components['publicExponent']
                );

                $RSAPublicKey = "-----BEGIN PUBLIC KEY-----\r\n" .
                                 chunk_split(base64_encode($RSAPublicKey)) .
                                 '-----END PUBLIC KEY-----';

                return $RSAPublicKey;
        }
    }

    /**
     * Break a public or private key down into its constituant components
     *
     * @access private
     * @see _convertPublicKey()
     * @see _convertPrivateKey()
     * @param String $key
     * @param Integer $type
     * @return Array
     */
    function _parseKey($key, $type)
    {
        switch ($type) {
            case CRYPT_RSA_PUBLIC_FORMAT_RAW:
                if (!is_array($key)) {
                    return false;
                }
                $components = array();
                switch (true) {
                    case isset($key['e']):
                        $components['publicExponent'] = $key['e']->copy();
                        break;
                    case isset($key['exponent']):
                        $components['publicExponent'] = $key['exponent']->copy();
                        break;
                    case isset($key['publicExponent']):
                        $components['publicExponent'] = $key['publicExponent']->copy();
                        break;
                    case isset($key[0]):
                        $components['publicExponent'] = $key[0]->copy();
                }
                switch (true) {
                    case isset($key['n']):
                        $components['modulus'] = $key['n']->copy();
                        break;
                    case isset($key['modulo']):
                        $components['modulus'] = $key['modulo']->copy();
                        break;
                    case isset($key['modulus']):
                        $components['modulus'] = $key['modulus']->copy();
                        break;
                    case isset($key[1]):
                        $components['modulus'] = $key[1]->copy();
                }
                return $components;
            case CRYPT_RSA_PRIVATE_FORMAT_PKCS1:
            case CRYPT_RSA_PUBLIC_FORMAT_PKCS1:
                /* Although PKCS#1 proposes a format that public and private keys can use, encrypting them is
                   "outside the scope" of PKCS#1.  PKCS#1 then refers you to PKCS#12 and PKCS#15 if you're wanting to
                   protect private keys, however, that's not what OpenSSL* does.  OpenSSL protects private keys by adding
                   two new "fields" to the key - DEK-Info and Proc-Type.  These fields are discussed here:

                   http://tools.ietf.org/html/rfc1421#section-4.6.1.1
                   http://tools.ietf.org/html/rfc1421#section-4.6.1.3

                   DES-EDE3-CBC as an algorithm, however, is not discussed anywhere, near as I can tell.
                   DES-CBC and DES-EDE are discussed in RFC1423, however, DES-EDE3-CBC isn't, nor is its key derivation
                   function.  As is, the definitive authority on this encoding scheme isn't the IETF but rather OpenSSL's
                   own implementation.  ie. the implementation *is* the standard and any bugs that may exist in that
                   implementation are part of the standard, as well.

                   * OpenSSL is the de facto standard.  It's utilized by OpenSSH and other projects */
                if (preg_match('#DEK-Info: (.+),(.+)#', $key, $matches)) {
                    $iv = pack('H*', trim($matches[2]));
                    $symkey = pack('H*', md5($this->password . substr($iv, 0, 8))); // symkey is short for symmetric key
                    $symkey.= substr(pack('H*', md5($symkey . $this->password . $iv)), 0, 8);
                    $ciphertext = preg_replace('#.+(\r|\n|\r\n)\1|[\r\n]|-.+-#s', '', $key);
                    $ciphertext = preg_match('#^[a-zA-Z\d/+]*={0,2}$#', $ciphertext) ? base64_decode($ciphertext) : false;
                    if ($ciphertext === false) {
                        $ciphertext = $key;
                    }
                    switch ($matches[1]) {
                        case 'AES-128-CBC':
                            $symkey = substr($symkey, 0, 16);
                            $crypto = new Crypt_AES();
                            break;
                        case 'DES-EDE3-CFB':
                            $crypto = new Crypt_TripleDES(CRYPT_DES_MODE_CFB);
                            break;
                        case 'DES-EDE3-CBC':
                            $crypto = new Crypt_TripleDES();
                            break;
                        case 'DES-CBC':
                            $crypto = new Crypt_DES();
                            break;
                        default:
                            return false;
                    }
                    $crypto->setKey($symkey);
                    $crypto->setIV($iv);
                    $decoded = $crypto->decrypt($ciphertext);
                } else {
                    $decoded = preg_replace('#-.+-|[\r\n]#', '', $key);
                    $decoded = preg_match('#^[a-zA-Z\d/+]*={0,2}$#', $decoded) ? base64_decode($decoded) : false;
                }

                if ($decoded !== false) {
                    $key = $decoded;
                }

                $components = array();

                if (ord($this->_string_shift($key)) != CRYPT_RSA_ASN1_SEQUENCE) {
                    return false;
                }
                if ($this->_decodeLength($key) != strlen($key)) {
                    return false;
                }

                $tag = ord($this->_string_shift($key));
                if ($tag == CRYPT_RSA_ASN1_SEQUENCE) {
                    /* intended for keys for which OpenSSL's asn1parse returns the following:

                        0:d=0  hl=4 l= 290 cons: SEQUENCE
                        4:d=1  hl=2 l=  13 cons:  SEQUENCE
                        6:d=2  hl=2 l=   9 prim:   OBJECT            :rsaEncryption
                       17:d=2  hl=2 l=   0 prim:   NULL
                       19:d=1  hl=4 l= 271 prim:  BIT STRING */
                    $this->_string_shift($key, $this->_decodeLength($key));
                    $this->_string_shift($key); // skip over the BIT STRING tag
                    $this->_decodeLength($key); // skip over the BIT STRING length
                    // "The initial octet shall encode, as an unsigned binary integer wtih bit 1 as the least significant bit, the number of
                    //  unused bits in teh final subsequent octet. The number shall be in the range zero to seven."
                    //  -- http://www.itu.int/ITU-T/studygroups/com17/languages/X.690-0207.pdf (section 8.6.2.2)
                    $this->_string_shift($key);
                    if (ord($this->_string_shift($key)) != CRYPT_RSA_ASN1_SEQUENCE) {
                        return false;
                    }
                    if ($this->_decodeLength($key) != strlen($key)) {
                        return false;
                    }
                    $tag = ord($this->_string_shift($key));
                }
                if ($tag != CRYPT_RSA_ASN1_INTEGER) {
                    return false;
                }

                $length = $this->_decodeLength($key);
                $temp = $this->_string_shift($key, $length);
                if (strlen($temp) != 1 || ord($temp) > 2) {
                    $components['modulus'] = new Math_BigInteger($temp, -256);
                    $this->_string_shift($key); // skip over CRYPT_RSA_ASN1_INTEGER
                    $length = $this->_decodeLength($key);
                    $components[$type == CRYPT_RSA_PUBLIC_FORMAT_PKCS1 ? 'publicExponent' : 'privateExponent'] = new Math_BigInteger($this->_string_shift($key, $length), -256);

                    return $components;
                }
                if (ord($this->_string_shift($key)) != CRYPT_RSA_ASN1_INTEGER) {
                    return false;
                }
                $length = $this->_decodeLength($key);
                $components['modulus'] = new Math_BigInteger($this->_string_shift($key, $length), -256);
                $this->_string_shift($key);
                $length = $this->_decodeLength($key);
                $components['publicExponent'] = new Math_BigInteger($this->_string_shift($key, $length), -256);
                $this->_string_shift($key);
                $length = $this->_decodeLength($key);
                $components['privateExponent'] = new Math_BigInteger($this->_string_shift($key, $length), -256);
                $this->_string_shift($key);
                $length = $this->_decodeLength($key);
                $components['primes'] = array(1 => new Math_BigInteger($this->_string_shift($key, $length), -256));
                $this->_string_shift($key);
                $length = $this->_decodeLength($key);
                $components['primes'][] = new Math_BigInteger($this->_string_shift($key, $length), -256);
                $this->_string_shift($key);
                $length = $this->_decodeLength($key);
                $components['exponents'] = array(1 => new Math_BigInteger($this->_string_shift($key, $length), -256));
                $this->_string_shift($key);
                $length = $this->_decodeLength($key);
                $components['exponents'][] = new Math_BigInteger($this->_string_shift($key, $length), -256);
                $this->_string_shift($key);
                $length = $this->_decodeLength($key);
                $components['coefficients'] = array(2 => new Math_BigInteger($this->_string_shift($key, $length), -256));

                if (!empty($key)) {
                    if (ord($this->_string_shift($key)) != CRYPT_RSA_ASN1_SEQUENCE) {
                        return false;
                    }
                    $this->_decodeLength($key);
                    while (!empty($key)) {
                        if (ord($this->_string_shift($key)) != CRYPT_RSA_ASN1_SEQUENCE) {
                            return false;
                        }
                        $this->_decodeLength($key);
                        $key = substr($key, 1);
                        $length = $this->_decodeLength($key);
                        $components['primes'][] = new Math_BigInteger($this->_string_shift($key, $length), -256);
                        $this->_string_shift($key);
                        $length = $this->_decodeLength($key);
                        $components['exponents'][] = new Math_BigInteger($this->_string_shift($key, $length), -256);
                        $this->_string_shift($key);
                        $length = $this->_decodeLength($key);
                        $components['coefficients'][] = new Math_BigInteger($this->_string_shift($key, $length), -256);
                    }
                }

                return $components;
            case CRYPT_RSA_PUBLIC_FORMAT_OPENSSH:
                $key = base64_decode(preg_replace('#^ssh-rsa | .+$#', '', $key));
                if ($key === false) {
                    return false;
                }

                $cleanup = substr($key, 0, 11) == "\0\0\0\7ssh-rsa";

                extract(unpack('Nlength', $this->_string_shift($key, 4)));
                $publicExponent = new Math_BigInteger($this->_string_shift($key, $length), -256);
                extract(unpack('Nlength', $this->_string_shift($key, 4)));
                $modulus = new Math_BigInteger($this->_string_shift($key, $length), -256);

                if ($cleanup && strlen($key)) {
                    extract(unpack('Nlength', $this->_string_shift($key, 4)));
                    return array(
                        'modulus' => new Math_BigInteger($this->_string_shift($key, $length), -256),
                        'publicExponent' => $modulus
                    );
                } else {
                    return array(
                        'modulus' => $modulus,
                        'publicExponent' => $publicExponent
                    );
                }
            // http://www.w3.org/TR/xmldsig-core/#sec-RSAKeyValue
            // http://en.wikipedia.org/wiki/XML_Signature
            case CRYPT_RSA_PRIVATE_FORMAT_XML:
            case CRYPT_RSA_PUBLIC_FORMAT_XML:
                $this->components = array();

                $xml = xml_parser_create('UTF-8');
                xml_set_object($xml, $this);
                xml_set_element_handler($xml, '_start_element_handler', '_stop_element_handler');
                xml_set_character_data_handler($xml, '_data_handler');
                if (!xml_parse($xml, $key)) {
                    return false;
                }

                return $this->components;
            // from PuTTY's SSHPUBK.C
            case CRYPT_RSA_PRIVATE_FORMAT_PUTTY:
                $components = array();
                $key = preg_split('#\r\n|\r|\n#', $key);
                $type = trim(preg_replace('#PuTTY-User-Key-File-2: (.+)#', '$1', $key[0]));
                if ($type != 'ssh-rsa') {
                    return false;
                }
                $encryption = trim(preg_replace('#Encryption: (.+)#', '$1', $key[1]));

                $publicLength = trim(preg_replace('#Public-Lines: (\d+)#', '$1', $key[3]));
                $public = base64_decode(implode('', array_map('trim', array_slice($key, 4, $publicLength))));
                $public = substr($public, 11);
                extract(unpack('Nlength', $this->_string_shift($public, 4)));
                $components['publicExponent'] = new Math_BigInteger($this->_string_shift($public, $length), -256);
                extract(unpack('Nlength', $this->_string_shift($public, 4)));
                $components['modulus'] = new Math_BigInteger($this->_string_shift($public, $length), -256);

                $privateLength = trim(preg_replace('#Private-Lines: (\d+)#', '$1', $key[$publicLength + 4]));
                $private = base64_decode(implode('', array_map('trim', array_slice($key, $publicLength + 5, $privateLength))));

                switch ($encryption) {
                    case 'aes256-cbc':
                        $symkey = '';
                        $sequence = 0;
                        while (strlen($symkey) < 32) {
                            $temp = pack('Na*', $sequence++, $this->password);
                            $symkey.= pack('H*', sha1($temp));
                        }
                        $symkey = substr($symkey, 0, 32);
                        $crypto = new Crypt_AES();
                }

                if ($encryption != 'none') {
                    $crypto->setKey($symkey);
                    $crypto->disablePadding();
                    $private = $crypto->decrypt($private);
                    if ($private === false) {
                        return false;
                    }
                }

                extract(unpack('Nlength', $this->_string_shift($private, 4)));
                $components['privateExponent'] = new Math_BigInteger($this->_string_shift($private, $length), -256);
                extract(unpack('Nlength', $this->_string_shift($private, 4)));
                $components['primes'] = array(1 => new Math_BigInteger($this->_string_shift($private, $length), -256));
                extract(unpack('Nlength', $this->_string_shift($private, 4)));
                $components['primes'][] = new Math_BigInteger($this->_string_shift($private, $length), -256);

                $temp = $components['primes'][1]->subtract($this->one);
                $components['exponents'] = array(1 => $components['publicExponent']->modInverse($temp));
                $temp = $components['primes'][2]->subtract($this->one);
                $components['exponents'][] = $components['publicExponent']->modInverse($temp);

                extract(unpack('Nlength', $this->_string_shift($private, 4)));
                $components['coefficients'] = array(2 => new Math_BigInteger($this->_string_shift($private, $length), -256));

                return $components;
        }
    }

    /**
     * Start Element Handler
     *
     * Called by xml_set_element_handler()
     *
     * @access private
     * @param Resource $parser
     * @param String $name
     * @param Array $attribs
     */
    function _start_element_handler($parser, $name, $attribs)
    {
        //$name = strtoupper($name);
        switch ($name) {
            case 'MODULUS':
                $this->current = &$this->components['modulus'];
                break;
            case 'EXPONENT':
                $this->current = &$this->components['publicExponent'];
                break;
            case 'P':
                $this->current = &$this->components['primes'][1];
                break;
            case 'Q':
                $this->current = &$this->components['primes'][2];
                break;
            case 'DP':
                $this->current = &$this->components['exponents'][1];
                break;
            case 'DQ':
                $this->current = &$this->components['exponents'][2];
                break;
            case 'INVERSEQ':
                $this->current = &$this->components['coefficients'][2];
                break;
            case 'D':
                $this->current = &$this->components['privateExponent'];
                break;
            default:
                unset($this->current);
        }
        $this->current = '';
    }

    /**
     * Stop Element Handler
     *
     * Called by xml_set_element_handler()
     *
     * @access private
     * @param Resource $parser
     * @param String $name
     */
    function _stop_element_handler($parser, $name)
    {
        //$name = strtoupper($name);
        if ($name == 'RSAKEYVALUE') {
            return;
        }
        $this->current = new Math_BigInteger(base64_decode($this->current), 256);
    }

    /**
     * Data Handler
     *
     * Called by xml_set_character_data_handler()
     *
     * @access private
     * @param Resource $parser
     * @param String $data
     */
    function _data_handler($parser, $data)
    {
        if (!isset($this->current) || is_object($this->current)) {
            return;
        }
        $this->current.= trim($data);
    }

    /**
     * Loads a public or private key
     *
     * Returns true on success and false on failure (ie. an incorrect password was provided or the key was malformed)
     *
     * @access public
     * @param String $key
     * @param Integer $type optional
     */
    function loadKey($key, $type = false)
    {
        if ($type === false) {
            $types = array(
                CRYPT_RSA_PUBLIC_FORMAT_RAW,
                CRYPT_RSA_PRIVATE_FORMAT_PKCS1,
                CRYPT_RSA_PRIVATE_FORMAT_XML,
                CRYPT_RSA_PRIVATE_FORMAT_PUTTY,
                CRYPT_RSA_PUBLIC_FORMAT_OPENSSH
            );
            foreach ($types as $type) {
                $components = $this->_parseKey($key, $type);
                if ($components !== false) {
                    break;
                }
            }

        } else {
            $components = $this->_parseKey($key, $type);
        }

        if ($components === false) {
            return false;
        }

        $this->modulus = $components['modulus'];
        $this->k = strlen($this->modulus->toBytes());
        $this->exponent = isset($components['privateExponent']) ? $components['privateExponent'] : $components['publicExponent'];
        if (isset($components['primes'])) {
            $this->primes = $components['primes'];
            $this->exponents = $components['exponents'];
            $this->coefficients = $components['coefficients'];
            $this->publicExponent = $components['publicExponent'];
        } else {
            $this->primes = array();
            $this->exponents = array();
            $this->coefficients = array();
            $this->publicExponent = false;
        }

        return true;
    }

    /**
     * Sets the password
     *
     * Private keys can be encrypted with a password.  To unset the password, pass in the empty string or false.
     * Or rather, pass in $password such that empty($password) is true.
     *
     * @see createKey()
     * @see loadKey()
     * @access public
     * @param String $password
     */
    function setPassword($password)
    {
        $this->password = $password;
    }

    /**
     * Defines the public key
     *
     * Some private key formats define the public exponent and some don't.  Those that don't define it are problematic when
     * used in certain contexts.  For example, in SSH-2, RSA authentication works by sending the public key along with a
     * message signed by the private key to the server.  The SSH-2 server looks the public key up in an index of public keys
     * and if it's present then proceeds to verify the signature.  Problem is, if your private key doesn't include the public
     * exponent this won't work unless you manually add the public exponent.
     *
     * Do note that when a new key is loaded the index will be cleared.
     *
     * Returns true on success, false on failure
     *
     * @see getPublicKey()
     * @access public
     * @param String $key
     * @param Integer $type optional
     * @return Boolean
     */
    function setPublicKey($key, $type = CRYPT_RSA_PUBLIC_FORMAT_PKCS1)
    {
        $components = $this->_parseKey($key, $type);

        if (empty($this->modulus) || !$this->modulus->equals($components['modulus'])) {
            user_error('Trying to load a public key?  Use loadKey() instead.  It\'s called loadKey() and not loadPrivateKey() for a reason.', E_USER_NOTICE);
            return false;
        }

        $this->publicExponent = $components['publicExponent'];

        return true;
    }

    /**
     * Returns the public key
     *
     * The public key is only returned under two circumstances - if the private key had the public key embedded within it
     * or if the public key was set via setPublicKey().  If the currently loaded key is supposed to be the public key this
     * function won't return it since this library, for the most part, doesn't distinguish between public and private keys.
     *
     * @see getPublicKey()
     * @access public
     * @param String $key
     * @param Integer $type optional
     */
    function getPublicKey($type = CRYPT_RSA_PUBLIC_FORMAT_PKCS1)
    {
        if (empty($this->modulus) || empty($this->publicExponent)) {
            return false;
        }

        $oldFormat = $this->publicKeyFormat;
        $this->publicKeyFormat = $type;
        $temp = $this->_convertPublicKey($this->modulus, $this->publicExponent);
        $this->publicKeyFormat = $oldFormat;
        return $temp;
    }

    /**
     * Generates the smallest and largest numbers requiring $bits bits
     *
     * @access private
     * @param Integer $bits
     * @return Array
     */
    function _generateMinMax($bits)
    {
        $bytes = $bits >> 3;
        $min = str_repeat(chr(0), $bytes);
        $max = str_repeat(chr(0xFF), $bytes);
        $msb = $bits & 7;
        if ($msb) {
            $min = chr(1 << ($msb - 1)) . $min;
            $max = chr((1 << $msb) - 1) . $max;
        } else {
            $min[0] = chr(0x80);
        }

        return array(
            'min' => new Math_BigInteger($min, 256),
            'max' => new Math_BigInteger($max, 256)
        );
    }

    /**
     * DER-decode the length
     *
     * DER supports lengths up to (2**8)**127, however, we'll only support lengths up to (2**8)**4.  See
     * {@link http://itu.int/ITU-T/studygroups/com17/languages/X.690-0207.pdf#p=13 X.690 § 8.1.3} for more information.
     *
     * @access private
     * @param String $string
     * @return Integer
     */
    function _decodeLength(&$string)
    {
        $length = ord($this->_string_shift($string));
        if ( $length & 0x80 ) { // definite length, long form
            $length&= 0x7F;
            $temp = $this->_string_shift($string, $length);
            list(, $length) = unpack('N', substr(str_pad($temp, 4, chr(0), STR_PAD_LEFT), -4));
        }
        return $length;
    }

    /**
     * DER-encode the length
     *
     * DER supports lengths up to (2**8)**127, however, we'll only support lengths up to (2**8)**4.  See
     * {@link http://itu.int/ITU-T/studygroups/com17/languages/X.690-0207.pdf#p=13 X.690 § 8.1.3} for more information.
     *
     * @access private
     * @param Integer $length
     * @return String
     */
    function _encodeLength($length)
    {
        if ($length <= 0x7F) {
            return chr($length);
        }

        $temp = ltrim(pack('N', $length), chr(0));
        return pack('Ca*', 0x80 | strlen($temp), $temp);
    }

    /**
     * String Shift
     *
     * Inspired by array_shift
     *
     * @param String $string
     * @param optional Integer $index
     * @return String
     * @access private
     */
    function _string_shift(&$string, $index = 1)
    {
        $substr = substr($string, 0, $index);
        $string = substr($string, $index);
        return $substr;
    }

    /**
     * Determines the private key format
     *
     * @see createKey()
     * @access public
     * @param Integer $format
     */
    function setPrivateKeyFormat($format)
    {
        $this->privateKeyFormat = $format;
    }

    /**
     * Determines the public key format
     *
     * @see createKey()
     * @access public
     * @param Integer $format
     */
    function setPublicKeyFormat($format)
    {
        $this->publicKeyFormat = $format;
    }

    /**
     * Determines which hashing function should be used
     *
     * Used with signature production / verification and (if the encryption mode is CRYPT_RSA_ENCRYPTION_OAEP) encryption and
     * decryption.  If $hash isn't supported, sha1 is used.
     *
     * @access public
     * @param String $hash
     */
    function setHash($hash)
    {
        // Crypt_Hash supports algorithms that PKCS#1 doesn't support.  md5-96 and sha1-96, for example.
        switch ($hash) {
            case 'md2':
            case 'md5':
            case 'sha1':
            case 'sha256':
            case 'sha384':
            case 'sha512':
                $this->hash = new Crypt_Hash($hash);
                $this->hashName = $hash;
                break;
            default:
                $this->hash = new Crypt_Hash('sha1');
                $this->hashName = 'sha1';
        }
        $this->hLen = $this->hash->getLength();
    }

    /**
     * Determines which hashing function should be used for the mask generation function
     *
     * The mask generation function is used by CRYPT_RSA_ENCRYPTION_OAEP and CRYPT_RSA_SIGNATURE_PSS and although it's
     * best if Hash and MGFHash are set to the same thing this is not a requirement.
     *
     * @access public
     * @param String $hash
     */
    function setMGFHash($hash)
    {
        // Crypt_Hash supports algorithms that PKCS#1 doesn't support.  md5-96 and sha1-96, for example.
        switch ($hash) {
            case 'md2':
            case 'md5':
            case 'sha1':
            case 'sha256':
            case 'sha384':
            case 'sha512':
                $this->mgfHash = new Crypt_Hash($hash);
                break;
            default:
                $this->mgfHash = new Crypt_Hash('sha1');
        }
        $this->mgfHLen = $this->mgfHash->getLength();
    }

    /**
     * Determines the salt length
     *
     * To quote from {@link http://tools.ietf.org/html/rfc3447#page-38 RFC3447#page-38}:
     *
     *    Typical salt lengths in octets are hLen (the length of the output
     *    of the hash function Hash) and 0.
     *
     * @access public
     * @param Integer $format
     */
    function setSaltLength($sLen)
    {
        $this->sLen = $sLen;
    }

    /**
     * Generates a random string x bytes long
     *
     * @access public
     * @param Integer $bytes
     * @param optional Integer $nonzero
     * @return String
     */
    function _random($bytes, $nonzero = false)
    {
        $temp = '';
        if ($nonzero) {
            for ($i = 0; $i < $bytes; $i++) {
                $temp.= chr(crypt_random(1, 255));
            }
        } else {
            $ints = ($bytes + 1) >> 2;
            for ($i = 0; $i < $ints; $i++) {
                $temp.= pack('N', crypt_random());
            }
            $temp = substr($temp, 0, $bytes);
        }
        return $temp;
    }

    /**
     * Integer-to-Octet-String primitive
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-4.1 RFC3447#section-4.1}.
     *
     * @access private
     * @param Math_BigInteger $x
     * @param Integer $xLen
     * @return String
     */
    function _i2osp($x, $xLen)
    {
        $x = $x->toBytes();
        if (strlen($x) > $xLen) {
            user_error('Integer too large', E_USER_NOTICE);
            return false;
        }
        return str_pad($x, $xLen, chr(0), STR_PAD_LEFT);
    }

    /**
     * Octet-String-to-Integer primitive
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-4.2 RFC3447#section-4.2}.
     *
     * @access private
     * @param String $x
     * @return Math_BigInteger
     */
    function _os2ip($x)
    {
        return new Math_BigInteger($x, 256);
    }

    /**
     * Exponentiate with or without Chinese Remainder Theorem
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-5.1.1 RFC3447#section-5.1.2}.
     *
     * @access private
     * @param Math_BigInteger $x
     * @return Math_BigInteger
     */
    function _exponentiate($x)
    {
        if (empty($this->primes) || empty($this->coefficients) || empty($this->exponents)) {
            return $x->modPow($this->exponent, $this->modulus);
        }

        $num_primes = count($this->primes);

        if (defined('CRYPT_RSA_DISABLE_BLINDING')) {
            $m_i = array(
                1 => $x->modPow($this->exponents[1], $this->primes[1]),
                2 => $x->modPow($this->exponents[2], $this->primes[2])
            );
            $h = $m_i[1]->subtract($m_i[2]);
            $h = $h->multiply($this->coefficients[2]);
            list(, $h) = $h->divide($this->primes[1]);
            $m = $m_i[2]->add($h->multiply($this->primes[2]));

            $r = $this->primes[1];
            for ($i = 3; $i <= $num_primes; $i++) {
                $m_i = $x->modPow($this->exponents[$i], $this->primes[$i]);

                $r = $r->multiply($this->primes[$i - 1]);

                $h = $m_i->subtract($m);
                $h = $h->multiply($this->coefficients[$i]);
                list(, $h) = $h->divide($this->primes[$i]);

                $m = $m->add($r->multiply($h));
            }
        } else {
            $smallest = $this->primes[1];
            for ($i = 2; $i <= $num_primes; $i++) {
                if ($smallest->compare($this->primes[$i]) > 0) {
                    $smallest = $this->primes[$i];
                }
            }

            $one = new Math_BigInteger(1);
            $one->setRandomGenerator('crypt_random');

            $r = $one->random($one, $smallest->subtract($one));

            $m_i = array(
                1 => $this->_blind($x, $r, 1),
                2 => $this->_blind($x, $r, 2)
            );
            $h = $m_i[1]->subtract($m_i[2]);
            $h = $h->multiply($this->coefficients[2]);
            list(, $h) = $h->divide($this->primes[1]);
            $m = $m_i[2]->add($h->multiply($this->primes[2]));

            $r = $this->primes[1];
            for ($i = 3; $i <= $num_primes; $i++) {
                $m_i = $this->_blind($x, $r, $i);

                $r = $r->multiply($this->primes[$i - 1]);

                $h = $m_i->subtract($m);
                $h = $h->multiply($this->coefficients[$i]);
                list(, $h) = $h->divide($this->primes[$i]);

                $m = $m->add($r->multiply($h));
            }
        }

        return $m;
    }

    /**
     * Performs RSA Blinding
     *
     * Protects against timing attacks by employing RSA Blinding.
     * Returns $x->modPow($this->exponents[$i], $this->primes[$i])
     *
     * @access private
     * @param Math_BigInteger $x
     * @param Math_BigInteger $r
     * @param Integer $i
     * @return Math_BigInteger
     */
    function _blind($x, $r, $i)
    {
        $x = $x->multiply($r->modPow($this->publicExponent, $this->primes[$i]));
        $x = $x->modPow($this->exponents[$i], $this->primes[$i]);

        $r = $r->modInverse($this->primes[$i]);
        $x = $x->multiply($r);
        list(, $x) = $x->divide($this->primes[$i]);

        return $x;
    }

    /**
     * RSAEP
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-5.1.1 RFC3447#section-5.1.1}.
     *
     * @access private
     * @param Math_BigInteger $m
     * @return Math_BigInteger
     */
    function _rsaep($m)
    {
        if ($m->compare($this->zero) < 0 || $m->compare($this->modulus) > 0) {
            user_error('Message representative out of range', E_USER_NOTICE);
            return false;
        }
        return $this->_exponentiate($m);
    }

    /**
     * RSADP
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-5.1.2 RFC3447#section-5.1.2}.
     *
     * @access private
     * @param Math_BigInteger $c
     * @return Math_BigInteger
     */
    function _rsadp($c)
    {
        if ($c->compare($this->zero) < 0 || $c->compare($this->modulus) > 0) {
            user_error('Ciphertext representative out of range', E_USER_NOTICE);
            return false;
        }
        return $this->_exponentiate($c);
    }

    /**
     * RSASP1
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-5.2.1 RFC3447#section-5.2.1}.
     *
     * @access private
     * @param Math_BigInteger $m
     * @return Math_BigInteger
     */
    function _rsasp1($m)
    {
        if ($m->compare($this->zero) < 0 || $m->compare($this->modulus) > 0) {
            user_error('Message representative out of range', E_USER_NOTICE);
            return false;
        }
        return $this->_exponentiate($m);
    }

    /**
     * RSAVP1
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-5.2.2 RFC3447#section-5.2.2}.
     *
     * @access private
     * @param Math_BigInteger $s
     * @return Math_BigInteger
     */
    function _rsavp1($s)
    {
        if ($s->compare($this->zero) < 0 || $s->compare($this->modulus) > 0) {
            user_error('Signature representative out of range', E_USER_NOTICE);
            return false;
        }
        return $this->_exponentiate($s);
    }

    /**
     * MGF1
     *
     * See {@link http://tools.ietf.org/html/rfc3447#appendix-B.2.1 RFC3447#appendix-B.2.1}.
     *
     * @access private
     * @param String $mgfSeed
     * @param Integer $mgfLen
     * @return String
     */
    function _mgf1($mgfSeed, $maskLen)
    {
        // if $maskLen would yield strings larger than 4GB, PKCS#1 suggests a "Mask too long" error be output.

        $t = '';
        $count = ceil($maskLen / $this->mgfHLen);
        for ($i = 0; $i < $count; $i++) {
            $c = pack('N', $i);
            $t.= $this->mgfHash->hash($mgfSeed . $c);
        }

        return substr($t, 0, $maskLen);
    }

    /**
     * RSAES-OAEP-ENCRYPT
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-7.1.1 RFC3447#section-7.1.1} and
     * {http://en.wikipedia.org/wiki/Optimal_Asymmetric_Encryption_Padding OAES}.
     *
     * @access private
     * @param String $m
     * @param String $l
     * @return String
     */
    function _rsaes_oaep_encrypt($m, $l = '')
    {
        $mLen = strlen($m);

        // Length checking

        // if $l is larger than two million terrabytes and you're using sha1, PKCS#1 suggests a "Label too long" error
        // be output.

        if ($mLen > $this->k - 2 * $this->hLen - 2) {
            user_error('Message too long', E_USER_NOTICE);
            return false;
        }

        // EME-OAEP encoding

        $lHash = $this->hash->hash($l);
        $ps = str_repeat(chr(0), $this->k - $mLen - 2 * $this->hLen - 2);
        $db = $lHash . $ps . chr(1) . $m;
        $seed = $this->_random($this->hLen);
        $dbMask = $this->_mgf1($seed, $this->k - $this->hLen - 1);
        $maskedDB = $db ^ $dbMask;
        $seedMask = $this->_mgf1($maskedDB, $this->hLen);
        $maskedSeed = $seed ^ $seedMask;
        $em = chr(0) . $maskedSeed . $maskedDB;

        // RSA encryption

        $m = $this->_os2ip($em);
        $c = $this->_rsaep($m);
        $c = $this->_i2osp($c, $this->k);

        // Output the ciphertext C

        return $c;
    }

    /**
     * RSAES-OAEP-DECRYPT
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-7.1.2 RFC3447#section-7.1.2}.  The fact that the error
     * messages aren't distinguishable from one another hinders debugging, but, to quote from RFC3447#section-7.1.2:
     *
     *    Note.  Care must be taken to ensure that an opponent cannot
     *    distinguish the different error conditions in Step 3.g, whether by
     *    error message or timing, or, more generally, learn partial
     *    information about the encoded message EM.  Otherwise an opponent may
     *    be able to obtain useful information about the decryption of the
     *    ciphertext C, leading to a chosen-ciphertext attack such as the one
     *    observed by Manger [36].
     *
     * As for $l...  to quote from {@link http://tools.ietf.org/html/rfc3447#page-17 RFC3447#page-17}:
     *
     *    Both the encryption and the decryption operations of RSAES-OAEP take
     *    the value of a label L as input.  In this version of PKCS #1, L is
     *    the empty string; other uses of the label are outside the scope of
     *    this document.
     *
     * @access private
     * @param String $c
     * @param String $l
     * @return String
     */
    function _rsaes_oaep_decrypt($c, $l = '')
    {
        // Length checking

        // if $l is larger than two million terrabytes and you're using sha1, PKCS#1 suggests a "Label too long" error
        // be output.

        if (strlen($c) != $this->k || $this->k < 2 * $this->hLen + 2) {
            user_error('Decryption error', E_USER_NOTICE);
            return false;
        }

        // RSA decryption

        $c = $this->_os2ip($c);
        $m = $this->_rsadp($c);
        if ($m === false) {
            user_error('Decryption error', E_USER_NOTICE);
            return false;
        }
        $em = $this->_i2osp($m, $this->k);

        // EME-OAEP decoding

        $lHash = $this->hash->hash($l);
        $y = ord($em[0]);
        $maskedSeed = substr($em, 1, $this->hLen);
        $maskedDB = substr($em, $this->hLen + 1);
        $seedMask = $this->_mgf1($maskedDB, $this->hLen);
        $seed = $maskedSeed ^ $seedMask;
        $dbMask = $this->_mgf1($seed, $this->k - $this->hLen - 1);
        $db = $maskedDB ^ $dbMask;
        $lHash2 = substr($db, 0, $this->hLen);
        $m = substr($db, $this->hLen);
        if ($lHash != $lHash2) {
            user_error('Decryption error', E_USER_NOTICE);
            return false;
        }
        $m = ltrim($m, chr(0));
        if (ord($m[0]) != 1) {
            user_error('Decryption error', E_USER_NOTICE);
            return false;
        }

        // Output the message M

        return substr($m, 1);
    }

    /**
     * RSAES-PKCS1-V1_5-ENCRYPT
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-7.2.1 RFC3447#section-7.2.1}.
     *
     * @access private
     * @param String $m
     * @return String
     */
    function _rsaes_pkcs1_v1_5_encrypt($m)
    {
        $mLen = strlen($m);

        // Length checking

        if ($mLen > $this->k - 11) {
            user_error('Message too long', E_USER_NOTICE);
            return false;
        }

        // EME-PKCS1-v1_5 encoding

        $ps = $this->_random($this->k - $mLen - 3, true);
        $em = chr(0) . chr(2) . $ps . chr(0) . $m;

        // RSA encryption
        $m = $this->_os2ip($em);
        $c = $this->_rsaep($m);
        $c = $this->_i2osp($c, $this->k);

        // Output the ciphertext C

        return $c;
    }

    /**
     * RSAES-PKCS1-V1_5-DECRYPT
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-7.2.2 RFC3447#section-7.2.2}.
     *
     * For compatability purposes, this function departs slightly from the description given in RFC3447.
     * The reason being that RFC2313#section-8.1 (PKCS#1 v1.5) states that ciphertext's encrypted by the
     * private key should have the second byte set to either 0 or 1 and that ciphertext's encrypted by the
     * public key should have the second byte set to 2.  In RFC3447 (PKCS#1 v2.1), the second byte is supposed
     * to be 2 regardless of which key is used.  for compatability purposes, we'll just check to make sure the
     * second byte is 2 or less.  If it is, we'll accept the decrypted string as valid.
     *
     * As a consequence of this, a private key encrypted ciphertext produced with Crypt_RSA may not decrypt
     * with a strictly PKCS#1 v1.5 compliant RSA implementation.  Public key encrypted ciphertext's should but
     * not private key encrypted ciphertext's.
     *
     * @access private
     * @param String $c
     * @return String
     */
    function _rsaes_pkcs1_v1_5_decrypt($c)
    {
        // Length checking

        if (strlen($c) != $this->k) { // or if k < 11
            user_error('Decryption error', E_USER_NOTICE);
            return false;
        }

        // RSA decryption

        $c = $this->_os2ip($c);
        $m = $this->_rsadp($c);

        if ($m === false) {
            user_error('Decryption error', E_USER_NOTICE);
            return false;
        }
        $em = $this->_i2osp($m, $this->k);

        // EME-PKCS1-v1_5 decoding

        if (ord($em[0]) != 0 || ord($em[1]) > 2) {
            user_error('Decryption error', E_USER_NOTICE);
            return false;
        }

        $ps = substr($em, 2, strpos($em, chr(0), 2) - 2);
        $m = substr($em, strlen($ps) + 3);

        if (strlen($ps) < 8) {
            user_error('Decryption error', E_USER_NOTICE);
            return false;
        }

        // Output M

        return $m;
    }

    /**
     * EMSA-PSS-ENCODE
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-9.1.1 RFC3447#section-9.1.1}.
     *
     * @access private
     * @param String $m
     * @param Integer $emBits
     */
    function _emsa_pss_encode($m, $emBits)
    {
        // if $m is larger than two million terrabytes and you're using sha1, PKCS#1 suggests a "Label too long" error
        // be output.

        $emLen = ($emBits + 1) >> 3; // ie. ceil($emBits / 8)
        $sLen = $this->sLen == false ? $this->hLen : $this->sLen;

        $mHash = $this->hash->hash($m);
        if ($emLen < $this->hLen + $sLen + 2) {
            user_error('Encoding error', E_USER_NOTICE);
            return false;
        }

        $salt = $this->_random($sLen);
        $m2 = "\0\0\0\0\0\0\0\0" . $mHash . $salt;
        $h = $this->hash->hash($m2);
        $ps = str_repeat(chr(0), $emLen - $sLen - $this->hLen - 2);
        $db = $ps . chr(1) . $salt;
        $dbMask = $this->_mgf1($h, $emLen - $this->hLen - 1);
        $maskedDB = $db ^ $dbMask;
        $maskedDB[0] = ~chr(0xFF << ($emBits & 7)) & $maskedDB[0];
        $em = $maskedDB . $h . chr(0xBC);

        return $em;
    }

    /**
     * EMSA-PSS-VERIFY
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-9.1.2 RFC3447#section-9.1.2}.
     *
     * @access private
     * @param String $m
     * @param String $em
     * @param Integer $emBits
     * @return String
     */
    function _emsa_pss_verify($m, $em, $emBits)
    {
        // if $m is larger than two million terrabytes and you're using sha1, PKCS#1 suggests a "Label too long" error
        // be output.

        $emLen = ($emBits + 1) >> 3; // ie. ceil($emBits / 8);
        $sLen = $this->sLen == false ? $this->hLen : $this->sLen;

        $mHash = $this->hash->hash($m);
        if ($emLen < $this->hLen + $sLen + 2) {
            return false;
        }

        if ($em[strlen($em) - 1] != chr(0xBC)) {
            return false;
        }

        $maskedDB = substr($em, 0, -$this->hLen - 1);
        $h = substr($em, -$this->hLen - 1, $this->hLen);
        $temp = chr(0xFF << ($emBits & 7));
        if ((~$maskedDB[0] & $temp) != $temp) {
            return false;
        }
        $dbMask = $this->_mgf1($h, $emLen - $this->hLen - 1);
        $db = $maskedDB ^ $dbMask;
        $db[0] = ~chr(0xFF << ($emBits & 7)) & $db[0];
        $temp = $emLen - $this->hLen - $sLen - 2;
        if (substr($db, 0, $temp) != str_repeat(chr(0), $temp) || ord($db[$temp]) != 1) {
            return false;
        }
        $salt = substr($db, $temp + 1); // should be $sLen long
        $m2 = "\0\0\0\0\0\0\0\0" . $mHash . $salt;
        $h2 = $this->hash->hash($m2);
        return $h == $h2;
    }

    /**
     * RSASSA-PSS-SIGN
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-8.1.1 RFC3447#section-8.1.1}.
     *
     * @access private
     * @param String $m
     * @return String
     */
    function _rsassa_pss_sign($m)
    {
        // EMSA-PSS encoding

        $em = $this->_emsa_pss_encode($m, 8 * $this->k - 1);

        // RSA signature

        $m = $this->_os2ip($em);
        $s = $this->_rsasp1($m);
        $s = $this->_i2osp($s, $this->k);

        // Output the signature S

        return $s;
    }

    /**
     * RSASSA-PSS-VERIFY
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-8.1.2 RFC3447#section-8.1.2}.
     *
     * @access private
     * @param String $m
     * @param String $s
     * @return String
     */
    function _rsassa_pss_verify($m, $s)
    {
        // Length checking

        if (strlen($s) != $this->k) {
            user_error('Invalid signature', E_USER_NOTICE);
            return false;
        }

        // RSA verification

        $modBits = 8 * $this->k;

        $s2 = $this->_os2ip($s);
        $m2 = $this->_rsavp1($s2);
        if ($m2 === false) {
            user_error('Invalid signature', E_USER_NOTICE);
            return false;
        }
        $em = $this->_i2osp($m2, $modBits >> 3);
        if ($em === false) {
            user_error('Invalid signature', E_USER_NOTICE);
            return false;
        }

        // EMSA-PSS verification

        return $this->_emsa_pss_verify($m, $em, $modBits - 1);
    }

    /**
     * EMSA-PKCS1-V1_5-ENCODE
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-9.2 RFC3447#section-9.2}.
     *
     * @access private
     * @param String $m
     * @param Integer $emLen
     * @return String
     */
    function _emsa_pkcs1_v1_5_encode($m, $emLen)
    {
        $h = $this->hash->hash($m);
        if ($h === false) {
            return false;
        }

        // see http://tools.ietf.org/html/rfc3447#page-43
        switch ($this->hashName) {
            case 'md2':
                $t = pack('H*', '3020300c06082a864886f70d020205000410');
                break;
            case 'md5':
                $t = pack('H*', '3020300c06082a864886f70d020505000410');
                break;
            case 'sha1':
                $t = pack('H*', '3021300906052b0e03021a05000414');
                break;
            case 'sha256':
                $t = pack('H*', '3031300d060960864801650304020105000420');
                break;
            case 'sha384':
                $t = pack('H*', '3041300d060960864801650304020205000430');
                break;
            case 'sha512':
                $t = pack('H*', '3051300d060960864801650304020305000440');
        }
        $t.= $h;
        $tLen = strlen($t);

        if ($emLen < $tLen + 11) {
            user_error('Intended encoded message length too short', E_USER_NOTICE);
            return false;
        }

        $ps = str_repeat(chr(0xFF), $emLen - $tLen - 3);

        $em = "\0\1$ps\0$t";

        return $em;
    }

    /**
     * RSASSA-PKCS1-V1_5-SIGN
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-8.2.1 RFC3447#section-8.2.1}.
     *
     * @access private
     * @param String $m
     * @return String
     */
    function _rsassa_pkcs1_v1_5_sign($m)
    {
        // EMSA-PKCS1-v1_5 encoding

        $em = $this->_emsa_pkcs1_v1_5_encode($m, $this->k);
        if ($em === false) {
            user_error('RSA modulus too short', E_USER_NOTICE);
            return false;
        }

        // RSA signature

        $m = $this->_os2ip($em);
        $s = $this->_rsasp1($m);
        $s = $this->_i2osp($s, $this->k);

        // Output the signature S

        return $s;
    }

    /**
     * RSASSA-PKCS1-V1_5-VERIFY
     *
     * See {@link http://tools.ietf.org/html/rfc3447#section-8.2.2 RFC3447#section-8.2.2}.
     *
     * @access private
     * @param String $m
     * @return String
     */
    function _rsassa_pkcs1_v1_5_verify($m, $s)
    {
        // Length checking

        if (strlen($s) != $this->k) {
            user_error('Invalid signature', E_USER_NOTICE);
            return false;
        }

        // RSA verification

        $s = $this->_os2ip($s);
        $m2 = $this->_rsavp1($s);
        if ($m2 === false) {
            user_error('Invalid signature', E_USER_NOTICE);
            return false;
        }
        $em = $this->_i2osp($m2, $this->k);
        if ($em === false) {
            user_error('Invalid signature', E_USER_NOTICE);
            return false;
        }

        // EMSA-PKCS1-v1_5 encoding

        $em2 = $this->_emsa_pkcs1_v1_5_encode($m, $this->k);
        if ($em2 === false) {
            user_error('RSA modulus too short', E_USER_NOTICE);
            return false;
        }

        // Compare

        return $em === $em2;
    }

    /**
     * Set Encryption Mode
     *
     * Valid values include CRYPT_RSA_ENCRYPTION_OAEP and CRYPT_RSA_ENCRYPTION_PKCS1.
     *
     * @access public
     * @param Integer $mode
     */
    function setEncryptionMode($mode)
    {
        $this->encryptionMode = $mode;
    }

    /**
     * Set Signature Mode
     *
     * Valid values include CRYPT_RSA_SIGNATURE_PSS and CRYPT_RSA_SIGNATURE_PKCS1
     *
     * @access public
     * @param Integer $mode
     */
    function setSignatureMode($mode)
    {
        $this->signatureMode = $mode;
    }

    /**
     * Encryption
     *
     * Both CRYPT_RSA_ENCRYPTION_OAEP and CRYPT_RSA_ENCRYPTION_PKCS1 both place limits on how long $plaintext can be.
     * If $plaintext exceeds those limits it will be broken up so that it does and the resultant ciphertext's will
     * be concatenated together.
     *
     * @see decrypt()
     * @access public
     * @param String $plaintext
     * @return String
     */
    function encrypt($plaintext)
    {
        switch ($this->encryptionMode) {
            case CRYPT_RSA_ENCRYPTION_PKCS1:
                $length = $this->k - 11;
                if ($length <= 0) {
                    return false;
                }

                $plaintext = str_split($plaintext, $length);
                $ciphertext = '';
                foreach ($plaintext as $m) {
                    $ciphertext.= $this->_rsaes_pkcs1_v1_5_encrypt($m);
                }
                return $ciphertext;
            //case CRYPT_RSA_ENCRYPTION_OAEP:
            default:
                $length = $this->k - 2 * $this->hLen - 2;
                if ($length <= 0) {
                    return false;
                }

                $plaintext = str_split($plaintext, $length);
                $ciphertext = '';
                foreach ($plaintext as $m) {
                    $ciphertext.= $this->_rsaes_oaep_encrypt($m);
                }
                return $ciphertext;
        }
    }

    /**
     * Decryption
     *
     * @see encrypt()
     * @access public
     * @param String $plaintext
     * @return String
     */
    function decrypt($ciphertext)
    {
        if ($this->k <= 0) {
            return false;
        }

        $ciphertext = str_split($ciphertext, $this->k);
        $plaintext = '';

        switch ($this->encryptionMode) {
            case CRYPT_RSA_ENCRYPTION_PKCS1:
                $decrypt = '_rsaes_pkcs1_v1_5_decrypt';
                break;
            //case CRYPT_RSA_ENCRYPTION_OAEP:
            default:
                $decrypt = '_rsaes_oaep_decrypt';
        }

        foreach ($ciphertext as $c) {
            $temp = $this->$decrypt($c);
            if ($temp === false) {
                return false;
            }
            $plaintext.= $temp;
        }

        return $plaintext;
    }

    /**
     * Create a signature
     *
     * @see verify()
     * @access public
     * @param String $message
     * @return String
     */
    function sign($message)
    {
        if (empty($this->modulus) || empty($this->exponent)) {
            return false;
        }

        switch ($this->signatureMode) {
            case CRYPT_RSA_SIGNATURE_PKCS1:
                return $this->_rsassa_pkcs1_v1_5_sign($message);
            //case CRYPT_RSA_SIGNATURE_PSS:
            default:
                return $this->_rsassa_pss_sign($message);
        }
    }

    /**
     * Verifies a signature
     *
     * @see sign()
     * @access public
     * @param String $message
     * @param String $signature
     * @return Boolean
     */
    function verify($message, $signature)
    {
        if (empty($this->modulus) || empty($this->exponent)) {
            return false;
        }

        switch ($this->signatureMode) {
            case CRYPT_RSA_SIGNATURE_PKCS1:
                return $this->_rsassa_pkcs1_v1_5_verify($message, $signature);
            //case CRYPT_RSA_SIGNATURE_PSS:
            default:
                return $this->_rsassa_pss_verify($message, $signature);
        }
    }
}
