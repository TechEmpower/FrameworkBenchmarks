<?php
/** @package    verysimple::Authentication */

/**
 * Bcrypt abstracts the use of bcrypt for secure password hashing.
 * The verify method is backwards compatible with any algorithm
 * supported by php crypt function
 *
 * @author http://stackoverflow.com/questions/4795385/how-do-you-use-bcrypt-for-hashing-passwords-in-php
 * @author J.Hinkle http://verysimple.com/
 * @example <code>
 * $bcrypt = new Bcrypt(15);
 * $hash = $bcrypt->hash('password');
 * $isGood = $bcrypt->verify('password', $hash);
 * </code>
 */
class Bcrypt
{
	private $rounds;
	private $randomState;

	/**
	 * Constructor
	 * @param int number of crypt rounds
	 * @throws Exception if bcrypt is not supported
	 */
	public function __construct($rounds = 12) {
		if(CRYPT_BLOWFISH != 1) {
			throw new Exception("bcrypt not supported in this installation. See http://php.net/crypt");
		}

		$this->rounds = $rounds;
	}

	/**
	 * Return true if the given hash is crypted with the blowfish algorithm
	 * @param string $hash
	 */
	static function isBlowfish($hash)
	{
		return substr($hash,0,4) == '$2a$';
	}

	/**
	 * generate a hash
	 * @param string plain text input
	 */
	public function hash($input) {
		$hash = crypt($input, $this->getSalt());

		if(strlen($hash) > 13)
			return $hash;

		return false;
	}

	/**
	 * Verify if the input is equal to the hashed value
	 * @param string plain text input
	 * @param string hashed input
	 */
	public function verify($input, $existingHash) {
		$hash = crypt($input, $existingHash);

		return $hash === $existingHash;
	}

	/**
	 * return an ascii-encoded 16 char salt
	 */
	private function getSalt() {
		$salt = sprintf('$2a$%02d$', $this->rounds);

		$bytes = $this->getRandomBytes(16);

		$salt .= $this->encodeBytes($bytes);

		return $salt;
	}

	/**
	 * get random bytes to be used in random salts
	 * @param int $count
	 */
	private function getRandomBytes($count) {
		$bytes = '';

		if(function_exists('openssl_random_pseudo_bytes') &&
				(strtoupper(substr(PHP_OS, 0, 3)) !== 'WIN')) { // OpenSSL slow on Win
			$bytes = openssl_random_pseudo_bytes($count);
		}

		if($bytes === '' && is_readable('/dev/urandom') &&
				($hRand = @fopen('/dev/urandom', 'rb')) !== FALSE) {
			$bytes = fread($hRand, $count);
			fclose($hRand);
		}

		if(strlen($bytes) < $count) {
			$bytes = '';

			if($this->randomState === null) {
				$this->randomState = microtime();
				if(function_exists('getmypid')) {
					$this->randomState .= getmypid();
				}
			}

			for($i = 0; $i < $count; $i += 16) {
				$this->randomState = md5(microtime() . $this->randomState);

				if (PHP_VERSION >= '5') {
					$bytes .= md5($this->randomState, true);
				} else {
					$bytes .= pack('H*', md5($this->randomState));
				}
			}

			$bytes = substr($bytes, 0, $count);
		}

		return $bytes;
	}

	/**
	 * ascii-encode used for converting random salt into legit ascii value
	 * @param string $input
	 */
	private function encodeBytes($input) {
		// The following is code from the PHP Password Hashing Framework
		$itoa64 = './ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';

		$output = '';
		$i = 0;
		do {
			$c1 = ord($input[$i++]);
			$output .= $itoa64[$c1 >> 2];
			$c1 = ($c1 & 0x03) << 4;
			if ($i >= 16) {
				$output .= $itoa64[$c1];
				break;
			}

			$c2 = ord($input[$i++]);
			$c1 |= $c2 >> 4;
			$output .= $itoa64[$c1];
			$c1 = ($c2 & 0x0f) << 2;

			$c2 = ord($input[$i++]);
			$c1 |= $c2 >> 6;
			$output .= $itoa64[$c1];
			$output .= $itoa64[$c2 & 0x3f];
		} while (1);

		return $output;
	}
}
?>