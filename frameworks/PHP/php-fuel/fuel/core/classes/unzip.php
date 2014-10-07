<?php
/**
 * Part of the Fuel framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

namespace Fuel\Core;



/**
 * UnZip Class
 *
 * This class is based on a library I found at PHPClasses:
 * http://phpclasses.org/package/2495-PHP-Pack-and-unpack-files-packed-in-ZIP-archives.html
 *
 * The original library is a little rough around the edges so I
 * refactored it and added several additional methods -- Phil Sturgeon
 *
 * This class requires extension ZLib Enabled.
 *
 * @package		Fuel
 * @subpackage	Core
 * @category	Encryption
 * @author		Alexandre Tedeschi
 * @author		Phil Sturgeon
 * @license
 * @version     1.0.0
 */
class Unzip
{

	private $compressed_list = array();

	// List of files in the ZIP
	private $central_dir_list = array();

	// Central dir list... It's a kind of 'extra attributes' for a set of files
	private $end_of_central = array();

	// End of central dir, contains ZIP Comments
	private $info = array();
	private $error = array();
	private $_zip_file = '';
	private $_target_dir = false;
	private $apply_chmod = 0777;
	private $fh;
	private $zip_signature = "\x50\x4b\x03\x04";

	// local file header signature
	private $dir_signature = "\x50\x4b\x01\x02";

	// central dir header signature
	private $central_signature_end = "\x50\x4b\x05\x06";

	// ignore these directories (useless meta data)
	private $_skip_dirs = array('__MACOSX');

	private $_allow_extensions = NULL; // What is allowed out of the zip

	// --------------------------------------------------------------------

	/**
	 * Unzip all files in archive.
	 *
	 * @access    Public
	 * @param     none
	 * @return    none
	 */
	public function extract($zip_file, $target_dir = NULL, $preserve_filepath = TRUE)
	{
		$this->_zip_file = $zip_file;
		$this->_target_dir = $target_dir ? $target_dir : dirname($this->_zip_file);

		if ( ! $files = $this->_list_files())
		{
			throw new \FuelException('ZIP folder was empty.');
			return false;
		}

		$file_locations = array();
		foreach ($files as $file => $trash)
		{
			$dirname = pathinfo($file, PATHINFO_DIRNAME);
			$extension = pathinfo($file, PATHINFO_EXTENSION);

			$folders = explode('/', $dirname);
			$out_dn = $this->_target_dir . '/' . $dirname;

			// Skip stuff in stupid folders
			if (in_array(current($folders), $this->_skip_dirs))
			{
				continue;
			}

			// Skip any files that are not allowed
			if (is_array($this->_allow_extensions) AND $extension AND ! in_array($extension, $this->_allow_extensions))
			{
				continue;
			}

			if ( ! is_dir($out_dn) AND $preserve_filepath)
			{
				$str = "";
				foreach ($folders as $folder)
				{
					$str = $str ? $str . '/' . $folder : $folder;
					if ( ! is_dir($this->_target_dir . '/' . $str))
					{
						$this->set_debug('Creating folder: ' . $this->_target_dir . '/' . $str);

						if ( ! @mkdir($this->_target_dir . '/' . $str))
						{
							throw new \FuelException('Desitnation path is not writable.');
							return false;
						}

						// Apply chmod if configured to do so
						$this->apply_chmod AND chmod($this->_target_dir . '/' . $str, $this->apply_chmod);
					}
				}
			}

			if (substr($file, -1, 1) == '/') continue;

			$file_locations[] = $file_location = $this->_target_dir . '/' . ($preserve_filepath ? $file : basename($file));

			$this->_extract_file($file, $file_location);
		}

		return $file_locations;
	}

	// --------------------------------------------------------------------

	/**
	 * What extensions do we want out of this ZIP
	 *
	 * @access    Public
	 * @param     none
	 * @return    none
	 */
	public function allow($ext = NULL)
	{
		$this->_allow_extensions = $ext;
	}

	// --------------------------------------------------------------------

	/**
	 * Show error messages
	 *
	 * @access    public
	 * @param    string
	 * @return    string
	 */
	public function error_string($open = '<p>', $close = '</p>')
	{
		return $open . implode($close . $open, $this->error) . $close;
	}

	// --------------------------------------------------------------------

	/**
	 * Show debug messages
	 *
	 * @access    public
	 * @param    string
	 * @return    string
	 */
	public function debug_string($open = '<p>', $close = '</p>')
	{
		return $open . implode($close . $open, $this->info) . $close;
	}

	// --------------------------------------------------------------------

	/**
	 * Save errors
	 *
	 * @access    Private
	 * @param    string
	 * @return    none
	 */
	function set_error($string)
	{
		$this->error[] = $string;
	}

	// --------------------------------------------------------------------

	/**
	 * Save debug data
	 *
	 * @access    Private
	 * @param    string
	 * @return    none
	 */
	function set_debug($string)
	{
		$this->info[] = $string;
	}

	// --------------------------------------------------------------------

	/**
	 * List all files in archive.
	 *
	 * @access    Public
	 * @param     boolean
	 * @return    mixed
	 */
	private function _list_files($stop_on_file = false)
	{
		if (sizeof($this->compressed_list))
		{
			$this->set_debug('Returning already loaded file list.');
			return $this->compressed_list;
		}

		// Open file, and set file handler
		$fh = fopen($this->_zip_file, 'r');
		$this->fh = &$fh;

		if ( ! $fh)
		{
			throw new \FuelException('Failed to load file: ' . $this->_zip_file);
			return false;
		}

		$this->set_debug('Loading list from "End of Central Dir" index list...');

		if ( ! $this->_load_file_list_by_eof($fh, $stop_on_file))
		{
			$this->set_debug('Failed! Trying to load list looking for signatures...');

			if ( ! $this->_load_files_by_signatures($fh, $stop_on_file))
			{
				$this->set_debug('Failed! Could not find any valid header.');
				throw new \FuelException('ZIP File is corrupted or empty');

				return false;
			}
		}

		return $this->compressed_list;
	}

	// --------------------------------------------------------------------

	/**
	 * Unzip file in archive.
	 *
	 * @access    Public
	 * @param     string, boolean
	 * @return    Unziped file.
	 */
	private function _extract_file($compressed_file_name, $target_file_name = false)
	{
		if ( ! sizeof($this->compressed_list))
		{
			$this->set_debug('Trying to unzip before loading file list... Loading it!');
			$this->_list_files(false, $compressed_file_name);
		}

		$fdetails = &$this->compressed_list[$compressed_file_name];

		if ( ! isset($this->compressed_list[$compressed_file_name]))
		{
			throw new \FuelException('File "<strong>' . $compressed_file_name . '</strong>" is not compressed in the zip.');
			return false;
		}

		if (substr($compressed_file_name, -1) == '/')
		{
			throw new \FuelException('Trying to unzip a folder name "<strong>' . $compressed_file_name . '</strong>".');
			return false;
		}

		if ( ! $fdetails['uncompressed_size'])
		{
			$this->set_debug('File "<strong>' . $compressed_file_name . '</strong>" is empty.');

			return $target_file_name ? file_put_contents($target_file_name, '') : '';
		}

		fseek($this->fh, $fdetails['contents_start_offset']);
		$ret = $this->_uncompress(
			fread($this->fh, $fdetails['compressed_size']),
			$fdetails['compression_method'],
			$fdetails['uncompressed_size'],
			$target_file_name
		);

		if ($this->apply_chmod AND $target_file_name)
		{
			chmod($target_file_name, 0644);
		}

		return $ret;
	}

	// --------------------------------------------------------------------

	/**
	 * Free the file resource.
	 *
	 * @access    Public
	 * @param     none
	 * @return    none
	 */
	public function close()
	{
		// Free the file resource
		if ($this->fh)
		{
			fclose($this->fh);
		}
	}

	// --------------------------------------------------------------------

	/**
	 * Free the file resource Automatic destroy.
	 *
	 * @access    Public
	 * @param     none
	 * @return    none
	 */
	public function __destroy()
	{
		$this->close();
	}

	// --------------------------------------------------------------------

	/**
	 * Uncompress file. And save it to the targetFile.
	 *
	 * @access    Private
	 * @param     Filecontent, int, int, boolean
	 * @return    none
	 */
	private function _uncompress($content, $mode, $uncompressed_size, $target_file_name = false)
	{
		switch ($mode)
		{
			case 0:
				return $target_file_name ? file_put_contents($target_file_name, $content) : $content;
			case 1:
				throw new \FuelException('Shrunk mode is not supported... yet?');
				return false;
			case 2:
			case 3:
			case 4:
			case 5:
				throw new \FuelException('Compression factor ' . ($mode - 1) . ' is not supported... yet?');
				return false;
			case 6:
				throw new \FuelException('Implode is not supported... yet?');
				return false;
			case 7:
				throw new \FuelException('Tokenizing compression algorithm is not supported... yet?');
				return false;
			case 8:
				// Deflate
				return $target_file_name ?
						file_put_contents($target_file_name, gzinflate($content, $uncompressed_size)) :
						gzinflate($content, $uncompressed_size);
			case 9:
				throw new \FuelException('Enhanced Deflating is not supported... yet?');
				return false;
			case 10:
				throw new \FuelException('PKWARE Date Compression Library Impoloding is not supported... yet?');
				return false;
			case 12:
				// Bzip2
				return $target_file_name ?
						file_put_contents($target_file_name, bzdecompress($content)) :
						bzdecompress($content);
			case 18:
				throw new \FuelException('IBM TERSE is not supported... yet?');
				return false;
			default:
				throw new \FuelException('Unknown uncompress method: $mode');
				return false;
		}
	}

	private function _load_file_list_by_eof(&$fh, $stop_on_file = false)
	{
		// Check if there's a valid Central Dir signature.
		// Let's consider a file comment smaller than 1024 characters...
		// Actually, it length can be 65536.. But we're not going to support it.

		for ($x = 0; $x < 1024; $x++)
		{
			fseek($fh, -22 - $x, SEEK_END);

			$signature = fread($fh, 4);

			if ($signature == $this->central_signature_end)
			{
				// If found EOF Central Dir
				$eodir['disk_number_this'] = unpack("v", fread($fh, 2)); // number of this disk
				$eodir['disk_number'] = unpack("v", fread($fh, 2)); // number of the disk with the start of the central directory
				$eodir['total_entries_this'] = unpack("v", fread($fh, 2)); // total number of entries in the central dir on this disk
				$eodir['total_entries'] = unpack("v", fread($fh, 2)); // total number of entries in
				$eodir['size_of_cd'] = unpack("V", fread($fh, 4)); // size of the central directory
				$eodir['offset_start_cd'] = unpack("V", fread($fh, 4)); // offset of start of central directory with respect to the starting disk number
				$zip_comment_lenght = unpack("v", fread($fh, 2)); // zipfile comment length
				$eodir['zipfile_comment'] = $zip_comment_lenght[1] ? fread($fh, $zip_comment_lenght[1]) : ''; // zipfile comment

				$this->end_of_central = array(
					'disk_number_this' => $eodir['disk_number_this'][1],
					'disk_number' => $eodir['disk_number'][1],
					'total_entries_this' => $eodir['total_entries_this'][1],
					'total_entries' => $eodir['total_entries'][1],
					'size_of_cd' => $eodir['size_of_cd'][1],
					'offset_start_cd' => $eodir['offset_start_cd'][1],
					'zipfile_comment' => $eodir['zipfile_comment'],
				);

				// Then, load file list
				fseek($fh, $this->end_of_central['offset_start_cd']);
				$signature = fread($fh, 4);

				while ($signature == $this->dir_signature)
				{
					$dir['version_madeby'] = unpack("v", fread($fh, 2)); // version made by
					$dir['version_needed'] = unpack("v", fread($fh, 2)); // version needed to extract
					$dir['general_bit_flag'] = unpack("v", fread($fh, 2)); // general purpose bit flag
					$dir['compression_method'] = unpack("v", fread($fh, 2)); // compression method
					$dir['lastmod_time'] = unpack("v", fread($fh, 2)); // last mod file time
					$dir['lastmod_date'] = unpack("v", fread($fh, 2)); // last mod file date
					$dir['crc-32'] = fread($fh, 4);			  // crc-32
					$dir['compressed_size'] = unpack("V", fread($fh, 4)); // compressed size
					$dir['uncompressed_size'] = unpack("V", fread($fh, 4)); // uncompressed size
					$zip_file_length = unpack("v", fread($fh, 2)); // filename length
					$extra_field_length = unpack("v", fread($fh, 2)); // extra field length
					$fileCommentLength = unpack("v", fread($fh, 2)); // file comment length
					$dir['disk_number_start'] = unpack("v", fread($fh, 2)); // disk number start
					$dir['internal_attributes'] = unpack("v", fread($fh, 2)); // internal file attributes-byte1
					$dir['external_attributes1'] = unpack("v", fread($fh, 2)); // external file attributes-byte2
					$dir['external_attributes2'] = unpack("v", fread($fh, 2)); // external file attributes
					$dir['relative_offset'] = unpack("V", fread($fh, 4)); // relative offset of local header
					$dir['file_name'] = fread($fh, $zip_file_length[1]);							 // filename
					$dir['extra_field'] = $extra_field_length[1] ? fread($fh, $extra_field_length[1]) : ''; // extra field
					$dir['file_comment'] = $fileCommentLength[1] ? fread($fh, $fileCommentLength[1]) : ''; // file comment

					// Convert the date and time, from MS-DOS format to UNIX Timestamp
					$binary_mod_date = str_pad(decbin($dir['lastmod_date'][1]), 16, '0', STR_PAD_LEFT);
					$binary_mod_time = str_pad(decbin($dir['lastmod_time'][1]), 16, '0', STR_PAD_LEFT);
					$last_mod_year = bindec(substr($binary_mod_date, 0, 7)) + 1980;
					$last_mod_month = bindec(substr($binary_mod_date, 7, 4));
					$last_mod_day = bindec(substr($binary_mod_date, 11, 5));
					$last_mod_hour = bindec(substr($binary_mod_time, 0, 5));
					$last_mod_minute = bindec(substr($binary_mod_time, 5, 6));
					$last_mod_second = bindec(substr($binary_mod_time, 11, 5));

					$this->central_dir_list[$dir['file_name']] = array(
						'version_madeby' => $dir['version_madeby'][1],
						'version_needed' => $dir['version_needed'][1],
						'general_bit_flag' => str_pad(decbin($dir['general_bit_flag'][1]), 8, '0', STR_PAD_LEFT),
						'compression_method' => $dir['compression_method'][1],
						'lastmod_datetime' => mktime($last_mod_hour, $last_mod_minute, $last_mod_second, $last_mod_month, $last_mod_day, $last_mod_year),
						'crc-32' => str_pad(dechex(ord($dir['crc-32'][3])), 2, '0', STR_PAD_LEFT) .
						str_pad(dechex(ord($dir['crc-32'][2])), 2, '0', STR_PAD_LEFT) .
						str_pad(dechex(ord($dir['crc-32'][1])), 2, '0', STR_PAD_LEFT) .
						str_pad(dechex(ord($dir['crc-32'][0])), 2, '0', STR_PAD_LEFT),
						'compressed_size' => $dir['compressed_size'][1],
						'uncompressed_size' => $dir['uncompressed_size'][1],
						'disk_number_start' => $dir['disk_number_start'][1],
						'internal_attributes' => $dir['internal_attributes'][1],
						'external_attributes1' => $dir['external_attributes1'][1],
						'external_attributes2' => $dir['external_attributes2'][1],
						'relative_offset' => $dir['relative_offset'][1],
						'file_name' => $dir['file_name'],
						'extra_field' => $dir['extra_field'],
						'file_comment' => $dir['file_comment'],
					);

					$signature = fread($fh, 4);
				}

				// If loaded centralDirs, then try to identify the offsetPosition of the compressed data.
				if ($this->central_dir_list)
				{
					foreach ($this->central_dir_list as $filename => $details)
					{
						$i = $this->_get_file_header($fh, $details['relative_offset']);

						$this->compressed_list[$filename]['file_name'] = $filename;
						$this->compressed_list[$filename]['compression_method'] = $details['compression_method'];
						$this->compressed_list[$filename]['version_needed'] = $details['version_needed'];
						$this->compressed_list[$filename]['lastmod_datetime'] = $details['lastmod_datetime'];
						$this->compressed_list[$filename]['crc-32'] = $details['crc-32'];
						$this->compressed_list[$filename]['compressed_size'] = $details['compressed_size'];
						$this->compressed_list[$filename]['uncompressed_size'] = $details['uncompressed_size'];
						$this->compressed_list[$filename]['lastmod_datetime'] = $details['lastmod_datetime'];
						$this->compressed_list[$filename]['extra_field'] = $i['extra_field'];
						$this->compressed_list[$filename]['contents_start_offset'] = $i['contents_start_offset'];

						if (strtolower($stop_on_file) == strtolower($filename))
						{
							break;
						}
					}
				}

				return TRUE;
			}
		}
		return false;
	}

	private function _load_files_by_signatures(&$fh, $stop_on_file = false)
	{
		fseek($fh, 0);

		$return = false;
		for (;;)
		{
			$details = $this->_get_file_header($fh);

			if ( ! $details)
			{
				$this->set_debug('Invalid signature. Trying to verify if is old style Data Descriptor...');
				fseek($fh, 12 - 4, SEEK_CUR); // 12: Data descriptor - 4: Signature (that will be read again)
				$details = $this->_get_file_header($fh);
			}

			if ( ! $details)
			{
				$this->set_debug('Still invalid signature. Probably reached the end of the file.');
				break;
			}

			$filename = $details['file_name'];
			$this->compressed_list[$filename] = $details;
			$return = true;

			if (strtolower($stop_on_file) == strtolower($filename))
			{
				break;
			}
		}

		return $return;
	}

	private function _get_file_header(&$fh, $start_offset = false)
	{
		if ($start_offset !== false)
		{
			fseek($fh, $start_offset);
		}

		$signature = fread($fh, 4);

		if ($signature == $this->zip_signature)
		{
			// Get information about the zipped file
			$file['version_needed'] = unpack("v", fread($fh, 2)); // version needed to extract
			$file['general_bit_flag'] = unpack("v", fread($fh, 2)); // general purpose bit flag
			$file['compression_method'] = unpack("v", fread($fh, 2)); // compression method
			$file['lastmod_time'] = unpack("v", fread($fh, 2)); // last mod file time
			$file['lastmod_date'] = unpack("v", fread($fh, 2)); // last mod file date
			$file['crc-32'] = fread($fh, 4);			  // crc-32
			$file['compressed_size'] = unpack("V", fread($fh, 4)); // compressed size
			$file['uncompressed_size'] = unpack("V", fread($fh, 4)); // uncompressed size
			$zip_file_length = unpack("v", fread($fh, 2)); // filename length
			$extra_field_length = unpack("v", fread($fh, 2)); // extra field length
			$file['file_name'] = fread($fh, $zip_file_length[1]); // filename
			$file['extra_field'] = $extra_field_length[1] ? fread($fh, $extra_field_length[1]) : ''; // extra field
			$file['contents_start_offset'] = ftell($fh);

			// Bypass the whole compressed contents, and look for the next file
			fseek($fh, $file['compressed_size'][1], SEEK_CUR);

			// Convert the date and time, from MS-DOS format to UNIX Timestamp
			$binary_mod_date = str_pad(decbin($file['lastmod_date'][1]), 16, '0', STR_PAD_LEFT);
			$binary_mod_time = str_pad(decbin($file['lastmod_time'][1]), 16, '0', STR_PAD_LEFT);

			$last_mod_year = bindec(substr($binary_mod_date, 0, 7)) + 1980;
			$last_mod_month = bindec(substr($binary_mod_date, 7, 4));
			$last_mod_day = bindec(substr($binary_mod_date, 11, 5));
			$last_mod_hour = bindec(substr($binary_mod_time, 0, 5));
			$last_mod_minute = bindec(substr($binary_mod_time, 5, 6));
			$last_mod_second = bindec(substr($binary_mod_time, 11, 5));

			// Mount file table
			$i = array(
				'file_name' => $file['file_name'],
				'compression_method' => $file['compression_method'][1],
				'version_needed' => $file['version_needed'][1],
				'lastmod_datetime' => mktime($last_mod_hour, $last_mod_minute, $last_mod_second, $last_mod_month, $last_mod_day, $last_mod_year),
				'crc-32' => str_pad(dechex(ord($file['crc-32'][3])), 2, '0', STR_PAD_LEFT) .
				str_pad(dechex(ord($file['crc-32'][2])), 2, '0', STR_PAD_LEFT) .
				str_pad(dechex(ord($file['crc-32'][1])), 2, '0', STR_PAD_LEFT) .
				str_pad(dechex(ord($file['crc-32'][0])), 2, '0', STR_PAD_LEFT),
				'compressed_size' => $file['compressed_size'][1],
				'uncompressed_size' => $file['uncompressed_size'][1],
				'extra_field' => $file['extra_field'],
				'general_bit_flag' => str_pad(decbin($file['general_bit_flag'][1]), 8, '0', STR_PAD_LEFT),
				'contents_start_offset' => $file['contents_start_offset']
			);

			return $i;
		}

		return false;
	}
}


