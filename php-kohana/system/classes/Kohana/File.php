<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * File helper class.
 *
 * @package    Kohana
 * @category   Helpers
 * @author     Kohana Team
 * @copyright  (c) 2007-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_File {

	/**
	 * Attempt to get the mime type from a file. This method is horribly
	 * unreliable, due to PHP being horribly unreliable when it comes to
	 * determining the mime type of a file.
	 *
	 *     $mime = File::mime($file);
	 *
	 * @param   string  $filename   file name or path
	 * @return  string  mime type on success
	 * @return  FALSE   on failure
	 */
	public static function mime($filename)
	{
		// Get the complete path to the file
		$filename = realpath($filename);

		// Get the extension from the filename
		$extension = strtolower(pathinfo($filename, PATHINFO_EXTENSION));

		if (preg_match('/^(?:jpe?g|png|[gt]if|bmp|swf)$/', $extension))
		{
			// Use getimagesize() to find the mime type on images
			$file = getimagesize($filename);

			if (isset($file['mime']))
				return $file['mime'];
		}

		if (class_exists('finfo', FALSE))
		{
			if ($info = new finfo(defined('FILEINFO_MIME_TYPE') ? FILEINFO_MIME_TYPE : FILEINFO_MIME))
			{
				return $info->file($filename);
			}
		}

		if (ini_get('mime_magic.magicfile') AND function_exists('mime_content_type'))
		{
			// The mime_content_type function is only useful with a magic file
			return mime_content_type($filename);
		}

		if ( ! empty($extension))
		{
			return File::mime_by_ext($extension);
		}

		// Unable to find the mime-type
		return FALSE;
	}

	/**
	 * Return the mime type of an extension.
	 *
	 *     $mime = File::mime_by_ext('png'); // "image/png"
	 *
	 * @param   string  $extension  php, pdf, txt, etc
	 * @return  string  mime type on success
	 * @return  FALSE   on failure
	 */
	public static function mime_by_ext($extension)
	{
		// Load all of the mime types
		$mimes = Kohana::$config->load('mimes');

		return isset($mimes[$extension]) ? $mimes[$extension][0] : FALSE;
	}

	/**
	 * Lookup MIME types for a file
	 *
	 * @see Kohana_File::mime_by_ext()
	 * @param string $extension Extension to lookup
	 * @return array Array of MIMEs associated with the specified extension
	 */
	public static function mimes_by_ext($extension)
	{
		// Load all of the mime types
		$mimes = Kohana::$config->load('mimes');

		return isset($mimes[$extension]) ? ( (array) $mimes[$extension]) : array();
	}

	/**
	 * Lookup file extensions by MIME type
	 *
	 * @param   string  $type File MIME type
	 * @return  array   File extensions matching MIME type
	 */
	public static function exts_by_mime($type)
	{
		static $types = array();

		// Fill the static array
		if (empty($types))
		{
			foreach (Kohana::$config->load('mimes') as $ext => $mimes)
			{
				foreach ($mimes as $mime)
				{
					if ($mime == 'application/octet-stream')
					{
						// octet-stream is a generic binary
						continue;
					}

					if ( ! isset($types[$mime]))
					{
						$types[$mime] = array( (string) $ext);
					}
					elseif ( ! in_array($ext, $types[$mime]))
					{
						$types[$mime][] = (string) $ext;
					}
				}
			}
		}

		return isset($types[$type]) ? $types[$type] : FALSE;
	}

	/**
	 * Lookup a single file extension by MIME type.
	 *
	 * @param   string  $type  MIME type to lookup
	 * @return  mixed          First file extension matching or false
	 */
	public static function ext_by_mime($type)
	{
		return current(File::exts_by_mime($type));
	}

	/**
	 * Split a file into pieces matching a specific size. Used when you need to
	 * split large files into smaller pieces for easy transmission.
	 *
	 *     $count = File::split($file);
	 *
	 * @param   string  $filename   file to be split
	 * @param   integer $piece_size size, in MB, for each piece to be
	 * @return  integer The number of pieces that were created
	 */
	public static function split($filename, $piece_size = 10)
	{
		// Open the input file
		$file = fopen($filename, 'rb');

		// Change the piece size to bytes
		$piece_size = floor($piece_size * 1024 * 1024);

		// Write files in 8k blocks
		$block_size = 1024 * 8;

		// Total number of peices
		$peices = 0;

		while ( ! feof($file))
		{
			// Create another piece
			$peices += 1;

			// Create a new file piece
			$piece = str_pad($peices, 3, '0', STR_PAD_LEFT);
			$piece = fopen($filename.'.'.$piece, 'wb+');

			// Number of bytes read
			$read = 0;

			do
			{
				// Transfer the data in blocks
				fwrite($piece, fread($file, $block_size));

				// Another block has been read
				$read += $block_size;
			}
			while ($read < $piece_size);

			// Close the piece
			fclose($piece);
		}

		// Close the file
		fclose($file);

		return $peices;
	}

	/**
	 * Join a split file into a whole file. Does the reverse of [File::split].
	 *
	 *     $count = File::join($file);
	 *
	 * @param   string  $filename   split filename, without .000 extension
	 * @return  integer The number of pieces that were joined.
	 */
	public static function join($filename)
	{
		// Open the file
		$file = fopen($filename, 'wb+');

		// Read files in 8k blocks
		$block_size = 1024 * 8;

		// Total number of peices
		$pieces = 0;

		while (is_file($piece = $filename.'.'.str_pad($pieces + 1, 3, '0', STR_PAD_LEFT)))
		{
			// Read another piece
			$pieces += 1;

			// Open the piece for reading
			$piece = fopen($piece, 'rb');

			while ( ! feof($piece))
			{
				// Transfer the data in blocks
				fwrite($file, fread($piece, $block_size));
			}

			// Close the peice
			fclose($piece);
		}

		return $pieces;
	}

} // End file
