<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * STDOUT log writer. Writes out messages to STDOUT.
 *
 * @package    Kohana
 * @category   Logging
 * @author     Kohana Team
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaphp.com/license
 */
class Kohana_Log_StdOut extends Log_Writer {

	/**
	 * Writes each of the messages to STDOUT.
	 *
	 *     $writer->write($messages);
	 *
	 * @param   array   $messages
	 * @return  void
	 */
	public function write(array $messages)
	{
		foreach ($messages as $message)
		{
			// Writes out each message
			fwrite(STDOUT, $this->format_message($message).PHP_EOL);
		}
	}

} // End Kohana_Log_StdOut
