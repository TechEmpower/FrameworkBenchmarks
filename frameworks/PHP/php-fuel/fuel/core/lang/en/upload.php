<?php

return array(
	'error_'.\Upload::UPLOAD_ERR_OK						=> 'The file uploaded with success',
	'error_'.\Upload::UPLOAD_ERR_INI_SIZE				=> 'The uploaded file exceeds the upload_max_filesize directive in php.ini',
	'error_'.\Upload::UPLOAD_ERR_FORM_SIZE				=> 'The uploaded file exceeds the MAX_FILE_SIZE directive that was specified in the HTML form',
	'error_'.\Upload::UPLOAD_ERR_PARTIAL				=> 'The uploaded file was only partially uploaded',
	'error_'.\Upload::UPLOAD_ERR_NO_FILE				=> 'No file was uploaded',
	'error_'.\Upload::UPLOAD_ERR_NO_TMP_DIR				=> 'Configured temporary upload folder is missing',
	'error_'.\Upload::UPLOAD_ERR_CANT_WRITE				=> 'Failed to write uploaded file to disk',
	'error_'.\Upload::UPLOAD_ERR_EXTENSION				=> 'Upload blocked by an installed PHP extension',
	'error_'.\Upload::UPLOAD_ERR_MAX_SIZE				=> 'The uploaded file exceeds the defined maximum size',
	'error_'.\Upload::UPLOAD_ERR_EXT_BLACKLISTED		=> 'Upload of files with this extension is not allowed',
	'error_'.\Upload::UPLOAD_ERR_EXT_NOT_WHITELISTED	=> 'Upload of files with this extension is not allowed',
	'error_'.\Upload::UPLOAD_ERR_TYPE_BLACKLISTED		=> 'Upload of files of this file type is not allowed',
	'error_'.\Upload::UPLOAD_ERR_TYPE_NOT_WHITELISTED	=> 'Upload of files of this file type is not allowed',
	'error_'.\Upload::UPLOAD_ERR_MIME_BLACKLISTED		=> 'Upload of files of this mime type is not allowed',
	'error_'.\Upload::UPLOAD_ERR_MIME_NOT_WHITELISTED	=> 'Upload of files of this mime type is not allowed',
	'error_'.\Upload::UPLOAD_ERR_MAX_FILENAME_LENGTH	=> 'The uploaded file name exceeds the defined maximum length',
	'error_'.\Upload::UPLOAD_ERR_MOVE_FAILED			=> 'Unable to move the uploaded file to it\'s final destination',
	'error_'.\Upload::UPLOAD_ERR_DUPLICATE_FILE 		=> 'A file with the name of the uploaded file already exists',
	'error_'.\Upload::UPLOAD_ERR_MKDIR_FAILED			=> 'Unable to create the file\'s destination directory',
);
