<?php
/*
 *---------------------------------------------------------------
 * Database Bundle
 *---------------------------------------------------------------
 * 
 * Here we define the database interface shadow and namespace
 */
// namepace
\CCFinder::map( 'DB', COREPATH.'bundles/Database/' );

// and the shdaow
\CCFinder::shadow( 'DB', 'DB', COREPATH.'bundles/Database/DB'.EXT );

/*
 *---------------------------------------------------------------
 * UI Bundle
 *---------------------------------------------------------------
 * 
 * The UI Bundle contains some helpers to generate HTML.
 */
// namepace
\CCFinder::map( 'UI', COREPATH.'bundles/UI/' );

/*
 *---------------------------------------------------------------
 * Session Bundle
 *---------------------------------------------------------------
 * 
 * Session managment bundle
 */
// namepace
\CCFinder::map( 'Session', COREPATH.'bundles/Session/' );

// and the shdaow
\CCFinder::shadow( 'CCSession', 'Session', COREPATH.'bundles/Session/CCSession'.EXT );

/*
 *---------------------------------------------------------------
 * Authentication Bundle
 *---------------------------------------------------------------
 * 
 * The Authentication bundle for basic a basic user and login
 */
// namepace
\CCFinder::map( 'Auth', COREPATH.'bundles/Auth/' );

// and the shdaow
\CCFinder::shadow( 'CCAuth', 'Auth', COREPATH.'bundles/Auth/CCAuth'.EXT );

/*
 *---------------------------------------------------------------
 * Email Bundle
 *---------------------------------------------------------------
 * 
 * The Email bundle mostly wraps phpmailer
 */
// namepace
\CCFinder::map( 'Mail', COREPATH.'bundles/Mail/' );
// phpmailer
\CCFinder::bind( array(
    "Mail\\PHPMailer\\PHPMailer" => COREPATH.'bundles/Mail/PHPMailer/class.phpmailer'.EXT,
    "Mail\\PHPMailer\\POP3" => COREPATH.'bundles/Mail/PHPMailer/class.php3'.EXT,
    "Mail\\PHPMailer\\SMTP" => COREPATH.'bundles/Mail/PHPMailer/class.smtp'.EXT,
));

// and the shdaow
\CCFinder::shadow( 'CCMail', 'Mail', COREPATH.'bundles/Mail/CCMail'.EXT );