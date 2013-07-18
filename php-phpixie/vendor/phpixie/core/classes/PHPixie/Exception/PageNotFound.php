<?php

namespace PHPixie\Exception;

/**
 * This exception is throw when none of the routes matched
 * or the specified controller or action wasn't found.
 */
class PageNotFound extends \Exception {}