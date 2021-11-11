<?php

namespace app\helpers;

class Query
{
    public static function clamp($value): int
    {
        if (!ctype_digit($value) || $value < 1) {
            return 1;
        } elseif ($value > 500) {
            return 500;
        } else {
            return (int) $value;
        }
    }
}
