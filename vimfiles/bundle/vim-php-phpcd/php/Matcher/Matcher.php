<?php
namespace PHPCD\Matcher;

interface Matcher
{
    /**
     * match string
     *
     * @param string $pattern
     * @param string $target
     *
     * @return bool If $patterh is empty, return true.
     */
    function match($pattern, $target);
}
