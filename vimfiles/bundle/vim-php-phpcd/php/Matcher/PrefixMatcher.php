<?php
namespace PHPCD\Matcher;

class PrefixMatcher implements Matcher
{
    public function match($pattern, $target)
    {
        if (!$pattern) {
            return true;
        }

        return substr($target, 0, strlen($pattern)) === $pattern;
    }
}
