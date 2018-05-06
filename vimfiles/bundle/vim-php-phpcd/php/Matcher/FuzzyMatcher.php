<?php
namespace PHPCD\Matcher;

class FuzzyMatcher implements Matcher
{
    public function match($pattern, $subject)
    {
        if (!$pattern) {
            return true;
        }

        $chars = str_split($pattern);
        $parts = ['/.*'];

        foreach ($chars as $char) {
            $parts[] = $char;
            $parts[] = '.*';
        }
        $parts[] = '/i';

        $pattern = implode($parts);

        return preg_match($pattern, $subject);
    }
}
