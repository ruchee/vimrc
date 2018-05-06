<?php
namespace PHPCD\Matcher;

class Factory
{
    /**
     * @param $name matcher name, support 'fuzzy', 'prefix'
     * @return Matcher
     */
    public static function make(string $name)
    {
        switch ($name) {
        case 'fuzzy':
            return new FuzzyMatcher();
        case 'prefix':
            return new PrefixMatcher();
        default:
            throw new \InvalidArgumentException('invalid name');
        }
    }
}
