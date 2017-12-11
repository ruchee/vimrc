<?php
namespace PHPCD\SameName\A;

abstract class SuperSame
{
    /**
     * @return self
     */
    public static function getInstance()
    {
        if (!static::$instance) {
            static::$instance = new static();
        }

        return static::$instance;
    }

    abstract function mm();
}
