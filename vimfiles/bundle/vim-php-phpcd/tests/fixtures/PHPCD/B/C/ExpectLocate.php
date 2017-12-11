<?php
namespace PHPCD\B\C;

use PHPCD\SameName\A\Same;
use PHPCD\SameName\B\Same as BSame;

class ExpectLocate
{
    public function xxx()
    {
        $a = new Same();
        $b = new BSame();

        $a->pa;
        $b->pa;

        $a->na();
        $b->na();
        $a->nb();

        Same::CA;
        BSame::CA;

        Same::getInstance()->na();
        Same::getInstance()->nb();
    }
}
