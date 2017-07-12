<?php

namespace PHPCD\B\C;

class ExpectPublicVariable
{
    public function x()
    {
        $alpha = new \PHPCD\A\Alpha;

        $alpha->pb
    }
}
// classInfo $class_name == parent
