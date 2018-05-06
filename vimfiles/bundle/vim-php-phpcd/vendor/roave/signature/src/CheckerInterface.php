<?php

declare(strict_types=1);

namespace Roave\Signature;

interface CheckerInterface
{
    /**
     * @param string $phpCode
     *
     * @return bool
     */
    public function check(string $phpCode): bool;
}
