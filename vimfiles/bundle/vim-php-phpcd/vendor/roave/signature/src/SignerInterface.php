<?php

declare(strict_types=1);

namespace Roave\Signature;

interface SignerInterface
{
    public function sign(string $phpCode) : string;
}
