<?php

declare(strict_types=1);

namespace Roave\Signature\Encoder;

final class Base64Encoder implements EncoderInterface
{
    /**
     * {@inheritDoc}
     */
    public function encode(string $codeWithoutSignature): string
    {
        return base64_encode($codeWithoutSignature);
    }

    /**
     * {@inheritDoc}
     */
    public function verify(string $codeWithoutSignature, string $signature): bool
    {
        return hash_equals($this->encode($codeWithoutSignature), $signature);
    }
}
