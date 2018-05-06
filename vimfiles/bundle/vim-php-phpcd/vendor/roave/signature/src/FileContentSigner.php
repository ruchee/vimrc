<?php

declare(strict_types=1);

namespace Roave\Signature;

use Roave\Signature\Encoder\EncoderInterface;

final class FileContentSigner implements SignerInterface
{
    /**
     * @var EncoderInterface
     */
    private $encoder;

    /**
     * {@inheritDoc}
     */
    public function __construct(EncoderInterface $encoder)
    {
        $this->encoder = $encoder;
    }

    public function sign(string $phpCode): string
    {
        return 'Roave/Signature: ' . $this->encoder->encode($phpCode);
    }
}
