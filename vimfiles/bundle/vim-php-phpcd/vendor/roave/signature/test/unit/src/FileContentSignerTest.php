<?php

declare(strict_types=1);

namespace Roave\SignatureTest;

use PHPUnit_Framework_TestCase;
use Roave\Signature\Encoder\Base64Encoder;
use Roave\Signature\FileContentSigner;

/**
 * @covers \Roave\Signature\FileContentSigner
 */
final class FileContentSignerTest extends PHPUnit_Framework_TestCase
{
    public function testSign()
    {
        $signer = new FileContentSigner(new Base64Encoder());

        self::assertSame('Roave/Signature: PD9waHA=', $signer->sign('<?php'));
        self::assertSame('Roave/Signature: PD9waHAK', $signer->sign('<?php' . "\n"));
        self::assertSame('Roave/Signature: PGh0bWw+', $signer->sign('<html>'));
        self::assertSame('Roave/Signature: cGxhaW4gdGV4dA==', $signer->sign('plain text'));
    }
}
