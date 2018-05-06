<?php

declare(strict_types=1);

namespace Roave\SignatureTest\Encoder;

use Roave\Signature\Encoder\Sha1SumEncoder;

/**
 * @covers \Roave\Signature\Encoder\Sha1SumEncoder
 */
final class Sha1SumEncoderTest extends \PHPUnit_Framework_TestCase
{
    public function testEncode()
    {
        $value = uniqid('values', true);
        self::assertSame(sha1($value), (new Sha1SumEncoder())->encode($value));
    }

    public function testVerify()
    {
        $value = uniqid('values', true);
        self::assertTrue((new Sha1SumEncoder())->verify($value, sha1($value)));
    }
}
