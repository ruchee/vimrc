<?php

declare(strict_types=1);

namespace Roave\SignatureTest\Encoder;

use Roave\Signature\Encoder\Base64Encoder;

/**
 * @covers \Roave\Signature\Encoder\Base64Encoder
 */
final class Base64EncoderTest extends \PHPUnit_Framework_TestCase
{
    public function testEncode()
    {
        $encoder = new Base64Encoder();

        self::assertSame('IA==', $encoder->encode(' '));
        self::assertSame('PD9waHA=', $encoder->encode('<?php'));
    }

    public function testVerify()
    {
        $value = uniqid('values', true);
        self::assertTrue((new Base64Encoder())->verify($value, base64_encode($value)));
    }
}
