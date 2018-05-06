<?php

declare(strict_types=1);

namespace Roave\SignatureTest;

use PHPUnit_Framework_TestCase;
use Roave\Signature\Encoder\Base64Encoder;
use Roave\Signature\Encoder\EncoderInterface;
use Roave\Signature\FileContentChecker;

/**
 * @covers \Roave\Signature\FileContentChecker
 */
final class FileContentCheckerTest extends PHPUnit_Framework_TestCase
{
    /**
     * @var EncoderInterface|\PHPUnit_Framework_MockObject_MockObject
     */
    private $encoder;

    /**
     * {@inheritDoc}
     */
    protected function setUp()
    {
        parent::setUp();

        $this->encoder = $this->createMock(EncoderInterface::class);
    }

    public function testShouldCheckClassFileContent()
    {
        $classFilePath = __DIR__ . '/../../fixture/UserClassSignedByFileContent.php';

        self::assertFileExists($classFilePath);

        $checker = new FileContentChecker(new Base64Encoder());

        $checker->check(file_get_contents($classFilePath));
    }

    public function testShouldReturnFalseIfSignatureDoesNotMatch()
    {
        $classFilePath = __DIR__ . '/../../fixture/UserClassSignedByFileContent.php';

        self::assertFileExists($classFilePath);

        $expectedSignature = 'YToxOntpOjA7czoxNDE6Ijw/cGhwCgpuYW1lc3BhY2UgU2lnbmF0dXJlVGVzdEZpeHR1cmU7' .
            'CgpjbGFzcyBVc2VyQ2xhc3NTaWduZWRCeUZpbGVDb250ZW50CnsKICAgIHB1YmxpYyAkbmFtZTsKCiAgICBwcm90ZW' .
            'N0ZWQgJHN1cm5hbWU7CgogICAgcHJpdmF0ZSAkYWdlOwp9CiI7fQ==';

        $this->encoder->expects(self::once())->method('verify')->with(
            str_replace(
                '/** Roave/Signature: ' . $expectedSignature . ' */' . "\n",
                '',
                file_get_contents($classFilePath)
            ),
            $expectedSignature
        );

        $checker = new FileContentChecker($this->encoder);

        self::assertFalse($checker->check(file_get_contents($classFilePath)));
    }

    public function testShouldReturnFalseIfClassIsNotSigned()
    {
        $classFilePath = __DIR__ . '/../../fixture/UserClass.php';

        self::assertFileExists($classFilePath);

        $checker = new FileContentChecker($this->encoder);

        self::assertFalse($checker->check(file_get_contents($classFilePath)));
    }
}
