<?php namespace Lvht\MsgpackRpc;

use PHPUnit\Framework\TestCase;
use Mockery as m;

class JsonMessengerTest extends TestCase
{
    public function testNext()
    {
        $io = m::mock(Io::class);
        $io->shouldReceive('readLine')->andReturn("[1,2,3]\n");
        $messenger = new JsonMessenger($io);
        $msg = $messenger->next();
        self::assertEquals(array(1,2,3), $msg);
    }

    public function testSend()
    {
        $io = m::mock(Io::class);
        $io->shouldReceive('write')->andReturnUsing(function ($msg) {
            $this->assertEquals("[1,2,3]\n", $msg);
        });
        $messenger = new JsonMessenger($io);
        $messenger->send(array(1,2,3));
    }
}
