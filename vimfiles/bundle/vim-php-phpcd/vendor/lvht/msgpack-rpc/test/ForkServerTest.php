<?php namespace Lvht\MsgpackRpc;

use PHPUnit\Framework\TestCase;
use Mockery as m;

class EchoHandler implements Handler
{
    public function setServer(Server $server) {}

    public function echo($message)
    {
        return $message;
    }
}

class ForkServerTest extends TestCase
{
    public function testLoop()
    {
        $messenger = m::mock(Messenger::class);
        $messenger->shouldReceive('next')->andReturn(array(0, 1, 'echo', array("hello")));
        $messenger->shouldReceive('send')->with(array(1, 1, null, "hello"));
        $server = new ForkServer($messenger, new EchoHandler());
        $server->loop(false);
    }
}
