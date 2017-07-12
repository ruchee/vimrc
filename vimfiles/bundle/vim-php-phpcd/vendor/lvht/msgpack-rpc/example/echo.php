<?php
require __DIR__.'/../vendor/autoload.php';

use Lvht\MsgpackRpc\Handler;
use Lvht\MsgpackRpc\Server;
use Lvht\MsgpackRpc\ForkServer;
use Lvht\MsgpackRpc\JsonMessenger;
use Lvht\MsgpackRpc\StdIo;

class EchoHandler implements Handler
{
    /**
     * @var Server
     */
    private $server;

    public function setServer(Server $server)
    {
        $this->server = $server;
    }

    public function echo($data)
    {
        return $data;
    }
}

// if message is line terminated json string
$messenger = new JsonMessenger(new StdIo());
// if message is msgpack
// $messenger = new MsgpackMessenger(new StdIo());

$server = new ForkServer($messenger, new EchoHandler());
$server->loop(false);
