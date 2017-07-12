<?php namespace Lvht\MsgpackRpc;

class JsonMessenger implements Messenger
{
    /**
     * @var Io
     */
    private $io;

    public function __construct(Io $io)
    {
        $this->io = $io;
    }

    public function next()
    {
        $json = rtrim($this->io->readLine());
        return json_decode($json, true);
    }

    public function send($message)
    {
        $this->io->write(json_encode($message)."\n");
    }
}
