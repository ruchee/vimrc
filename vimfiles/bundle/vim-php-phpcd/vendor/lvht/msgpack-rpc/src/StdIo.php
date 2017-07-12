<?php
namespace Lvht\MsgpackRpc;

class StdIo implements Io
{
    private $stdin;

    public function __construct()
    {
        $this->stdin = fopen('php://stdin', 'r');
    }

    public function read($length)
    {
        return fread($this->stdin, 1024);
    }

    public function readLine()
    {
        return fgets($this->stdin);
    }

    public function write($data)
    {
        return fwrite(STDOUT, $data);
    }
}

