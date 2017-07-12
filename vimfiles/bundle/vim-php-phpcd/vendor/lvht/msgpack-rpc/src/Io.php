<?php
namespace Lvht\MsgpackRpc;

interface Io
{
    /**
     * blocking io read
     */
    function read($length);

    /**
     * read one line
     */
    public function readLine();

    /**
     * blocking io write
     *
     * @return int the written content's length
     */
    function write($data);
}
