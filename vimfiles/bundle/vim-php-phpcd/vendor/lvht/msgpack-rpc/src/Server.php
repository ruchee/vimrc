<?php
namespace Lvht\MsgpackRpc;

interface Server
{
    const TYPE_REQUEST = 0;
    const TYPE_RESPONSE = 1;
    const TYPE_NOTIFICATION = 2;

    /**
     * set handler
     *
     * call this method multiple will add multiple handlers
     */
    function setHandler(Handler $handler);

    /**
     * add handler
     *
     * call this method multiple will add multiple handlers
     */
    function addHandler(Handler $handler);

    /**
     * @var string $method
     * @var array $params
     * @var callable $callback function ($error, $result) {}
     */
    function call($method, $params, $callback = null);

    function loop($infinite = true);

    function shutdown();

    function getMessageId();

    function write(array $message);
}
