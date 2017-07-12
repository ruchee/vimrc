<?php namespace Lvht\MsgpackRpc;

interface Messenger
{
    function next();
    function send($message);
}
