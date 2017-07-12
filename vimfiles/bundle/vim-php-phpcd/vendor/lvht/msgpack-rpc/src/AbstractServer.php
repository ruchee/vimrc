<?php
namespace Lvht\MsgpackRpc;

trait AbstractServer
{
    private $msg_id = 0;

    private $current_msg_id;

    private $request_callback = [];

    private $handlers;

    public function setHandler(Handler $handler)
    {
        $handler->setServer($this);
        $this->handlers = [$handler];
    }

    public function addHandler(Handler $handler)
    {
        $handler->setServer($this);
        $this->handlers[] = $handler;
    }

    public function shutdown()
    {
        if (!$this->current_msg_id) {
            return;
        }

        $error = error_get_last();
        $error = sprintf('"%s at %s:%d"', $error['message'], $error['file'], $error['line']);

        $response = array(self::TYPE_RESPONSE, $this->current_msg_id, $error, null);
        $this->write($response);
    }

    protected function onMessage(array $message)
    {
        $type = current($message);
        switch ($type) {
        case Server::TYPE_REQUEST:
            $this->onRequest($message);
            break;
        case Server::TYPE_NOTIFICATION:
            $this->onNotification($message);
            break;
        case Server::TYPE_RESPONSE:
            $this->onResponse($message);
            break;
        }
    }

    protected function onRequest($message)
    {
        list($type, $msg_id, $method, $params) = $message;

        $this->current_msg_id = $msg_id;

        $result = null;
        $error = 'method not exists';
        foreach ($this->handlers as $handler) {
            if (method_exists($handler, $method)) {
                $result = $this->doRequest(array($handler, $method), $params);
                $error = null;
                break;
            }
        }
        $response = array(self::TYPE_RESPONSE, $msg_id, $error, $result);
        $this->write($response);

        $this->current_msg_id = null;
    }

    protected function onNotification($message)
    {
        list($type, $method, $params) = $message;
        foreach ($this->handlers as $handler) {
            if (method_exists($handler, $method)) {
                $this->doRequest(array($handler, $method), $params);
            }
        }
    }

    protected function doRequest($callback, $params)
    {
        return call_user_func_array($callback, $params);
    }

    protected function onResponse($message)
    {
        list($type, $msg_id, $error, $result) = $message;
        $this->doCallback($msg_id, $error, $result);
    }

    public function call($method, $params, $callback = null)
    {
        if ($callback) {
            $msg_id = $this->getMessageId();
            $message = array(self::TYPE_REQUEST, $msg_id, $method, $params);
            $this->addCallback($msg_id, $callback);
        } else {
            $message = array(self::TYPE_NOTIFICATION, $method, $params);
        }

        $this->write($message);
    }

    private function addCallback($msg_id, $callback)
    {
        $this->request_callback[$msg_id] = $addCallback;
    }

    private function doCallback($msg_id, $error, $result)
    {
        if (!isset($this->request_callback[$msg_id])) {
            return;
        }

        $callback = $this->request_callback[$msg_id];
        $callback($error, $result);
    }

    public function getMessageId()
    {
        return $this->msg_id++;
    }
}
