<?php
namespace Lvht\MsgpackRpc;

class ForkServer implements Server
{
    use AbstractServer;

   /**
    * @var Messenger
    */
    private $messenger;

    public function __construct(Messenger $messenger, Handler $handler)
    {
        $this->messenger = $messenger;
        $this->setHandler($handler);

        register_shutdown_function(array($this, 'shutdown'));
    }

    public function loop($infinite = true)
    {
        do {
            $message = $this->messenger->next();
            $pid = pcntl_fork();
            if ($pid == -1) {
                $this->shutdown();
                exit(1);
            } elseif ($pid > 0) {
                pcntl_waitpid($pid, $status);
            } else {
                $this->onMessage($message);
                exit(0);
            }
        } while ($infinite);
    }

    public function write(array $message)
    {
        $this->messenger->send($message);
    }
}
