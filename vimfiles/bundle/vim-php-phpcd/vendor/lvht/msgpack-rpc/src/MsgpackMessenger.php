<?php namespace Lvht\MsgpackRpc;

class MsgpackMessenger implements Messenger
{
    /**
     * @var Io
     */
    private $io;

    /**
     * @var \MessagePackUnpacker;
     */
    private $unpacker;

    public function __construct(Io $io)
    {
        $this->io = $io;
        $this->unpacker = new \MessagePackUnpacker;
    }

    public function next()
    {
        do {
            $buffer = $this->io->read(1024);
            $this->unpacker->feed($buffer);

            if ($this->unpacker->execute()) {
                $message = $this->unpacker->data();
                $this->unpacker->reset();
                return $message;
            }
        } while (true);
    }

    public function send($message)
    {
        $this->io->write(msgpack_pack($message));
    }
}
