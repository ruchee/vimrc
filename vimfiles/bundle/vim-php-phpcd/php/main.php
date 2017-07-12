<?php
set_error_handler(function ($severity, $message, $file, $line) {
    throw new ErrorException($message, 0, $severity, $file, $line);
});

$root   = $argv[1];
$messenger = $argv[2];
$autoload_file = $argv[3];

/** load autoloader for PHPCD **/
require __DIR__ . '/../vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;
use Lvht\MsgpackRpc\ForkServer;
use Lvht\MsgpackRpc\MsgpackMessenger;
use Lvht\MsgpackRpc\JsonMessenger;
use Lvht\MsgpackRpc\StdIo;

$log_path = getenv('HOME') . '/.phpcd.log';
ini_set('error_log', $log_path);
$logger = new Logger('PHPCD');
$logger->pushHandler(new StreamHandler($log_path, Logger::DEBUG));
if ($messenger == 'json') {
    $messenger = new JsonMessenger(new StdIo());
} else {
    $messenger = new MsgpackMessenger(new StdIo());
}

try {
    /** load autoloader for the project **/
    if (is_readable($autoload_file)) {
        require $autoload_file;
    }

    $server = new ForkServer($messenger, new PHPCD\PHPCD($root, $logger));
    $server->addHandler(new PHPCD\PHPID($root, $logger));

    $server->loop();
} catch (\Throwable $e) {
    $logger->error($e->getMessage(), $e->getTrace());
} catch (\Exception $e) {
    $logger->error($e->getMessage(), $e->getTrace());
}
