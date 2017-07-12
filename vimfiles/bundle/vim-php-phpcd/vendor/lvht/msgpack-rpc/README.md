# Msgpack-RPC

This is a simple implementation of [MsgpackRpc](https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md).

## Install
	composer require lvht/msgpack-rpc

## Usage

```bash
➜  echo '[0,1,"echo",["hello"]]'|php example/echo.php
[1,1,null,"hello"]
```

## TODO
- [x] Unit Test
- [ ] SocketIo
- [ ] AsyncServer
- [ ] Docs
