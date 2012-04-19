wsecli
======

A WebSocket client written in Erlang

* [Disclaimer](#disclaimer)
* [Supported WebSocket version](#versions)
* [Build](#build)
* [Usage](#usage)
* [Tests](#tests)
* [TODO](#todo)
* [License](#license)
* [Contribute](#contribute)

### Disclaimer <a name="disclaimer">

This project was born as a side project while I'm doing my end of dregree project. What this means is:

* I don't have too much time to include new features.

But,

* I'll try to fix any bugs.
* I'll try to finish anything on the TODO.
* If anyone suggests something interesting/cool, I'll try to include it : )

### Supported protocol versions <a name="versions"/> ###
Currently only the version specificied at [RFC6455](http://tools.ietf.org/html/rfc6455) (version 13) is supported.

Please notice that currently, neither _subprotocols_ nor _extensions_ are currently available.

### Build <a name="build">###

Add this repo as a dependency to your rebar.config file and then

  ```bash
  rebar compile
  ```

### Usage <a name="usage">###

I will demostrate its usage with the echo service at [www.websocket.org](http://www.websocket.org/echo.html).


Start it,


  ```erlang
  1>wsecli:start("echo.websocket.org", 80, "/").
  ```

Add a callback for received messages,

  ```erlang
  2>wsecli:on_message(fun(text, Message)-> io:format("Echoed message: ~s ~n", [Message]) end).
  ```

Send a message that will be echoed,

  ```erlang
  3> wsecli:send("Hello").
  ok
  Echoed message: Hello
  ```

And finally to stop it

  ```erlang
  4>wsecli:stop().
  ```

#### Callbacks

Callbacks for the events: *on_open*, *on_error*, *on_message* and *on_close* can be added. Check the code or the documentation for details.

### Tests <a name="tests">

#### Unit tests

Unit test where done with the library [_espec_](https://github.com/lucaspiller/espec) by [lucaspiller](https://github.com/lucaspiller).

 To run them

  ```bash
  rake spec
  ```
  or, in case you don't have rake installed,

  ```bash
  rebar compile && ERL_LIBS='deps/' ./espec test/spec/
  ```

### TODO <a name="todo">

* Accept WebSocket uris (those with ws:// format).
* Support streaming (not sure how to do this).
* Support ssl.
* Creation on multiple clients (too lazy, that's why I haven't done it already)


### License <a name="installation">

Licensed under Apache 2.0. Check LICENSE for details

### Contribute <a name="contribute">

If you find or think that something isn't working properly, just open an issue.

Pull requests and patches (with tests) are welcome.
