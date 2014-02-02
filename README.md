[![Analytics](https://ga-beacon.appspot.com/UA-46795389-1/wsecli/README)](https://github.com/igrigorik/ga-beacon)


wsecli
======

A WebSocket client written in Erlang 
[![Build Status](https://travis-ci.org/madtrick/e_e.png?branch=master)](https://travis-ci.org/madtrick/wsecli)

* [Supported WebSocket version](#versions)
* [Build](#build)
* [Usage](#usage)
* [Tests](#tests)
* [TODO](#todo)
* [License](#license)
* [Contribute](#contribute)

### Features <a name="features"> ###
  * Built using [wsock](https://github.com/madtrick/wsock)

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
* Support streaming of data.
* Support ssl.
* Creation on multiple clients.

###Author
Farruco Sanjurjo. You can contact me at:

* Twitter [@madtrick](https://twitter.com/madtrick)
* Mail madtrick@gmail.com

### License <a name="installation">

Copyright [2013] [Farruco Sanjurjo Arcay]

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

### Contribute <a name="contribute">

If you find or think that something isn't working properly, just open an issue.

Pull requests and patches (with tests) are welcome.
