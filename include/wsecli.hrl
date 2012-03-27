-record(http_message,{
    type :: response | request,
    start_line :: list({atom(), string()}),
    headers :: list({atom(), string()})
  }).

-record(handshake, {
    version      :: integer(),
    message :: #http_message{}
  }).

-record(frame, {}).
