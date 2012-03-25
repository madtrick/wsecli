-record(handshake, {
    version      :: integer(),
    request_line :: list({string(), string()}),
    headers      :: list({string(), string()})
  }).

-record(http_message,{
    type :: response | request,
    start_line :: list({atom(), string()}),
    headers :: list({atom(), string()})
  }).
