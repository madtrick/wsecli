-record(handshake, {
    version      :: integer(),
    request_line :: list({string(), string()}),
    headers      :: list({string(), string()})
  }).
