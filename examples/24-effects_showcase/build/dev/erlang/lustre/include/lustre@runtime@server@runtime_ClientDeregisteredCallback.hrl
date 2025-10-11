-record(client_deregistered_callback, {
    callback :: fun((lustre@runtime@transport:client_message(any())) -> nil)
}).
