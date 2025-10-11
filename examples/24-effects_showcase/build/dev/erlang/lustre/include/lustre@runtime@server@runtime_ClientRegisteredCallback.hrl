-record(client_registered_callback, {
    callback :: fun((lustre@runtime@transport:client_message(any())) -> nil)
}).
