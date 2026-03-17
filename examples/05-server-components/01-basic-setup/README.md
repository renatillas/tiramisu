# 05-server-components/01-basic-setup

This mirrors Lustre's basic server-component guide, but the server component
renders a Tiramisu scene instead of a counter.

It demonstrates:

1. Serving a `<lustre-server-component>` host page from an Erlang server.
2. Starting a Lustre server component with `lustre.start_server_component`.
3. Forwarding websocket traffic between Mist and the server component runtime.
4. Registering Tiramisu in the browser so server-rendered scene tags become live
   custom elements.
5. Driving scene animation with `scene.on_tick` inside server-side Lustre state.
6. Combining websocket-driven UI controls with continuous movement updates.

Run it with:

```sh
cd client
gleam build --target javascript
cd ..
gleam run
```

Then open `http://localhost:1234`.
