# discord-ml
Experimental (and incomplete) bindings to the Discord API (in a very early state).

Inspired by the (much more mature) [disml](https://github.com/Mishio595/disml) bindings and reusing parts of it.

The final goal is to build bots that can run as Mirage unikernels.

## Sketchy todo:
- [ ] Conform to Discord's rate limiting policy
- [ ] Handle reconnection
- [ ] Allow sharding
- [ ] Complete the API bindings
- [ ] Handle voice UDP connections
- [ ] Make it Mirage compatible (see https://github.com/vbmithr/ocaml-websocket/issues/105)
