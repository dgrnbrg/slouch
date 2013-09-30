# slouch

Simple Clojure RPC, now with clusters!

Inspired by slacker and slacker-cluster, but using standard versions of:

- nippy (https://github.com/ptaoussanis/nippy)
- http.async.client (https://github.com/neotyk/http.async.client)
- ring (https://github.com/ring-clojure/ring)

## usage

;; server
(require '[slouch.server]
         '[slouch.example.api]
(def ring-app
 (slouch.server/handler 'slouch.example.api))

;; client
(require '[slouch.client])
(def c (client/new-client "http://localhost:3000"))
(defn-remote c sum :remote-ns 'slouch.example.api)
(sum 1 2 3)
;; 6

# design

## Protocol

Client messages
- Request (RPC function call)
- Ping
- Inspect function metadata
- Inspect namespace functions

Server messages
- Response (RPC function result)
- Pong
- Inspect function metadata ack
- Inspect namespace functions ack
- Protocol mismatch
- Invalid packet

Runs as a Ring adaptor

localhost:1234/clojure.core.nippy

URI encodes namespace, function, content-type
body encodes data (function args)


Can get rid of protocol/coded/etc by using known endpoints

/ping - Return HTTP 200, or not.
/meta - Inspect function metadata
/ns   - Inspect namespace functions


## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
