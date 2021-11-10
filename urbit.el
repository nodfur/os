(require 'urbit)
(require 'urbit-http)
(require 'urbit-chat)
(require 'aio)

(aio-wait-for
 (urbit-launch "http://localhost:8080" "savbex-dotnul-samtuc-datsyt"))

(urbit-http-subscribe
 "graph-store" "/updates"
 (lambda (event) (message "Graph update: %s" event)))
