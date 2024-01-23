val listen : request_handler:Unix.sockaddr Piaf.Server.Handler.t ->
    error_handler:(Unix.sockaddr ->
                   ?request:Piaf.Request.t ->
                   respond:(headers:Piaf.Headers.t ->
                            Piaf.Body.t -> Piaf.Server.Error_response.t) ->
                   [ `Bad_gateway
                   | `Bad_request
                   | `Exn of exn
                   | `Internal_server_error ] ->
                   Piaf.Server.Error_response.t Lwt.t) ->
    int -> unit
