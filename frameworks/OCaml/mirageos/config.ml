open Mirage

let handler =
  let packages = [package "mirage-http"] in
  foreign
    ~packages
    "Unikernel.Main" (conduit @-> job)

let () =
  register "conduit_server" [ handler $ conduit_direct (generic_stackv4 default_network) ]
