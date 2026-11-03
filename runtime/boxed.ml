type ('a : value, 'b : any) t : void

external magic_create
  : ('a : value) ('b : any).
  unit# -> ('a, 'b) t
  @@ stateless
  = "%identity"
