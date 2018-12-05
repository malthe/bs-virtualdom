open Virtualdom_events

type ('a, 'b) t =
    Attached of ('a, 'b) vnode
  | Attribute of string option * string * string
  | Component : (
      ('b -> ('c, 'd) t) *
      (('c -> unit) -> 'c -> 'a) *
      'b *
      (('c, 'd) t * EventSet.t * EventSet.t) option
    ) -> ('a, 'b) t
  | ClassName of string
  | Detached of
      string option * string *
      (Dom.element -> 'a option) option * ('a,'b) t array
  | EventListener :
      EventSet.t * bool * ('event Dom.event_like -> 'a option) -> ('a, 'b) t
  | Index of (Js.Dict.key * ('a, 'b) t) array
  | Property of string * string
  | RemoveTransition of string option * ('a, 'b) t array * bool
  | Skip
  | Style of string * string * bool
  | Text of Dom.text option * string
  | Thunk : ('c * ('c -> ('a, 'b) t) *
             (('a, 'b) t * EventSet.t * EventSet.t) option
            ) -> ('a, 'b) t
  | Wedge of (('a, 'b) t array * (EventSet.t * EventSet.t) option)

and ('a, 'b) vnode = {
  element : Dom.element;
  namespace : string option;
  selector : string;
  directives : ('a, 'b) t array;
  detached : ('a, 'b) t option;
  enabledEvents : EventSet.t;
  passiveEvents : EventSet.t;
}

type ('a, 'b, 'c) listener =
  ?passive:bool -> ('c -> 'a option) -> ('a, 'b) t
