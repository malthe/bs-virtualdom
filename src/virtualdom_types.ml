open Virtualdom_events

type ('a, 'b) t =
    Attached of ('a, 'b) vnode
  | Attribute of string option * string * string
  | Component : (
      ('b -> ('c, 'd) t) *
      ('c -> 'a) *
      'b *
      (('c, 'd) t * EventSet.t) option
    ) -> ('a, 'b) t
  | ClassName of string
  | Detached of string option * string * ('a,'b) t array
  | EventListener :
      EventSet.t * ('event Dom.event_like -> 'a option) -> ('a, 'b) t
  | Index of (Js.Dict.key * ('a, 'b) t) array * EventSet.t option
  | Property of string * string
  | RemoveTransition of string option * ('a, 'b) t array * bool
  | Skip
  | Style of string * string * bool
  | Text of Dom.text option * string
  | Thunk : ('c * ('c -> ('a, 'b) t) * (('a, 'b) t * EventSet.t) option) -> ('a, 'b) t
  | Wedge of (('a, 'b) t array * EventSet.t option)

and ('a, 'b) vnode = {
  element : Dom.element;
  namespace : string option;
  selector : string;
  directives : ('a, 'b) t array;
  detached : ('a, 'b) t option;
  events : EventSet.t;
}
