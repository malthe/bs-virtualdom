open Virtualdom_events

type 'a t =
    Attached of 'a vnode
  | Attribute of string option * string * string
  | Component : (
      ('b -> 'c t) *
      (('c -> unit) -> 'c -> 'a) *
      'b *
      ('c t * EventSet.t * EventSet.t) option
    ) -> 'a t
  | ClassName of string
  | Detached of
      string option * string *
      (Dom.element -> 'a option) option *
      (Dom.element -> Dom.element -> unit) option *
      'a t array
  | EventListener :
      EventSet.t * bool * ('event Dom.event_like -> 'a option) -> 'a t
  | Index of (Js.Dict.key * 'a t) array
  | Property of string * string
  | RemoveTransition of string option * 'a t array * bool
  | Skip
  | Style of string * string * bool
  | Text of Dom.text option * string
  | Thunk : ('c * ('c -> 'a t) *
             ('a t * EventSet.t * EventSet.t) option
            ) -> 'a t
  | Wedge of ('a t array * (EventSet.t * EventSet.t) option)

and 'a vnode = {
  element : Dom.element;
  namespace : string option;
  selector : string;
  directives : 'a t array;
  detached : 'a t option;
  enabledEvents : EventSet.t;
  passiveEvents : EventSet.t;
  onRemove : (Dom.element -> Dom.element -> unit) option;
}

type ('a, 'b) listener =
  ?passive:bool -> ('b -> 'a option) -> 'a t
