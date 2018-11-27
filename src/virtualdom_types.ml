type 'a t =
    Attached of 'a vnode
  | Attribute of string option * string * string
  | ClassName of string
  | Detached of string option * string * 'a t array
  | EventListener : int * ('a, 'b) listener -> 'a t
  | Index of (Js.Dict.key * 'a t) array
  | Property of string * string
  | RemoveTransition of string option * 'a t array * bool
  | Skip
  | Style of string * string * bool
  | Text of Dom.text option * string
  | Thunk : ('b * ('b -> 'a t) * 'a t option) -> 'a t
  | Wedge of 'a t array

and ('a, 'b) listener = 'b Dom.event_like -> 'a

and 'a vnode = {
  element : Dom.element;
  namespace : string option;
  selector : string;
  directives : 'a t array;
  detached : 'a t option;
  events : int;
}
