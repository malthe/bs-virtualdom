type 'a t =
    Attribute of string option * string * string
  | ClassName of string
  | EventListener : int * ('a, 'b) listener -> 'a t
  | RemoveTransition of string option * 'a t array * bool
  | Property of string * string
  | Skip
  | Style of string * string * bool
  | Text of Dom.text option * string
  | Thunk : ('b * ('b -> 'a t) * 'a t option) -> 'a t
  | Detached of string option * string option * string * 'a t array
  | Attached of 'a vnode
  | Wedge of 'a t array

and ('a, 'b) listener = 'b Dom.event_like -> 'a

and 'a vnode = {
  element : Dom.element;
  key : string option;
  namespace : string option;
  selector : string;
  directives : 'a t array;
  detached : 'a t option;
  events : int;
}
