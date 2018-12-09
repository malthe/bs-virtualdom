open Virtualdom_types

type 'a directive = 'a t

(** Set an element attribute. *)
val attr : ?namespace:string -> string -> string -> 'a t

(** Set an element class. *)
val className : string -> 'a t

(** Include a component. *)
val component :
  ('b -> 'c t) ->
  (('c -> unit) -> 'c -> 'a) ->
  'b -> 'a t

(** Include directive if condition is true. *)
val cond : bool -> 'a t -> 'a t

(** An empty array of directives *)
val empty : 'a t array

(** This function returns a new virtual DOM node. *)
val h :
  ?namespace:string ->
  ?onInsert:(Dom.element -> 'a option) ->
  string -> 'a t array -> 'a t

(** This function inserts an keyed array of directives. *)
val keyed : (Js.Dict.key * 'a t) array -> 'a t

(** Include directive if condition is true. *)
val maybe : ('c -> 'a t) -> 'c option -> 'a t

(** This function mounts the app on an existing DOM element. *)
val mount :
  ?namespace:string ->
  Dom.element ->
  ('a -> 'b t array) ->
  ('a -> ('b -> unit) -> 'b -> 'a) -> 'a ->
  ('b -> unit)

(** Bind event listener to the "abort" event. *)
val onAbort : ('a, Dom.event) listener

(** Bind event listener to the "beforeinput" event. *)
val onBeforeInput : ('a, Dom.inputEvent) listener

(** Bind event listener to the "beforeunload" event. *)
val onBeforeUnload : ('a, Dom.event) listener

(** Bind event listener to the "blur" event. *)
val onBlur : ('a, Dom.focusEvent) listener

(** Bind event listener to the "change" event. *)
val onChange : ('a, Dom.event) listener

(** Bind event listener to the "click" event. *)
val onClick : ('a, Dom.mouseEvent) listener

(** Bind event listener to the "dblclick" event. *)
val onDblClick : ('a, Dom.mouseEvent) listener

(** Bind event listener to the "dragend" event. *)
val onDragEnd : ('a, Dom.dragEvent) listener

(** Bind event listener to the "dragenter" event. *)
val onDragEnter : ('a, Dom.dragEvent) listener

(** Bind event listener to the "dragexit" event. *)
val onDragExit : ('a, Dom.dragEvent) listener

(** Bind event listener to the "dragleave" event. *)
val onDragLeave : ('a, Dom.dragEvent) listener

(** Bind event listener to the "dragover" event. *)
val onDragOver : ('a, Dom.dragEvent) listener

(** Bind event listener to the "dragstart" event. *)
val onDragStart : ('a, Dom.dragEvent) listener

(** Bind event listener to the "drop" event. *)
val onDrop : ('a, Dom.dragEvent) listener

(** Bind event listener to the "focus" event. *)
val onFocus : ('a, Dom.focusEvent) listener

(** Bind event listener to the "focusin" event. *)
val onFocusIn : ('a, Dom.focusEvent) listener

(** Bind event listener to the "focusout" event. *)
val onFocusOut : ('a, Dom.focusEvent) listener

(** Bind event listener to the "input" event. *)
val onInput : ('a, Dom.inputEvent) listener

(** Bind event listener to the "keydown" event. *)
val onKeyDown : ('a, Dom.keyboardEvent) listener

(** Bind event listener to the "keypress" event. *)
val onKeyPress : ('a, Dom.keyboardEvent) listener

(** Bind event listener to the "keyup" event. *)
val onKeyUp : ('a, Dom.keyboardEvent) listener

(** Bind event listener to the "load" event. *)
val onLoad : ('a, Dom.inputEvent) listener

(** Bind event listener to the "mousedown" event. *)
val onMouseDown : ('a, Dom.mouseEvent) listener

(** Bind event listener to the "mouseenter" event. *)
val onMouseEnter : ('a, Dom.mouseEvent) listener

(** Bind event listener to the "mouseleave" event. *)
val onMouseLeave : ('a, Dom.mouseEvent) listener

(** Bind event listener to the "mousemove" event. *)
val onMouseMove : ('a, Dom.mouseEvent) listener

(** Bind event listener to the "mouseout" event. *)
val onMouseOut : ('a, Dom.mouseEvent) listener

(** Bind event listener to the "mouseup" event. *)
val onMouseUp : ('a, Dom.mouseEvent) listener

(** Bind event listener to the "popstate" event. *)
val onPopState : ('a, Dom.event) listener

(** Bind event listener to the "readystatechange" event. *)
val onReadyStateChange : ('a, Dom.event) listener

(** Bind event listener to the "resize" event. *)
val onResize : ('a, Dom.event) listener

(** Bind event listener to the "scroll" event. *)
val onScroll : ('a, Dom.uiEvent) listener

(** Bind event listener to the "select" event. *)
val onSelect : ('a, Dom.event) listener

(** Bind event listener to the "submit" event. *)
val onSubmit : ('a, Dom.event) listener

(** Bind event listener to the "touchcancel" event. *)
val onTouchCancel : ('a, Dom.touchEvent) listener

(** Bind event listener to the "touchend" event. *)
val onTouchEnd : ('a, Dom.touchEvent) listener

(** Bind event listener to the "touchmove" event. *)
val onTouchMove : ('a, Dom.touchEvent) listener

(** Bind event listener to the "touchstart" event. *)
val onTouchStart : ('a, Dom.touchEvent) listener

(** Bind event listener to the "transitioncancel" event. *)
val onTransitionCancel : ('a, Dom.transitionEvent) listener

(** Bind event listener to the "transitionend" event. *)
val onTransitionEnd : ('a, Dom.transitionEvent) listener

(** Bind event listener to the "transitionrun" event. *)
val onTransitionRun : ('a, Dom.transitionEvent) listener

(** Bind event listener to the "transitionstart" event. *)
val onTransitionStart : ('a, Dom.transitionEvent) listener

(** Bind event listener to the "unhandledrejection" event. *)
val onUnhandledRejection : ('a, Dom.event) listener

(** Bind event listener to the "unload" event. *)
val onUnload : ('a, Dom.event) listener

(** Bind event listener to the "wheel" event. *)
val onWheel : ('a, Dom.wheelEvent) listener

(** Set an element property. *)
val prop : string -> string -> 'a t

(** Include directive when element is to be removed and until transition ends. *)
val removeTransition : ?name:string -> 'a t array -> 'a t

(** Set an element style property. *)
val style : ?important:bool -> string -> string -> 'a t

(** Adds a text node. *)
val text : string -> 'a t

(** The lazy function takes a cache key and a function that returns a
    directive, for example a text or virtual node, or a wedge that inserts
    multiple directives. *)
val thunk : ('c -> 'a t) -> 'c -> 'a t

(** A wedge inserts an array of directives at the position, useful for
    example to add an array of children to a node in addition to other
    directives. *)
val wedge : 'a t array -> 'a t
