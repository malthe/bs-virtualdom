open Virtualdom_types

type ('a, 'b) directive = ('a, 'b) t

(** Set an element attribute. *)
val attr : ?namespace:string -> string -> string -> ('a, 'b) t

(** Set an element class. *)
val className : string -> ('a, 'b) t

(** Include a component. *)
val component :
  ('b -> ('c, 'd) t) ->
  (('c -> unit) -> 'c -> 'a) ->
  'b -> ('a, 'b) t

(** Include directive if condition is true. *)
val cond : bool -> ('a, 'b) t -> ('a, 'b) t

(** An empty array of directives *)
val empty : ('a, 'b) t array

(** This function returns a new virtual DOM node. *)
val h :
  ?namespace:string ->
  ?onInsert:(Dom.element -> 'a option) ->
  string -> ('a, 'b) t array -> ('a, 'b) t

(** This function inserts an keyed array of directives. *)
val keyed : (Js.Dict.key * ('a, 'b) t) array -> ('a, 'b) t

(** Include directive if condition is true. *)
val maybe : 'c option -> ('c -> ('a, 'b) t) -> ('a, 'b) t

(** This function mounts the app on an existing DOM element. *)
val mount :
  ?namespace:string ->
  Dom.element ->
  ('a -> ('b, 'c) t array) ->
  ('a -> ('b -> unit) -> 'b -> 'a) -> 'a -> unit

(** Bind event listener to the "abort" event. *)
val onAbort : ('a, 'b, Dom.event) listener

(** Bind event listener to the "beforeinput" event. *)
val onBeforeInput : ('a, 'b, Dom.inputEvent) listener

(** Bind event listener to the "beforeunload" event. *)
val onBeforeUnload : ('a, 'b, Dom.event) listener

(** Bind event listener to the "blur" event. *)
val onBlur : ('a, 'b, Dom.focusEvent) listener

(** Bind event listener to the "change" event. *)
val onChange : ('a, 'b, Dom.event) listener

(** Bind event listener to the "click" event. *)
val onClick : ('a, 'b, Dom.mouseEvent) listener

(** Bind event listener to the "dblclick" event. *)
val onDblClick : ('a, 'b, Dom.mouseEvent) listener

(** Bind event listener to the "dragend" event. *)
val onDragEnd : ('a, 'b, Dom.dragEvent) listener

(** Bind event listener to the "dragenter" event. *)
val onDragEnter : ('a, 'b, Dom.dragEvent) listener

(** Bind event listener to the "dragexit" event. *)
val onDragExit : ('a, 'b, Dom.dragEvent) listener

(** Bind event listener to the "dragleave" event. *)
val onDragLeave : ('a, 'b, Dom.dragEvent) listener

(** Bind event listener to the "dragover" event. *)
val onDragOver : ('a, 'b, Dom.dragEvent) listener

(** Bind event listener to the "dragstart" event. *)
val onDragStart : ('a, 'b, Dom.dragEvent) listener

(** Bind event listener to the "drop" event. *)
val onDrop : ('a, 'b, Dom.dragEvent) listener

(** Bind event listener to the "focus" event. *)
val onFocus : ('a, 'b, Dom.focusEvent) listener

(** Bind event listener to the "focusin" event. *)
val onFocusIn : ('a, 'b, Dom.focusEvent) listener

(** Bind event listener to the "focusout" event. *)
val onFocusOut : ('a, 'b, Dom.focusEvent) listener

(** Bind event listener to the "input" event. *)
val onInput : ('a, 'b, Dom.inputEvent) listener

(** Bind event listener to the "keydown" event. *)
val onKeyDown : ('a, 'b, Dom.keyboardEvent) listener

(** Bind event listener to the "keypress" event. *)
val onKeyPress : ('a, 'b, Dom.keyboardEvent) listener

(** Bind event listener to the "keyup" event. *)
val onKeyUp : ('a, 'b, Dom.keyboardEvent) listener

(** Bind event listener to the "load" event. *)
val onLoad : ('a, 'b, Dom.inputEvent) listener

(** Bind event listener to the "mousedown" event. *)
val onMouseDown : ('a, 'b, Dom.mouseEvent) listener

(** Bind event listener to the "mouseenter" event. *)
val onMouseEnter : ('a, 'b, Dom.mouseEvent) listener

(** Bind event listener to the "mouseleave" event. *)
val onMouseLeave : ('a, 'b, Dom.mouseEvent) listener

(** Bind event listener to the "mousemove" event. *)
val onMouseMove : ('a, 'b, Dom.mouseEvent) listener

(** Bind event listener to the "mouseout" event. *)
val onMouseOut : ('a, 'b, Dom.mouseEvent) listener

(** Bind event listener to the "mouseup" event. *)
val onMouseUp : ('a, 'b, Dom.mouseEvent) listener

(** Bind event listener to the "popstate" event. *)
val onPopState : ('a, 'b, Dom.event) listener

(** Bind event listener to the "readystatechange" event. *)
val onReadyStateChange : ('a, 'b, Dom.event) listener

(** Bind event listener to the "resize" event. *)
val onResize : ('a, 'b, Dom.event) listener

(** Bind event listener to the "scroll" event. *)
val onScroll : ('a, 'b, Dom.uiEvent) listener

(** Bind event listener to the "select" event. *)
val onSelect : ('a, 'b, Dom.event) listener

(** Bind event listener to the "submit" event. *)
val onSubmit : ('a, 'b, Dom.event) listener

(** Bind event listener to the "touchcancel" event. *)
val onTouchCancel : ('a, 'b, Dom.touchEvent) listener

(** Bind event listener to the "touchend" event. *)
val onTouchEnd : ('a, 'b, Dom.touchEvent) listener

(** Bind event listener to the "touchmove" event. *)
val onTouchMove : ('a, 'b, Dom.touchEvent) listener

(** Bind event listener to the "touchstart" event. *)
val onTouchStart : ('a, 'b, Dom.touchEvent) listener

(** Bind event listener to the "transitioncancel" event. *)
val onTransitionCancel : ('a, 'b, Dom.transitionEvent) listener

(** Bind event listener to the "transitionend" event. *)
val onTransitionEnd : ('a, 'b, Dom.transitionEvent) listener

(** Bind event listener to the "transitionrun" event. *)
val onTransitionRun : ('a, 'b, Dom.transitionEvent) listener

(** Bind event listener to the "transitionstart" event. *)
val onTransitionStart : ('a, 'b, Dom.transitionEvent) listener

(** Bind event listener to the "unhandledrejection" event. *)
val onUnhandledRejection : ('a, 'b, Dom.event) listener

(** Bind event listener to the "unload" event. *)
val onUnload : ('a, 'b, Dom.event) listener

(** Bind event listener to the "wheel" event. *)
val onWheel : ('a, 'b, Dom.wheelEvent) listener

(** Set an element property. *)
val prop : string -> string -> ('a, 'b) t

(** Include directive when element is to be removed and until transition ends. *)
val removeTransition : ?name:string -> ('a, 'b) t array -> ('a, 'b) t

(** Set an element style property. *)
val style : ?important:bool -> string -> string -> ('a, 'b) t

(** Adds a text node. *)
val text : string -> ('a, 'b) t

(** The lazy function takes a cache key and a function that returns a
    directive, for example a text or virtual node, or a wedge that inserts
    multiple directives. *)
val thunk : ('c -> ('a, 'b) t) -> 'c -> ('a, 'b) t

(** A wedge inserts an array of directives at the position, useful for
    example to add an array of children to a node in addition to other
    directives. *)
val wedge : ('a, 'b) t array -> ('a, 'b) t
