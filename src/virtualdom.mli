open Virtualdom_types

type ('a, 'b) directive = ('a, 'b) t

(** Set an element attribute. *)
val attr : ?namespace:string -> string -> string -> ('a, 'b) t

(** Set an element class. *)
val className : string -> ('a, 'b) t

(** Include a component. *)
val component : ('b -> ('c, 'd) t) -> ('c -> 'a) -> 'b -> ('a, 'b) t

(** Include directive if condition is true. *)
val cond : bool -> ('a, 'b) t -> ('a, 'b) t

(** An empty array of directives *)
val empty : ('a, 'b) t array

(** This function returns a new virtual DOM node. *)
val h : ?namespace:string -> string -> ('a, 'b) t array -> ('a, 'b) t

(** This function inserts an keyed array of directives. *)
val keyed : (Js.Dict.key * ('a, 'b) t) array -> ('a, 'b) t

(** Include directive if condition is true. *)
val maybe : 'a option -> ('a -> ('a, 'b) t) -> ('a, 'b) t

(** This function mounts the app on an existing DOM element. *)
val mount :
  Dom.element ->
  ('a -> ('b, 'c) t array) ->
  ('a -> ('b -> unit) -> 'b -> 'a) -> 'a -> unit

(** Bind event listener to the "abort" event. *)
val onAbort : (Dom.event -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "beforeinput" event. *)
val onBeforeInput : (Dom.inputEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "beforeunload" event. *)
val onBeforeUnload : (Dom.event -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "blur" event. *)
val onBlur : (Dom.focusEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "change" event. *)
val onChange : (Dom.event -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "click" event. *)
val onClick : (Dom.mouseEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "dblclick" event. *)
val onDblClick : (Dom.mouseEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "dragend" event. *)
val onDragEnd : (Dom.dragEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "dragenter" event. *)
val onDragEnter : (Dom.dragEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "dragexit" event. *)
val onDragExit : (Dom.dragEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "dragleave" event. *)
val onDragLeave : (Dom.dragEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "dragover" event. *)
val onDragOver : (Dom.dragEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "dragstart" event. *)
val onDragStart : (Dom.dragEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "drop" event. *)
val onDrop : (Dom.dragEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "focus" event. *)
val onFocus : (Dom.focusEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "focusin" event. *)
val onFocusIn : (Dom.focusEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "focusout" event. *)
val onFocusOut : (Dom.focusEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "input" event. *)
val onInput : (Dom.inputEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "keydown" event. *)
val onKeyDown : (Dom.keyboardEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "keypress" event. *)
val onKeyPress : (Dom.keyboardEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "keyup" event. *)
val onKeyUp : (Dom.keyboardEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "load" event. *)
val onLoad : (Dom.inputEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "mousedown" event. *)
val onMouseDown : (Dom.mouseEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "mouseenter" event. *)
val onMouseEnter : (Dom.mouseEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "mouseleave" event. *)
val onMouseLeave : (Dom.mouseEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "mousemove" event. *)
val onMouseMove : (Dom.mouseEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "mouseout" event. *)
val onMouseOut : (Dom.mouseEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "mouseup" event. *)
val onMouseUp : (Dom.mouseEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "popstate" event. *)
val onPopState : (Dom.event -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "readystatechange" event. *)
val onReadyStateChange : (Dom.event -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "resize" event. *)
val onResize : (Dom.event -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "scroll" event. *)
val onScroll : (Dom.uiEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "select" event. *)
val onSelect : (Dom.event -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "submit" event. *)
val onSubmit : (Dom.event -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "touchcancel" event. *)
val onTouchCancel : (Dom.touchEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "touchend" event. *)
val onTouchEnd : (Dom.touchEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "touchmove" event. *)
val onTouchMove : (Dom.touchEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "touchstart" event. *)
val onTouchStart : (Dom.touchEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "transitioncancel" event. *)
val onTransitionCancel : (Dom.transitionEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "transitionend" event. *)
val onTransitionEnd : (Dom.transitionEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "transitionrun" event. *)
val onTransitionRun : (Dom.transitionEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "transitionstart" event. *)
val onTransitionStart : (Dom.transitionEvent -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "unhandledrejection" event. *)
val onUnhandledRejection : (Dom.event -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "unload" event. *)
val onUnload : (Dom.event -> 'a option) -> ('a, 'b) t

(** Bind event listener to the "wheel" event. *)
val onWheel : (Dom.wheelEvent -> 'a option) -> ('a, 'b) t

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
