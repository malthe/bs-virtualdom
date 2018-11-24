open Virtualdom_types

(** Set an element attribute. *)
val attr : ?namespace:string -> string -> string -> 'a t

(** Set an element class. *)
val className : string -> 'a t

(** Include directive if condition is true. *)
val cond : bool -> 'a t -> 'a t

(** An empty array of directives *)
val empty : 'a t array

(** This function returns a new virtual DOM tree. *)
val h : ?key:string -> ?namespace:string -> string -> 'a t array -> 'a t

(** Include directive if condition is true. *)
val maybe : 'a option -> ('a -> 'a t) -> 'a t

(** This function mounts the app on an existing DOM element. *)
val mount :
  Dom.element ->
  ('a -> 'b t array) ->
  ('a -> ('b -> unit) -> 'b -> 'a) ->
  'a ->
  unit

(** Bind event listener to the "abort" event. *)
val onAbort : (Dom.event -> 'a) -> 'a t

(** Bind event listener to the "beforeinput" event. *)
val onBeforeInput : (Dom.inputEvent -> 'a) -> 'a t

(** Bind event listener to the "beforeunload" event. *)
val onBeforeUnload : (Dom.event -> 'a) -> 'a t

(** Bind event listener to the "blur" event. *)
val onBlur : (Dom.focusEvent -> 'a) -> 'a t

(** Bind event listener to the "change" event. *)
val onChange : (Dom.event -> 'a) -> 'a t

(** Bind event listener to the "click" event. *)
val onClick : (Dom.mouseEvent -> 'a) -> 'a t

(** Bind event listener to the "dblclick" event. *)
val onDblClick : (Dom.mouseEvent -> 'a) -> 'a t

(** Bind event listener to the "dragend" event. *)
val onDragEnd : (Dom.dragEvent -> 'a) -> 'a t

(** Bind event listener to the "dragenter" event. *)
val onDragEnter : (Dom.dragEvent -> 'a) -> 'a t

(** Bind event listener to the "dragexit" event. *)
val onDragExit : (Dom.dragEvent -> 'a) -> 'a t

(** Bind event listener to the "dragleave" event. *)
val onDragLeave : (Dom.dragEvent -> 'a) -> 'a t

(** Bind event listener to the "dragover" event. *)
val onDragOver : (Dom.dragEvent -> 'a) -> 'a t

(** Bind event listener to the "dragstart" event. *)
val onDragStart : (Dom.dragEvent -> 'a) -> 'a t

(** Bind event listener to the "drop" event. *)
val onDrop : (Dom.dragEvent -> 'a) -> 'a t

(** Bind event listener to the "focus" event. *)
val onFocus : (Dom.focusEvent -> 'a) -> 'a t

(** Bind event listener to the "focusin" event. *)
val onFocusIn : (Dom.focusEvent -> 'a) -> 'a t

(** Bind event listener to the "focusout" event. *)
val onFocusOut : (Dom.focusEvent -> 'a) -> 'a t

(** Bind event listener to the "input" event. *)
val onInput : (Dom.inputEvent -> 'a) -> 'a t

(** Bind event listener to the "keydown" event. *)
val onKeyDown : (Dom.keyboardEvent -> 'a) -> 'a t

(** Bind event listener to the "keypress" event. *)
val onKeyPress : (Dom.keyboardEvent -> 'a) -> 'a t

(** Bind event listener to the "keyup" event. *)
val onKeyUp : (Dom.keyboardEvent -> 'a) -> 'a t

(** Bind event listener to the "load" event. *)
val onLoad : (Dom.inputEvent -> 'a) -> 'a t

(** Bind event listener to the "mousedown" event. *)
val onMouseDown : (Dom.mouseEvent -> 'a) -> 'a t

(** Bind event listener to the "mouseenter" event. *)
val onMouseEnter : (Dom.mouseEvent -> 'a) -> 'a t

(** Bind event listener to the "mouseleave" event. *)
val onMouseLeave : (Dom.mouseEvent -> 'a) -> 'a t

(** Bind event listener to the "mousemove" event. *)
val onMouseMove : (Dom.mouseEvent -> 'a) -> 'a t

(** Bind event listener to the "mouseout" event. *)
val onMouseOut : (Dom.mouseEvent -> 'a) -> 'a t

(** Bind event listener to the "mouseup" event. *)
val onMouseUp : (Dom.mouseEvent -> 'a) -> 'a t

(** Bind event listener to the "popstate" event. *)
val onPopState : (Dom.event -> 'a) -> 'a t

(** Bind event listener to the "readystatechange" event. *)
val onReadyStateChange : (Dom.event -> 'a) -> 'a t

(** Bind event listener to the "resize" event. *)
val onResize : (Dom.event -> 'a) -> 'a t

(** Bind event listener to the "scroll" event. *)
val onScroll : (Dom.uiEvent -> 'a) -> 'a t

(** Bind event listener to the "select" event. *)
val onSelect : (Dom.event -> 'a) -> 'a t

(** Bind event listener to the "submit" event. *)
val onSubmit : (Dom.event -> 'a) -> 'a t

(** Bind event listener to the "touchcancel" event. *)
val onTouchCancel : (Dom.touchEvent -> 'a) -> 'a t

(** Bind event listener to the "touchend" event. *)
val onTouchEnd : (Dom.touchEvent -> 'a) -> 'a t

(** Bind event listener to the "touchmove" event. *)
val onTouchMove : (Dom.touchEvent -> 'a) -> 'a t

(** Bind event listener to the "touchstart" event. *)
val onTouchStart : (Dom.touchEvent -> 'a) -> 'a t

(** Bind event listener to the "transitioncancel" event. *)
val onTransitionCancel : (Dom.transitionEvent -> 'a) -> 'a t

(** Bind event listener to the "transitionend" event. *)
val onTransitionEnd : (Dom.transitionEvent -> 'a) -> 'a t

(** Bind event listener to the "transitionrun" event. *)
val onTransitionRun : (Dom.transitionEvent -> 'a) -> 'a t

(** Bind event listener to the "transitionstart" event. *)
val onTransitionStart : (Dom.transitionEvent -> 'a) -> 'a t

(** Bind event listener to the "unhandledrejection" event. *)
val onUnhandledRejection : (Dom.event -> 'a) -> 'a t

(** Bind event listener to the "unload" event. *)
val onUnload : (Dom.event -> 'a) -> 'a t

(** Bind event listener to the "wheel" event. *)
val onWheel : (Dom.wheelEvent -> 'a) -> 'a t

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
val thunk : ('a -> 'b t) -> 'a -> 'b t

(** A wedge inserts an array of directives at the position, useful for
    example to add an array of children to a node in addition to other
    directives. *)
val wedge : 'a t array -> 'a t

