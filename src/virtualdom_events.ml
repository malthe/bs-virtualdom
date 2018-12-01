type event =
    Abort
  | BeforeInput
  | BeforeUnload
  | Blur
  | Change
  | Click
  | DblClick
  | DragEnd
  | DragEnter
  | DragExit
  | DragLeave
  | DragOver
  | DragStart
  | Drop
  | Focus
  | FocusIn
  | FocusOut
  | Input
  | KeyDown
  | KeyPress
  | KeyUp
  | Load
  | MouseDown
  | MouseEnter
  | MouseLeave
  | MouseMove
  | MouseOut
  | MouseUp
  | PopState
  | ReadyStateChange
  | RemoveChildren
  | RemoveSelf
  | Resize
  | Scroll
  | Select
  | Submit
  | TouchCancel
  | TouchEnd
  | TouchMove
  | TouchStart
  | TransitionCancel
  | TransitionEnd
  | TransitionRun
  | TransitionStart
  | UnhandledRejection
  | Unload
  | Wheel
[@@bs.deriving jsConverter]

let eventName = function
    Abort -> Some "abort"
  | BeforeInput -> Some "beforeinput"
  | BeforeUnload -> Some "beforeunload"
  | Blur -> Some "blur"
  | Change -> Some "change"
  | Click -> Some "click"
  | DblClick -> Some "dblclick"
  | DragEnd -> Some "dragend"
  | DragEnter -> Some "dragenter"
  | DragExit -> Some "dragexit"
  | DragLeave -> Some "dragleave"
  | DragOver -> Some "dragover"
  | DragStart -> Some "dragstart"
  | Drop -> Some "drop"
  | Focus -> Some "focus"
  | FocusIn -> Some "focusin"
  | FocusOut -> Some "focusout"
  | Input -> Some "input"
  | KeyDown -> Some "keydown"
  | KeyPress -> Some "keypress"
  | KeyUp -> Some "keyup"
  | Load -> Some "load"
  | MouseDown -> Some "mousedown"
  | MouseEnter -> Some "mouseenter"
  | MouseLeave -> Some "mouseleave"
  | MouseMove -> Some "mousemove"
  | MouseOut -> Some "mouseout"
  | MouseUp -> Some "mouseup"
  | PopState -> Some "popstate"
  | ReadyStateChange -> Some "readystatechange"
  | Resize -> Some "resize"
  | Scroll -> Some "scroll"
  | Select -> Some "select"
  | Submit -> Some "submit"
  | TouchCancel -> Some "touchcancel"
  | TouchEnd -> Some "touchend"
  | TouchMove -> Some "touchmove"
  | TouchStart -> Some "touchstart"
  | TransitionCancel -> Some "transitioncancel"
  | TransitionEnd -> Some "transitionend"
  | TransitionRun -> Some "transitionrun"
  | TransitionStart -> Some "transitionstart"
  | UnhandledRejection -> Some "unhandledrejection"
  | Unload -> Some "unload"
  | Wheel -> Some "wheel"
  | _ -> None

module EventSet : sig
  type t

  val add : t -> t -> t

  val empty : t

  val make : event -> t

  val contains : event -> t -> bool

  val childEventToParent : t -> t
end = struct
  type t = int

  let add a b = a lor b

  let empty = 0

  let make event = Js.Math.pow_int ~base:2 ~exp:(eventToJs event)

  let contains event =
    let t = make event in
    fun n -> n land t != 0

  let childEventToParent n =
    if contains RemoveSelf n then
      n land (lnot (make RemoveSelf))
      lor (make RemoveChildren)
    else
      n
end
