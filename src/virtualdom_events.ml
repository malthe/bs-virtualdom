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

module EventSet = struct
  type t = int

  let containsBit n i =
    n land (1 lsl i) != 0

  let contains n event =
    containsBit n (eventToJs event)

  let childEventToParent n =
    if contains n RemoveSelf then
      n land (lnot (1 lsl (eventToJs RemoveSelf)))
      lor (1 lsl (eventToJs RemoveChildren))
    else
      n
end
