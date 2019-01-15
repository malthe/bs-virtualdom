type event =
    Abort
  | AnimationCancel
  | AnimationEnd
  | AnimationIteration
  | AnimationStart
  | BeforeCopy
  | BeforeCut
  | BeforeInput
  | BeforePaste
  | BeforeUnload
  | Blur
  | Change
  | Click
  | ClipboardChange
  | Copy
  | Cut
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
  | Paste
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
  | AnimationCancel -> Some "animationcancel"
  | AnimationEnd -> Some "animationend"
  | AnimationIteration -> Some "animationiteration"
  | AnimationStart -> Some "animationstart"
  | BeforeCopy -> Some "beforecopy"
  | BeforeCut -> Some "beforecut"
  | BeforeInput -> Some "beforeinput"
  | BeforePaste -> Some "beforepaste"
  | BeforeUnload -> Some "beforeunload"
  | Blur -> Some "blur"
  | Change -> Some "change"
  | Click -> Some "click"
  | ClipboardChange -> Some "clipboardchange"
  | Copy -> Some "copy"
  | Cut -> Some "cut"
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
  | Paste -> Some "paste"
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
  type t = float

  let (land) : (float -> float -> float) = [%raw {|
    function(a, b) {
      var hi = 0x80000000;
      var hi1 = ~~(a / hi);
      var hi2 = ~~(b / hi);
      var lo1 = a - hi1 * hi;
      var lo2 = b - hi2 * hi;
      var h = hi1 & hi2;
      var l = lo1 & lo2;
      return h * hi + l;
    }
  |}]

  let (lor) : (float -> float -> float) = [%raw {|
    function(a, b) {
      var hi = 0x80000000;
      var hi1 = ~~(a / hi);
      var hi2 = ~~(b / hi);
      var lo1 = a - hi1 * hi;
      var lo2 = b - hi2 * hi;
      var h = hi1 | hi2;
      var l = lo1 | lo2;
      return h * hi + l;
    }
  |}]

  let add a b =
    a lor b

  let empty = float_of_int 0

  let make event =
    let exp = eventToJs event in
    Js.Math.pow_int ~base:2 ~exp |. float_of_int

  let contains event =
    let t = make event in
    fun n ->
      if n >= t then
        n land t != 0.0
      else
        false

  let childEventToParent n =
    if n > 0.0 && contains RemoveSelf n then
      add (n -. make RemoveSelf) (make RemoveChildren)
    else
      n
end
