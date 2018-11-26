open Virtualdom_dom
open Virtualdom_misc
open Virtualdom_types

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

type 'a update =
    Found of ('a t * Dom.node option)
  | Forward of ('a t * 'a t array * int * Dom.node option)
  | Keys of ('a vnode Js.Nullable.t Js.Dict.t * 'a update)
  | Events of (int * 'a update)
  | Match of 'a t
  | New of 'a t

let eventToName = function
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

let browserEvents = [|
  Abort;
  BeforeInput;
  BeforeUnload;
  Blur;
  Change;
  Click;
  DblClick;
  DragEnd;
  DragEnter;
  DragExit;
  DragLeave;
  DragOver;
  DragStart;
  Drop;
  Focus;
  FocusIn;
  FocusOut;
  Input;
  KeyDown;
  KeyPress;
  KeyUp;
  Load;
  MouseDown;
  MouseEnter;
  MouseLeave;
  MouseMove;
  MouseOut;
  MouseUp;
  PopState;
  ReadyStateChange;
  Resize;
  Scroll;
  Select;
  TouchCancel;
  TouchEnd;
  TouchMove;
  TouchStart;
  TransitionCancel;
  TransitionEnd;
  TransitionRun;
  TransitionStart;
  UnhandledRejection;
  Unload;
  Wheel;
|]

let eventListener event f = EventListener (eventToJs event, f)

module L = struct
  let drag ev (f : Dom.dragEvent -> 'a) = eventListener ev f
  let event ev (f: Dom.event -> 'a) = eventListener ev f
  let uiEvent ev (f: Dom.uiEvent -> 'a) = eventListener ev f
  let focus ev (f: Dom.focusEvent -> 'a) = eventListener ev f
  let input ev (f: Dom.inputEvent -> 'a) = eventListener ev f
  let keyboard ev (f : Dom.keyboardEvent -> 'a) = eventListener ev f
  let mouse ev (f: Dom.mouseEvent -> 'a) = eventListener ev f
  let touch ev (f: Dom.touchEvent -> 'a) = eventListener ev f
  let transition ev (f : Dom.transitionEvent -> 'a) = eventListener ev f
  let wheel ev (f: Dom.wheelEvent -> 'a) = eventListener ev f
end

let svgNS = "http://www.w3.org/2000/svg"

let isSVG selector =
  let g = Js.String.get selector in
  let length = String.length selector in
  length >= 3 && g 0 == "s" && g 1 == "v" && g 2 == "g" && (
    length == 3 || g 3 == "." || g 3 == "#"
  )

let rec getNextSibling = function
    Attached { element } -> nextElementSibling element
  | Wedge directives ->
    let rec g i =
      if i = 0 then
        None
      else
        let j = i - 1 in
        match Array.get directives j with
          Attached { element  } -> nextElementSibling element
        | Wedge _ as w -> getNextSibling w
        | _ -> g j
    in
    g @@ Array.length directives
  | _ -> None

let rec search f directives =
  let length = Array.length directives in
  let rec go i =
    if i < length then
      let d = Array.get directives i in
      match f d with
        Some x -> Some x
      | None -> (
          let g directives =
            match search f directives with
              Some x -> Some x
            | None -> go (i + 1)
          in
          match d with
            Wedge directives -> g directives
          | Thunk (_, _, Some d) -> g [| d |]
          | _ -> go (i + 1)
        )
    else
      None
  in
  go 0

let rec notify i event update directives =
  let length = Array.length directives in
  let rec go j =
    if j < length then
      match Array.get directives j with
      | EventListener (k, f) when i = k ->
        update (f (unsafe_identity event))
      | Thunk (_, _, Some d) -> notify i event update [| d |]
      | Wedge d -> notify i event update d
      | _ -> go (j + 1)
    else
      ()
  in
  go 0

let sameKeyOrSelector key namespace selector vnode =
  key = vnode.key &&
  namespace = vnode.namespace &&
  selector = vnode.selector

let hasRemoveEvent n event =
  n land (1 lsl eventToJs event) != 0

let insert parent child next =
  match next with
    Some reference -> insertBefore parent child reference
  | None -> appendElement parent child

let childEventToParent n =
  if hasRemoveEvent n RemoveSelf then
    n land (lnot (1 lsl (eventToJs RemoveSelf)))
    lor (1 lsl (eventToJs RemoveChildren))
  else
    n

let apply vnode element (directives, events) = {
  vnode with
  element;
  events;
  directives;
}

let rec findNextEventTarget element mask directives update =
  let length = Array.length directives in
  let rec go j =
    let next d =
      match findNextEventTarget element mask d update with
        Some _ as result -> result
      | None -> go (j + 1)
    in
    if j < length then
      match Array.get directives j with
        Attached { element = element'; events; directives }
        when element == element' ->
        if events land mask != 0 then
          Some (update, directives)
        else
          None
      | Thunk (_, _, Some d) -> next [| d |]
      | Wedge d -> next d
      | _ -> go (j + 1)
    else
      None
  in go 0

let rec dispatch i event update directives =
  function
    [] -> notify i event update directives
  | x::xs ->
    let mask = 1 lsl i in
    match findNextEventTarget x mask directives update with
      Some (update, directives) ->
      dispatch i event update directives xs
    | None -> ()

and patch ?remove:(removeTransition=false) element defaultNamespace =
  let rec cleanup keys = function
      Attribute (Some ns, name, _) -> removeAttributeNS element ns name
    | Attribute (None, name, _) -> removeAttribute element name
    | ClassName name -> removeClassName element name
    | Property (name, _) -> removeProperty element name
    | Style (name, _, _) -> removeStyle element name
    | Text (Some node, _) -> removeElement element node
    | Attached vnode
      when (
        (* There are two cases here. Either the vnode isn't keyed,
           or we need to check if it was already seen, indicated
           by `None`. *)
        match keys, vnode.key with
          Some keys, Some key -> (
            match Js.Dict.unsafeGet keys key with
              x when x = Js.Nullable.null -> false
            | _ -> true
          )
        | _ -> true
      ) ->
      removeVNode
        element
        vnode.element
        vnode.namespace
        vnode.directives
        vnode.events
        (fun () -> ())
    | Wedge directives ->
      let length = Array.length directives in
      for i = 0 to length - 1 do
        cleanup keys (Array.get directives i)
      done
    | _ -> ()
  in

  let rec go
      insertionPoint
      removeTransition
      keys
      events
      oldDirectives
      newDirectives =
    let updatedDirectives = Array.make (Array.length newDirectives) Skip in
    let events =
      fold_lefti
        (fun (directives, i, next, keys, events) j current ->
           let matchNext f =
             if i < Array.length directives then
               let d = Array.get directives i in
               match f (Some d) with
                 New x -> cleanup keys d; Match x
               | x -> x
             else
               f None
           in
           let scan () =
             let keys = Js.Dict.empty () in
             let rec go directives i =
               for j = i to Array.length directives - 1 do
                 match Array.get directives j with
                   Attached ({ key = Some key } as vnode) ->
                   Js.Dict.set keys key (Js.Nullable.return vnode)
                 | Wedge directives -> go directives 0
                 | _ -> ()
               done
             in
             go directives i;
             keys
           in
           let update =
             function
               Attribute (ns, name, value) as d ->
               matchNext (
                 function
                   Some (Attribute (ns', name', value'))
                   when ns = ns' &&
                        name = name' &&
                        value = value' -> Match d
                 | _ -> (
                     match ns with
                       Some ns -> setAttributeNS element ns name value
                     | None -> setAttribute element name value
                   );
                   New d
               )
             | ClassName name as d ->
               matchNext (
                 function
                   Some (ClassName name') when name = name' -> Match d
                 | _ -> addClassName element name; New d
               )
             | EventListener (n, _) as d ->
               Events (
                 1 lsl n,
                 matchNext (
                   function
                     Some (EventListener (n', _))
                     when n = n' -> Match d
                   | _ -> New d
                 )
               )
             | RemoveTransition (name, directives, active) as d ->
               if removeTransition then (
                 let updated, events =
                   go next active keys events empty directives in
                 Events (
                   events,
                   Found (
                     RemoveTransition (name, updated, true),
                     getNextSibling (
                       Wedge directives
                     )
                   )
                 )
               ) else (
                 Events (
                   1 lsl eventToJs RemoveSelf,
                   Match d
                 )
               )
             | Property (name, value) as d ->
               matchNext (
                 function
                   Some (Property (name', value'))
                   when name = name' && value = value' -> Match d
                 | _ -> setProperty element name value; New d
               )
             | Skip as d ->
               matchNext (
                 function
                   Some Skip -> Match d
                 | _ -> New d
               )
             | Style (name, value, important) as d ->
               matchNext (
                 function
                   Some (Style (name', value', important'))
                   when name = name' && value = value' &&
                        important = important' ->
                   Match d
                 | _ ->
                   setStyle element name value important;
                   New d
               )
             | Text (_, string) ->
               matchNext (
                 function
                   Some (Text (Some oldTextNode, string')) ->
                   if string <> string' then
                     setTextContent oldTextNode string;
                   Found (
                     Text (Some oldTextNode, string),
                     nextTextSibling oldTextNode
                   )
                 | _ ->
                   let child = createTextNode string in
                   insert element child next;
                   New (
                     Text (Some child, string)
                   )
               )
             | Thunk (state, fn, _) ->
               matchNext (
                 function
                   Some (
                     Thunk (state', _, Some d) as thunk
                   ) when state == unsafe_identity state' ->
                   Found (
                     thunk,
                     getNextSibling d
                   )
                 | t ->
                   let d = fn state in
                   let getDirectives = function
                       Wedge d -> d
                     | x -> [| x |]
                   in
                   let make xs =
                     let result, events =
                       go next
                         removeTransition keys events xs (getDirectives d)
                     in
                     events, (
                       if Array.length result > 1
                       then
                         Wedge result
                       else
                         Array.get result 0
                     ) in (
                     match t with
                       Some (Thunk (_, _, Some d)) ->
                       let events, d = make (getDirectives d) in
                       Events (
                         events,
                         Found (
                           Thunk (state, fn, Some d),
                           getNextSibling d
                         )
                       )
                     | _ ->
                       let events, d = make empty in
                       Events (
                         events,
                         New (
                           Thunk (state, fn, Some d)
                         )
                       )
                   )
               )
             | Attached _ as d -> Match d
             | Detached (Some key, namespace, selector, newDirectives) ->
               let keys = match keys with
                   Some keys -> keys
                 | None -> scan ()
               in (
                 match Js.Dict.get keys key >>! Js.Nullable.toOption with
                   Some ({ element = child;
                           directives = oldDirectives;
                         } as vnode)
                   when
                     sameKeyOrSelector
                       (Some key) namespace selector vnode ->
                   let sibling = nextElementSibling child in
                   insert element child next;
                   Js.Dict.set keys key Js.Nullable.null;
                   let vnode =
                     patch child namespace oldDirectives newDirectives
                     |> apply vnode child in
                   Events (
                     childEventToParent vnode.events,
                     Keys (
                       keys,
                       Forward (
                         Attached vnode,
                         empty,
                         0,
                         sibling
                       )
                     )
                   )
                 | _ ->
                   let vnode = create element next
                       (Some key) (namespace =| defaultNamespace)
                       selector newDirectives in
                   Events (
                     childEventToParent vnode.events,
                     Forward (
                       Attached vnode,
                       empty,
                       0,
                       None
                     )
                   )
               )
             | Detached (key, namespace, selector, newDirectives) ->
               matchNext (
                 function
                   Some (
                     Attached ({
                         element = child;
                         directives;
                       } as vnode)
                   ) when
                     sameKeyOrSelector
                       key namespace selector vnode ->
                   let vnode =
                     patch child namespace directives newDirectives
                     |> apply vnode child
                   in
                   Events (
                     childEventToParent vnode.events,
                     Found (
                       Attached vnode,
                       nextElementSibling child
                     )
                   )
                 | _ ->
                   let vnode =
                     create element next key
                       (namespace =| defaultNamespace)
                       selector newDirectives in
                   Events (
                     childEventToParent vnode.events,
                     New (Attached vnode)
                   )
               )
             | Wedge xs ->
               matchNext (
                 function
                   Some (Wedge xs') ->
                   let keys = Js.Dict.empty () in
                   let updated, events = go next
                       removeTransition (Some keys) events xs' xs in
                   let d = Wedge updated in
                   Events (events, Found (d, getNextSibling d))
                 | _ ->
                   let updated, events = go next
                       removeTransition keys events empty xs in
                   Events (events, New (Wedge updated))
               )
           in
           let set = Array.set updatedDirectives j in
           let rec go events keys = function
               Events (events', u) -> go (events lor events') keys u
             | Forward (d, xs, i, next) -> set d; (xs, i, next, keys, events)
             | Found (d, node) -> set d; (directives, i + 1, node, keys, events)
             | Keys (keys, u) -> go events (Some keys) u
             | Match d -> set d; (directives, i + 1, next, keys, events)
             | New d -> set d; (directives, i, next, keys, events)
           in
           go events keys @@ update current
        ) (oldDirectives, 0, insertionPoint, keys, events) newDirectives
      |>
      fun (directives, i, _, keys, events) -> (
        for j = i to Array.length directives - 1 do
          cleanup keys @@ Array.get directives j
        done;
        events
      )
    in
    updatedDirectives, events
  in
  go None removeTransition None 0

and create parent next key namespace selector directives =
  let namespace = match namespace with
      Some ns -> Some ns
    | None ->
      if isSVG selector then Some svgNS else None
  in
  let element = createElement namespace selector in
  insert parent element next;
  let (directives, events) =
    patch element namespace [||] directives in {
    element;
    key;
    namespace;
    selector;
    directives;
    events;
  }

and removeVNodeNested parent directives callback =
  let count = ref 0 in
  let rm () =
    let i = !count - 1 in
    if i = 0 then (
      callback ();
    ) else
      count := i;
  in
  let rec go parent directives =
    Array.fold_left
      (fun count d ->
         match d with
           Attached vnode ->
           count + (
             if hasRemoveEvent vnode.events RemoveSelf then
               let () =
                 removeVNode
                   parent
                   vnode.element
                   vnode.namespace
                   vnode.directives
                   vnode.events
                   rm in
               1
             else (
               if hasRemoveEvent vnode.events RemoveChildren then
                 go vnode.element vnode.directives
               else
                 0
             )
           )
         | _ -> count
      ) 0 directives
  in
  count := go parent directives

and removeVNodeOwnTransition element namespace directives callback =
  match eventToName TransitionEnd with
    Some name ->
    let directives, events =
      patch
        ~remove:true
        element
        namespace
        directives
        directives
    in
    let property = search (
        function
          RemoveTransition (Some name, _, true) -> Some name
        | _ -> None
      ) directives in
    let target = Webapi.Dom.Element.asEventTarget element in
    let options = [%bs.obj {
      capture = true;
      passive = true;
      once = false;
    }] in
    let r = ref None in
    let handler event =
      if (
        match property with
          Some name ->
          Webapi.Dom.TransitionEvent.propertyName
            (unsafe_identity event) = name
        | None -> true
      ) then (
        !r >>? fun f -> removeEventListener target name f options;
        if hasRemoveEvent events RemoveSelf then
          removeVNodeOwnTransition element namespace directives callback
        else
          callback ()
      )
    in
    addEventListener target name handler options;
    r := Some handler
  | None -> callback ()

and removeVNode parent child namespace directives events callback =
  let remove () =
    removeElement parent child;
    callback ()
  in
  let next () =
    if hasRemoveEvent events RemoveSelf then
      removeVNodeOwnTransition child namespace directives remove
    else
      remove ()
  in
  if hasRemoveEvent events RemoveChildren then
    removeVNodeNested child directives next
  else
    next ()

let start element selector =
  let listeners = Array.make (Array.length browserEvents) None in
  let patch vnode update directives =
    let events = vnode.events in
    let vnode =
      patch element vnode.namespace vnode.directives directives
      |> apply vnode element
    in
    let () =
      if events != vnode.events then
        let options = [%bs.obj {
          capture = true;
          passive = true;
          once = false;
        }] in
        let open Webapi.Dom in
        for i = 0 to Array.length browserEvents - 1 do
          let event = Array.get browserEvents i in
          let mask = 1 lsl (eventToJs event) in
          let before = events land mask != 0 in
          let after = vnode.events land mask != 0 in
          if before <> after then (
            let target = Element.asEventTarget element in
            let name = eventToName event in
            match name with
              Some name ->
              if before then (
                match Array.get listeners i with
                  Some f -> removeEventListener target name f options
                | None -> ()
              ) else
                let f =
                  fun ev ->
                    Event.stopPropagation ev;
                    let parents =
                      let rec go child =
                        match Element.parentElement child with
                        | Some parent
                          when parent == element -> [child]
                        | Some parent -> child::(go parent)
                        | None -> []
                      in
                      go @@ (EventTarget.unsafeAsElement @@ Event.target ev)
                    in
                    dispatch i ev update vnode.directives (List.rev parents)
                in
                addEventListener target name f options;
                Array.set listeners i (Some f)
            | None -> ()
          )
        done
    in vnode
    in
    let vnode = {
      events = 0;
      key = None;
      selector;
      element;
      directives = empty;
      namespace = None;
    } in
    let r = ref vnode in
    fun view update state ->
      let s = ref state in
      let rec go () =
        r := patch !r controller @@ view !s
      and controller message =
        s := update !s controller message;
        go ()
      in
      go ()

module Export = struct
  let attr ?namespace name value =
    Attribute (namespace, name, value)

  let className name =
    ClassName name

  let cond b directive = if b then directive else Skip

  let empty = [||]

  let h ?key ?namespace selector directives =
    Detached (key, namespace, selector, directives)

  let maybe opt f = match opt with
      Some x -> f x
    | None -> Skip

  let mount element =
    let join get prefix f s =
      let t = get element in
      if Js.String.length t > 0 then
        s ^ prefix ^ f t
      else
        s
    in
    let tag = getTagName element |> Js.String.toLowerCase in
    let selector =
      tag
      |> join getId "#" (fun id -> id)
      |> join getClassName "." (
        fun name ->
          Js.String.split name " " |>
          Js.Array.joinWith "."
      )
    in
    start element selector

  let removeTransition ?name directives =
    RemoveTransition (name, directives, false)

  let prop name value =
    Property (name, value)

  let style ?important:(important = false) name value =
    Style (name, value, important)

  let text string =
    Text (None, string)

  let thunk fn key =
    Thunk (key, fn, None)

  let wedge directives =
    Wedge directives

  let onAbort f = L.event Abort f
  let onBeforeInput f = L.input BeforeInput f
  let onBeforeUnload f = L.event BeforeUnload f
  let onBlur f = L.focus Blur f
  let onChange f = L.event Change f
  let onClick f = L.mouse Click f
  let onDblClick f = L.mouse DblClick f
  let onDragEnd f = L.drag DragEnd f
  let onDragEnter f = L.drag DragEnter f
  let onDragExit f = L.drag DragExit f
  let onDragLeave f = L.drag DragLeave f
  let onDragOver f = L.drag DragOver f
  let onDragStart f = L.drag DragStart f
  let onDrop f = L.drag Drop f
  let onFocus f = L.focus Focus f
  let onFocusIn f = L.focus FocusIn f
  let onFocusOut f = L.focus FocusOut f
  let onInput f = L.input Input f
  let onKeyDown f = L.keyboard KeyDown f
  let onKeyPress f = L.keyboard KeyPress f
  let onKeyUp f = L.keyboard KeyUp f
  let onLoad f = L.input Load f
  let onMouseDown f = L.mouse MouseDown f
  let onMouseEnter f = L.mouse MouseEnter f
  let onMouseLeave f = L.mouse MouseLeave f
  let onMouseMove f = L.mouse MouseMove f
  let onMouseOut f = L.mouse MouseOut f
  let onMouseUp f = L.mouse MouseUp f
  let onPopState f = L.event PopState f
  let onReadyStateChange f = L.event ReadyStateChange f
  let onResize f = L.event Resize f
  let onScroll f = L.uiEvent Scroll f
  let onSelect f = L.event Select f
  let onSubmit f = L.event Submit f
  let onTouchCancel f = L.touch TouchCancel f
  let onTouchEnd f = L.touch TouchEnd f
  let onTouchMove f = L.touch TouchMove f
  let onTouchStart f = L.touch TouchStart f
  let onTransitionCancel f = L.transition TransitionCancel f
  let onTransitionEnd f = L.transition TransitionEnd f
  let onTransitionRun f = L.transition TransitionRun f
  let onTransitionStart f = L.transition TransitionStart f
  let onUnhandledRejection f = L.event UnhandledRejection f
  let onUnload f = L.event Unload f
  let onWheel f = L.wheel Wheel f
end
