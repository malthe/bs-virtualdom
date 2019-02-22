open Virtualdom_dom
open Virtualdom_misc
open Virtualdom_events
open Virtualdom_types

external unsafeEvent : Dom.event -> 'a Dom.event_like = "%identity"
external unsafeDirective : 'a t -> 'b t = "%identity"
external safeIdentity : 'a -> 'a = "%identity"
external nodeOfElement : Dom.element -> Dom.node = "%identity"
external nodeOfText : Dom.text -> Dom.node = "%identity"

let browserEvents = [|
  Abort;
  AnimationCancel;
  AnimationEnd;
  AnimationIteration;
  AnimationStart;
  BeforeCopy;
  BeforeCut;
  BeforeInput;
  BeforePaste;
  BeforeUnload;
  Blur;
  Change;
  Click;
  ClipboardChange;
  Copy;
  Cut;
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
  Paste;
  PopState;
  ReadyStateChange;
  Resize;
  Scroll;
  Select;
  Submit;
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

let eventListener f event passive =
  EventListener (EventSet.make event, passive, f)

let svgNS = "http://www.w3.org/2000/svg"

let isSVG selector =
  let g = Js.String.get selector in
  let length = String.length selector in
  length >= 3 && g 0 == "s" && g 1 == "v" && g 2 == "g" && (
    length == 3 || g 3 == "." || g 3 == "#"
  )

let rec getLastElement = function
    Attached { element } -> Some (nodeOfElement element)
  | Wedge (directives, _) ->
    let rec g i =
      if i = 0 then
        None
      else
        let j = i - 1 in
        match Array.get directives j with
          Attached { element  } -> Some (nodeOfElement element)
        | Wedge _ as w -> getLastElement w
        | _ -> g j
    in
    g @@ Array.length directives
  | _ -> None

let sameVNode namespace selector vnode =
  namespace = vnode.namespace &&
  selector = vnode.selector

let arrayOf = function
    Wedge (d, _) -> d
  | x -> [| x |]

let ofArray array enabledEvents passiveEvents =
  if Array.length array != 1
  then
    Wedge (array, Some (enabledEvents, passiveEvents))
  else
    Array.get array 0

let insert parent child = function
    Some insertAfter -> (
      match nextSibling insertAfter with
        Some sibling ->
        if not (strictly_equal_to child sibling) then
          insertBefore parent child sibling
      | None -> appendElement parent child
    )
  | None ->
    if not (
        strictly_equal_to
          (Webapi.Dom.Element.firstChild parent) child)
    then
      prepend parent child

let rec insertNested parent directives reference =
  Array.fold_left
    (fun reference d ->
       let rec go =
         function
           Attached { element } ->
           insert parent element reference;
           Some (nodeOfElement element)
         | Text (Some node, _) ->
           insert parent node reference;
           Some (nodeOfText node)
         | Thunk (_, _, Some (d, _, _)) -> go d
         | Wedge (directives, _) ->
           insertNested parent directives reference
         | _ -> reference
       in go d
    )
    reference directives

let notifier update handler =
  let update message =
    let rec notify message =
      handler notify message |. update
    in
    handler notify message |. update in
  update

let rec dispatch
  : 'a.
    (EventSet.t -> EventSet.t -> bool) ->
    Dom.event ->
    ('a -> unit) ->
    'a t array ->
    Dom.element list ->
    unit =
  fun check ev update directives children ->
    Array.iter
      (function
          Attached {
            directives;
            element;
            enabledEvents;
            passiveEvents;
          }
          when check enabledEvents passiveEvents -> (
            match children with
              x::xs when x == element ->
              dispatch check ev update directives xs
            | _ -> ()
          )
        | Component (_, handler, _, Some (d, enabledEvents, passiveEvents))
          when check enabledEvents passiveEvents ->
          let update = notifier update handler in
          dispatch check ev update [| d |] children
        | EventListener (event, passive, f)
          when check event (if passive then event else EventSet.empty) ->
          f (unsafeEvent ev) >>?
          update
        | Index array ->
          let directives = Array.map snd array in
          dispatch check ev update directives children
        | Thunk (_, _, Some (d, enabledEvents, passiveEvents))
          when check enabledEvents passiveEvents ->
          dispatch check ev update [| d |] children
        | Wedge (directives, Some (enabledEvents, passiveEvents))
          when check enabledEvents passiveEvents ->
          dispatch check ev update directives children
        | _ -> ()
      ) directives

let rec patch
  : 'a.
    ?alwaysReorder:bool ->
    ?enableRemoveTransitions:bool ->
    ?notifyRemoveTransitions:string option list ->
    ?insertAfter:Dom.node ->
    Dom.element ->
    string option ->
    ('a -> unit) ->
    'a t array ->
    'a t array -> (
      'a t array *
      Dom.node option *
      EventSet.t *
      EventSet.t *
      string option list *
      (unit -> unit) list
    ) =
  fun
    ?alwaysReorder:(alwaysReorder=false)
    ?enableRemoveTransitions:(enableRemoveTransitions=false)
    ?notifyRemoveTransitions:(notifyRemoveTransitions=[])
    ?insertAfter
    element defaultNamespace notify ->
    let rec cleanup = function
        Attribute (Some ns, name, _) -> removeAttributeNS element ns name
      | Attribute (None, name, _) -> removeAttribute element name
      | ClassName name -> removeClassName element name
      | Component (_, handler, _, Some (d, _, _)) ->
        patch
          ~enableRemoveTransitions:true
          element
          defaultNamespace
          (notifier notify handler)
          (arrayOf d)
          empty
        |> ignore
      | Index d ->
        patch
          ~enableRemoveTransitions:true
          element
          defaultNamespace
          notify
          (Array.map snd d)
          empty
        |> ignore
      | Property (name, _) ->
        setProperty element name "";
        removeProperty element name
      | Style (name, _, _) -> removeStyle element name
      | Text (Some node, _) -> removeElement element node
      | Thunk (_, _, Some (d, _, _)) -> cleanup d
      | Attached vnode ->
        removeVNode
          vnode.element
          vnode.namespace
          vnode.directives
          (EventSet.contains RemoveSelf vnode.enabledEvents)
          (EventSet.contains RemoveChildren vnode.enabledEvents)
          (fun () -> (
               match vnode.onRemove with
                 Some f -> f
               | None -> removeElement
             ) element vnode.element)
      | Wedge (directives, _) ->
        let length = Array.length directives in
        for i = 0 to length - 1 do
          cleanup (Array.get directives i)
        done
      | _ -> ()

    and go1
        alwaysReorder
        insertAfter
        enableRemoveTransitions
        oldDirectives
        newDirectives =
      let updatedDirectives = Array.make (Array.length newDirectives) Skip in
      let directives, i, insertAfter,
          enabledEvents, passiveEvents, removeTransitions, callbacks =
        fold_lefti
          (fun (directives, i, insertAfter,
                enabledEvents, passiveEvents, removeTransitions, callbacks)
            j current ->
            let set = Array.set updatedDirectives j in
            let existing =
              if Array.length directives > i then
                Array.get directives i
              else
                Skip
            in
            let go enabledEvents passiveEvents callbacks = function
                (d, enabledEvents', passiveEvents',
                 insertAfter, isNew, newRemoveTransitions, callbacks') ->
                let enabledEvents = EventSet.add enabledEvents enabledEvents' in
                let passiveEvents = EventSet.add passiveEvents passiveEvents' in
                let callbacks = callbacks' @ callbacks in
                if isNew then
                  cleanup existing;
                set d;
                let removeTransitions =
                  removeTransitions @ newRemoveTransitions in
                (directives, i + 1, insertAfter, enabledEvents, passiveEvents,
                 removeTransitions, callbacks)
            in
            go enabledEvents passiveEvents callbacks @@
            update alwaysReorder enableRemoveTransitions existing
              insertAfter current
          )
          (oldDirectives, 0, insertAfter,
           EventSet.empty, EventSet.empty, [], [])
          newDirectives
      in
      for j = i to Array.length directives - 1 do
        Array.get directives j |. cleanup
      done;
      updatedDirectives, insertAfter, enabledEvents,
      passiveEvents, removeTransitions, callbacks

    and go2
        insertAfter
        enableRemoveTransitions
        oldIndex
        newIndex =
      let keys = Js.Dict.fromArray oldIndex in
      let reverse =
        Array.mapi (fun i key -> (key, i)) (Js.Dict.keys keys)
        |> Js.Dict.fromArray
      in
      let oldIndex = Array.copy oldIndex in
      let updatedIndex = [||] in

      let insertAfter, enabledEvents, passiveEvents, removeTransitions, callbacks =
        fold_lefti
          (fun (insertAfter, enabledEvents,
                passiveEvents, removeTransitions, callbacks) j (key, current) ->
            let set value = Array.unsafe_set updatedIndex j (key, value) in
            let existing =
              match Js.Dict.get keys key with
                Some value ->
                Js.Dict.get reverse key >>?
                (fun i -> Array.set oldIndex i (key, Skip));
                value
              | None -> Skip
            in
            let go enabledEvents passiveEvents callbacks = function
                (d, enabledEvents', passiveEvents',
                 insertAfter, isNew, newRemoveTransitions, callbacks') ->
                let enabledEvents = EventSet.add enabledEvents enabledEvents' in
                let passiveEvents = EventSet.add passiveEvents passiveEvents' in
                let callbacks = callbacks' @ callbacks in
                if isNew then
                  cleanup existing;
                set d;
                let removeTransitions =
                  removeTransitions @ newRemoveTransitions
                in
                (insertAfter, enabledEvents, passiveEvents, removeTransitions, callbacks)
            in
            update
              true
              enableRemoveTransitions
              existing
              insertAfter
              current
            |> go enabledEvents passiveEvents callbacks
          ) (insertAfter, EventSet.empty, EventSet.empty, [], []) newIndex
      in
      Array.iter cleanup (Array.map snd oldIndex);
      updatedIndex, insertAfter, enabledEvents,
      passiveEvents, removeTransitions, callbacks

    and update alwaysReorder enableRemoveTransitions next insertAfter =
      let simple d isNew =
        (d, EventSet.empty, EventSet.empty, insertAfter, isNew, [], [])
      in
      function
        Attribute (ns, name, value) as d ->
        let set () =
          match ns with
            Some ns -> setAttributeNS element ns name value
          | None -> setAttribute element name value
        in (
          match next with
            Attribute (ns', name', value')
            when ns = ns' &&
                 name = name' ->
            if value = value' then
              simple d false
            else (
              set (); simple d false
            )
          | _ -> (
              set (); simple d true
            )
        )
      | ClassName name as d -> (
          match next with
            ClassName name' when name = name' -> simple d false
          | _ -> addClassName element name; simple d true
        )
      | Component (view, handler, state, _) -> (
          match next with
            Component (view', _, state',
                       Some (d, enabledEvents, passiveEvents)) as component
            when strictly_equal_to view view' ->
            if strictly_equal_to state state' then
              let insertAfter =
                if alwaysReorder then
                  insertNested element [| d |] insertAfter
                else
                  getLastElement d
              in
              (component, enabledEvents, passiveEvents, insertAfter, false, [], [])
            else
              let result, insertAfter,
                  enabledEvents, passiveEvents, removeTransitions,
                  callbacks =
                patch
                  ~enableRemoveTransitions
                  ~alwaysReorder
                  ?insertAfter
                  element
                  defaultNamespace
                  (notifier notify handler)
                  (arrayOf (unsafeDirective d))
                  (view state |. arrayOf)
              in
              let d = ofArray result enabledEvents passiveEvents in
              let directive = Component (
                  view, handler, state, Some (d, enabledEvents, passiveEvents)
                ) in
              (directive, enabledEvents, passiveEvents,
               insertAfter, false, removeTransitions, callbacks)
          | _ ->
            let result, insertAfter,
                enabledEvents, passiveEvents, removeTransitions, callbacks =
              patch
                ~enableRemoveTransitions
                ~alwaysReorder
                ?insertAfter
                element
                defaultNamespace
                (notifier notify handler)
                empty
                (view state |. arrayOf)
            in
            let d = ofArray result enabledEvents passiveEvents in
            let directive = Component (
                view, handler, state, Some (d, enabledEvents, passiveEvents)
              ) in
            (directive, enabledEvents, passiveEvents,
             insertAfter, true, removeTransitions, callbacks)
        )
      | EventListener (event, passive, _) as d ->
        (d, event, (if passive then event else EventSet.empty),
         insertAfter, (
           match next with
             EventListener (event', passive', _)
             when event = event' &&
                  passive = passive' -> false
           | _ -> true
         ),
         [],
         []
        )
      | Index d -> (
          match next with
            Index d' ->
            let updated, insertAfter,
                enabledEvents, passiveEvents, removeTransitions, callbacks =
              go2 insertAfter enableRemoveTransitions d' d
            in
            (Index updated,
             enabledEvents, passiveEvents,
             insertAfter, false, removeTransitions, callbacks)
          | _ ->
            let directives = Array.map snd d in
            let updated, insertAfter,
                enabledEvents, passiveEvents, removeTransitions, callbacks =
              go1 false insertAfter
                enableRemoveTransitions empty directives in
            let d =
              Array.mapi
                (fun i directive -> (Array.get d i |. fst, directive))
                updated
            in
            (Index d,
             enabledEvents, passiveEvents,
             insertAfter, true, removeTransitions, callbacks)
        )
      | Property (name, value) as d -> (
          match next with
            Property (name', value')
            when name = name' ->
            if value = value' then
              simple d false
            else (
              setProperty element name value;
              simple d false
            )
          | _ ->
            setProperty element name value; simple d true
        )
      | RemoveTransition (name, directives, _) as d -> (
          match next with
            RemoveTransition (name', directives', active) ->
            if enableRemoveTransitions && not active ||
               name = name' &&
               List.mem name notifyRemoveTransitions
            then (
              let updated, insertAfter,
                  enabledEvents, passiveEvents, removeTransitions, callbacks =
                go1 alwaysReorder
                  insertAfter true (
                  if active && name = name' then
                    directives'
                  else
                    empty
                ) directives in (
                RemoveTransition (name, updated, true),
                enabledEvents,
                passiveEvents,
                insertAfter,
                false,
                (if not active then
                   name::removeTransitions
                 else
                   removeTransitions),
                callbacks
              )
            ) else
              (d, EventSet.make RemoveSelf, EventSet.empty,
               insertAfter, false, [], [])
          | _ ->
            if enableRemoveTransitions then (
              let updated, insertAfter,
                  enabledEvents, passiveEvents, removeTransitions, callbacks =
                go1 alwaysReorder
                  insertAfter false
                  empty directives in (
                RemoveTransition (name, updated, true),
                enabledEvents, passiveEvents,
                insertAfter,
                true,
                name::removeTransitions,
                callbacks
              )
            ) else
              (
              d, EventSet.make RemoveSelf, EventSet.empty,
              insertAfter, true, [], []
            )
        )
      | Skip as d -> simple d (
          match next with
            Skip -> false
          | _ -> true
        )
      | Style (name, value, important) as d -> (
          match next with
            Style (name', value', important')
            when name = name' ->
            if value = value' &&
               important = important' then
              simple d false
            else (
              setStyle element name value important;
              simple d false
            )
          | _ ->
            setStyle element name value important;
            simple d true
        )
      | Text (_, string) -> (
          match next with
            Text (Some oldTextNode, string') ->
            if string <> string' then
              setTextContent oldTextNode string;
            if alwaysReorder then
              insert element oldTextNode insertAfter;
            (Text (Some oldTextNode, string),
             EventSet.empty,
             EventSet.empty,
             Some (nodeOfText oldTextNode), false, [], [])
          | _ ->
            let child = createTextNode string in
            insert element child insertAfter;
            (Text (Some child, string),
             EventSet.empty,
             EventSet.empty,
             Some (nodeOfText child), true, [], [])
        )
      | Thunk (state, fn, _) -> (
          match next with
            Thunk (
              state', fn', Some (d, enabledEvents, passiveEvents)
            ) as thunk
            when
              strictly_equal_to fn fn' &&
              strictly_equal_to state state' ->
            let insertAfter =
              if alwaysReorder then
                insertNested element [| d |] insertAfter
              else
                getLastElement d
            in
            (thunk, enabledEvents, passiveEvents, insertAfter, false, [], [])
          | t ->
            let d = fn state in
            let isNew, directives =
              match t with
                Thunk (_, _, Some (d, _, _)) -> false, arrayOf d
              | _ -> true, empty
            in
            let result, insertAfter,
                enabledEvents, passiveEvents, removeTransitions, callbacks =
              go1
                alwaysReorder
                insertAfter
                enableRemoveTransitions directives (arrayOf d)
            in
            let updated = ofArray result enabledEvents passiveEvents in
            (Thunk (
                state,
                fn,
                Some (updated, enabledEvents, passiveEvents)),
             enabledEvents,
             passiveEvents,
             insertAfter, isNew, removeTransitions, callbacks)
        )
      | Attached { detached = Some d } ->
        update alwaysReorder enableRemoveTransitions next insertAfter d
      | Attached _ -> simple Skip true
      | Detached (namespace, selector, onInsert, onRemove, newDirectives)
        as detached -> (
          match next with
            Attached ({
                element = child;
                directives = oldDirectives;
                detached = Some detached';
              } as attached) as t
            when
              detached == detached' ||
              sameVNode namespace selector attached ->
            if alwaysReorder then
              insert element child insertAfter;
            if detached == detached' then
              (t,
               EventSet.childEventToParent attached.enabledEvents,
               attached.passiveEvents,
               Some (nodeOfElement child), false, [], [])
            else
              let directives, _,
                  enabledEvents, passiveEvents, removeTransitions, callbacks =
                patch child namespace notify oldDirectives newDirectives
              in
              (Attached {
                  attached with
                  element = child;
                  directives;
                  enabledEvents;
                  passiveEvents;
                  onRemove;
                },
               EventSet.childEventToParent enabledEvents,
               passiveEvents,
               Some (nodeOfElement child),
               false,
               removeTransitions,
               callbacks
              )
          | _ ->
            let namespace = match (namespace =| defaultNamespace) with
                Some ns -> Some ns
              | None ->
                if isSVG selector then Some svgNS else None
            in
            let child = createElement namespace selector in
            insert element child insertAfter;
            let directives, _, enabledEvents, passiveEvents, _, callbacks =
              patch child namespace notify empty newDirectives
            in
            let vnode = {
              element = child;
              namespace;
              selector;
              directives;
              detached = Some detached;
              enabledEvents;
              passiveEvents;
              onRemove;
            } in
            (Attached vnode,
             EventSet.childEventToParent enabledEvents,
             passiveEvents,
             Some (nodeOfElement child),
             true, [],
             match onInsert with
               Some f -> (fun () -> f child >>? notify)::callbacks
             | None -> callbacks)
        )
      | Wedge (xs, _) ->
        let isNew, directives = match next with
            Wedge (directives, _) -> false, directives
          | _ -> true, empty
        in
        let updated, insertAfter,
            enabledEvents, passiveEvents, removeTransitions, callbacks =
          go1 alwaysReorder insertAfter
            enableRemoveTransitions directives xs
        in
        (Wedge (updated, Some (enabledEvents, passiveEvents)),
         enabledEvents,
         passiveEvents,
         insertAfter, isNew, removeTransitions, callbacks)

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
                 if EventSet.contains RemoveSelf vnode.enabledEvents then
                   let () =
                     removeVNode
                       vnode.element
                       vnode.namespace
                       vnode.directives
                       true
                       (EventSet.contains RemoveChildren vnode.enabledEvents)
                       (fun () -> (
                            match vnode.onRemove with
                              Some f -> f
                            | None -> removeElement
                          ) parent vnode.element; rm ())
                   in 1
                 else (
                   if EventSet.contains RemoveChildren vnode.enabledEvents then
                     go vnode.element vnode.directives
                   else
                     0
                 )
               )
             | _ -> count
          ) 0 directives
      in
      count := go parent directives

    and removeVNodeOwnTransition
        ?removeTransitions element namespace directives callbacks =
      match eventName TransitionEnd with
        Some name ->
        let directives, _, enabledEvents, _, removeTransitions, callbacks' =
          patch
            ~enableRemoveTransitions:true
            ?notifyRemoveTransitions:removeTransitions
            element
            namespace
            notify
            directives
            directives
        in
        let callbacks = callbacks' @ callbacks in
        let target = Webapi.Dom.Element.asEventTarget element in
        let options = [%bs.obj {
          capture = true;
          passive = true;
          once = false;
        }] in
        let r = ref None in
        let handler event =
          let property =
            Webapi.Dom.TransitionEvent.propertyName
              (unsafeEvent event)
          in
          if List.exists (
              function
                Some property' -> property = property'
              | None -> true
            ) removeTransitions
          then
            !r >>? fun f -> removeEventListener target name f options;
            if EventSet.contains RemoveSelf enabledEvents then
              removeVNodeOwnTransition
                ~removeTransitions
                element namespace directives callbacks
            else
              List.iter (fun f -> f ()) callbacks
        in
        addEventListener target name handler options;
        r := Some handler
      | None -> List.iter (fun f -> f ()) callbacks

    and removeVNode
        child
        namespace
        directives
        removeSelf
        removeChildren
        callback =
      let next () =
        if removeSelf then
          removeVNodeOwnTransition child namespace directives [callback]
        else
          callback ()
      in
      if removeChildren then
        removeVNodeNested child directives next
      else
        next ()
    in
    go1 alwaysReorder insertAfter enableRemoveTransitions

let start ?namespace ?onPatch element view update state =
  let listeners = Array.make (Array.length browserEvents) None in
  let register oldEvents newEvents r notify passive =
    if oldEvents != newEvents then
      let open Webapi.Dom in
      let target = Element.asEventTarget element in
      let options = [%bs.obj {
        capture = true;
        passive = passive;
        once = false;
      }] in
      for i = 0 to Array.length browserEvents - 1 do
        let event = Array.get browserEvents i in
        let contains = EventSet.contains event in
        let check =
          if passive then
            (fun _ e -> contains e)
          else
            (fun e _ -> contains e)
        in
        let before = EventSet.contains event oldEvents in
        let after = EventSet.contains event newEvents in
        if before <> after then (
          let name = eventName event in
          match name with
            Some name ->
            if before then (
              match Array.get listeners i with
                Some f ->
                removeEventListener target name f options
              | None -> ()
            ) else
              let f =
                fun ev ->
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
                  let directives = !r in
                  dispatch
                    check
                    ev notify
                    directives
                    (List.rev parents)
              in
              addEventListener target name f options;
              Array.set listeners i (Some f)
          | None -> ()
        )
      done
  in
  let patch =
    let rEnabled = ref EventSet.empty in
    let rPassive = ref EventSet.empty in
    let rDirectives = ref empty in
    fun notify newDirectives ->
      let oldEnabledEvents = !rEnabled in
      let oldPassiveEvents = !rPassive in
      let oldDirectives = !rDirectives in
      let updatedDirectives, _, enabledEvents, passiveEvents, _, callbacks =
        patch element namespace notify oldDirectives newDirectives
      in
      rEnabled := enabledEvents;
      rPassive := passiveEvents;
      rDirectives := updatedDirectives;
      register oldEnabledEvents enabledEvents rDirectives notify false;
      register oldPassiveEvents passiveEvents rDirectives notify true;
      List.iter (fun f -> f ()) callbacks
  in
  let s = ref state in
  let rec go () =
    let rec notify message =
      let prev = !s in
      let next = update !s notify message in
      if prev != next then (
        s := next;
        go () |> ignore
      )
    in
    let state = !s in
    let node = view state in
    patch notify node;
    onPatch >>? (fun f -> f state);
    notify
  in
  go ()

module Event = struct
  let clipboard (f : Dom.clipboardEvent -> 'a) = eventListener f
  let drag (f : Dom.dragEvent -> 'a) = eventListener f
  let event (f: Dom.event -> 'a) = eventListener f
  let uiEvent (f: Dom.uiEvent -> 'a) = eventListener f
  let focus (f: Dom.focusEvent -> 'a) = eventListener f
  let input (f: Dom.inputEvent -> 'a) = eventListener f
  let keyboard (f : Dom.keyboardEvent -> 'a) = eventListener f
  let mouse (f: Dom.mouseEvent -> 'a) = eventListener f
  let touch (f: Dom.touchEvent -> 'a) = eventListener f
  let transition (f : Dom.transitionEvent -> 'a) = eventListener f
  let wheel (f: Dom.wheelEvent -> 'a) = eventListener f
end

module Export = struct
  type 'a directive = 'a t

  let attr ?namespace name value =
    Attribute (namespace, name, value)

  let className name =
    ClassName name

  let component view =
    let view = safeIdentity view in
    fun handler state ->
      Component (view, handler, state, None)

  let cond directive b = if b then directive else Skip

  let empty = [||]

  let h ?namespace ?onInsert ?onRemove selector directives =
    Detached (namespace, selector, onInsert, onRemove, directives)

  let keyed array =
    Index array

  let maybe f = function
      Some x -> f x
    | None -> Skip

  let mount = start

  let removeTransition ?name directives =
    RemoveTransition (name, directives, false)

  let prop name value =
    Property (name, value)

  let style ?important:(important = false) name value =
    Style (name, value, important)

  let text string =
    Text (None, string)

  let thunk fn =
    fun state ->
      Thunk (state, fn, None)

  let wedge directives =
    Wedge (directives, None)

  let onAbort ?passive:(passive=false) f = Event.event f Abort passive
  let onBeforeInput ?passive:(passive=false) f = Event.input f BeforeInput passive
  let onBeforeUnload ?passive:(passive=false) f = Event.event f BeforeUnload passive
  let onBlur ?passive:(passive=false) f = Event.focus f Blur passive
  let onChange ?passive:(passive=false) f = Event.event f Change passive
  let onClick ?passive:(passive=false) f = Event.mouse f Click passive
  let onDblClick ?passive:(passive=false) f = Event.mouse f DblClick passive
  let onDragEnd ?passive:(passive=false) f = Event.drag f DragEnd passive
  let onDragEnter ?passive:(passive=false) f = Event.drag f DragEnter passive
  let onDragExit ?passive:(passive=false) f = Event.drag f DragExit passive
  let onDragLeave ?passive:(passive=false) f = Event.drag f DragLeave passive
  let onDragOver ?passive:(passive=false) f = Event.drag f DragOver passive
  let onDragStart ?passive:(passive=false) f = Event.drag f DragStart passive
  let onDrop ?passive:(passive=false) f = Event.drag f Drop passive
  let onFocus ?passive:(passive=false) f = Event.focus f Focus passive
  let onFocusIn ?passive:(passive=false) f = Event.focus f FocusIn passive
  let onFocusOut ?passive:(passive=false) f = Event.focus f FocusOut passive
  let onInput ?passive:(passive=false) f = Event.input f Input passive
  let onKeyDown ?passive:(passive=false) f = Event.keyboard f KeyDown passive
  let onKeyPress ?passive:(passive=false) f = Event.keyboard f KeyPress passive
  let onKeyUp ?passive:(passive=false) f = Event.keyboard f KeyUp passive
  let onLoad ?passive:(passive=false) f = Event.input f Load passive
  let onMouseDown ?passive:(passive=false) f = Event.mouse f MouseDown passive
  let onMouseEnter ?passive:(passive=false) f = Event.mouse f MouseEnter passive
  let onMouseLeave ?passive:(passive=false) f = Event.mouse f MouseLeave passive
  let onMouseMove ?passive:(passive=false) f = Event.mouse f MouseMove passive
  let onMouseOut ?passive:(passive=false) f = Event.mouse f MouseOut passive
  let onMouseUp ?passive:(passive=false) f = Event.mouse f MouseUp passive
  let onPopState ?passive:(passive=false) f = Event.event f PopState passive
  let onReadyStateChange ?passive:(passive=false) f = Event.event f ReadyStateChange passive
  let onResize ?passive:(passive=false) f = Event.event f Resize passive
  let onScroll ?passive:(passive=false) f = Event.uiEvent f Scroll passive
  let onSelect ?passive:(passive=false) f = Event.event f Select passive
  let onSubmit ?passive:(passive=false) f = Event.event f Submit passive
  let onTouchCancel ?passive:(passive=false) f = Event.touch f TouchCancel passive
  let onTouchEnd ?passive:(passive=false) f = Event.touch f TouchEnd passive
  let onTouchMove ?passive:(passive=false) f = Event.touch f TouchMove passive
  let onTouchStart ?passive:(passive=false) f = Event.touch f TouchStart passive
  let onTransitionCancel ?passive:(passive=false) f = Event.transition f TransitionCancel passive
  let onTransitionEnd ?passive:(passive=false) f = Event.transition f TransitionEnd passive
  let onTransitionRun ?passive:(passive=false) f = Event.transition f TransitionRun passive
  let onTransitionStart ?passive:(passive=false) f = Event.transition f TransitionStart passive
  let onUnhandledRejection ?passive:(passive=false) f = Event.event f UnhandledRejection passive
  let onUnload ?passive:(passive=false) f = Event.event f Unload passive
  let onWheel ?passive:(passive=false) f = Event.wheel f Wheel passive
  let onBeforeCopy ?passive:(passive=false) f = Event.clipboard f BeforeCopy passive
  let onBeforeCut ?passive:(passive=false) f = Event.clipboard f BeforeCut passive
  let onBeforePaste ?passive:(passive=false) f = Event.clipboard f BeforePaste passive
  let onCopy ?passive:(passive=false) f = Event.clipboard f Copy passive
  let onCut ?passive:(passive=false) f = Event.clipboard f Cut passive
  let onPaste ?passive:(passive=false) f = Event.clipboard f Paste passive
  let onClipboardChange ?passive:(passive=false) f = Event.clipboard f ClipboardChange passive
end
