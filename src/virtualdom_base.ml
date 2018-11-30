open Virtualdom_dom
open Virtualdom_misc
open Virtualdom_events
open Virtualdom_types

type 'a update = 'a -> unit

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

let svgNS = "http://www.w3.org/2000/svg"

let isSVG selector =
  let g = Js.String.get selector in
  let length = String.length selector in
  length >= 3 && g 0 == "s" && g 1 == "v" && g 2 == "g" && (
    length == 3 || g 3 == "." || g 3 == "#"
  )

let rec getNextSibling = function
    Attached { element } -> nextElementSibling element
  | Wedge (directives, _) ->
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

let sameVNode namespace selector vnode =
  namespace = vnode.namespace &&
  selector = vnode.selector

let arrayOf = function
    Wedge (d, _) -> d
  | x -> [| x |]

let ofArray array events =
  if Array.length array != 1
  then
    Wedge (array, Some events)
  else
    Array.get array 0

let insert parent child reference =
  match reference with
    Some reference -> insertBefore parent child reference
  | None -> appendElement parent child

let rec insertNested parent directives reference =
  Array.fold_left
    (fun reference d ->
       let rec go =
         function
           Attached { element } ->
           insert parent element reference;
           nextElementSibling element
         | Text (Some node, _) ->
           insert parent node reference;
           nextTextSibling node
         | Thunk (_, _, Some (d, _)) -> go d
         | Wedge (directives, _) ->
           insertNested parent directives reference
         | _ -> reference
       in go d
    )
    reference directives

let rec dispatch
  : 'a 'b.
    EventSet.t ->
    Dom.event ->
    'a update ->
    ('a, 'b) t array ->
    Dom.element list ->
    unit =
  fun i event update directives children ->
    Array.iter
      (function
          Attached { element; events; directives }
          when EventSet.containsBit events i -> (
            match children with
              x::xs when x == element ->
              dispatch i event update directives xs
            | _ -> ()
          )
        | Component (_, handler, _, Some (d, events))
          when EventSet.containsBit events i ->
          let update message = handler message |. update in
          dispatch i event update [| d |] children
        | EventListener (k, f) when i = k ->
          f (unsafe_identity event) >>?
          update
        | Thunk (_, _, Some (d, events))
          when EventSet.containsBit events i ->
          dispatch i event update [| d |] children
        | Wedge (directives, Some events)
          when EventSet.containsBit events i ->
          dispatch i event update directives children
        | _ -> ()
      ) directives

let rec patch
  : 'a 'b.
    ?alwaysReorder:bool ->
    ?enableRemoveTransitions:bool ->
    ?notifyRemoveTransitions:string option list ->
    ?insertAt:Dom.node ->
    Dom.element ->
    string option ->
    ('a, 'b) t array option ->
    ('a, 'b) t array ->
    ('a, 'b) t array * Dom.node option * int * string option list =
  fun
    ?alwaysReorder:(alwaysReorder=false)
    ?enableRemoveTransitions:(enableRemoveTransitions=false)
    ?notifyRemoveTransitions:(notifyRemoveTransitions=[])
    ?insertAt
    element defaultNamespace ->
    let rec cleanup = function
        Attribute (Some ns, name, _) -> removeAttributeNS element ns name
      | Attribute (None, name, _) -> removeAttribute element name
      | ClassName name -> removeClassName element name
      | Component (_, _, _, Some (d, _)) ->
        patch
          ~enableRemoveTransitions:true
          element
          defaultNamespace
          (Some (arrayOf d))
          empty
        |> ignore
      | Property (name, _) -> removeProperty element name
      | Style (name, _, _) -> removeStyle element name
      | Text (Some node, _) -> removeElement element node
      | Thunk (_, _, Some (d, _)) -> cleanup d
      | Attached vnode ->
        removeVNode
          element
          vnode.element
          vnode.namespace
          vnode.directives
          vnode.events
          (fun () -> ())
      | Wedge (directives, _) ->
        let length = Array.length directives in
        for i = 0 to length - 1 do
          cleanup (Array.get directives i)
        done
      | _ -> ()

    and go1
        alwaysReorder
        insertionPoint
        enableRemoveTransitions
        events
        oldDirectives
        newDirectives =
      let updatedDirectives = Array.make (Array.length newDirectives) Skip in
      let directives, i, insertionPoint, events, removeTransitions =
        fold_lefti
          (fun (directives, i, insertionPoint, events, removeTransitions)
            j current ->
            let set = Array.set updatedDirectives j in
            let existing =
              match directives with
                Some directives -> Array.get directives i
              | None -> Skip
            in
            let go events = function
                (d, newEvents, insertionPoint, isNew, newRemoveTransitions) ->
                let events = events lor newEvents in
                if isNew then
                  cleanup existing;
                set d;
                let removeTransitions =
                  removeTransitions @ newRemoveTransitions in
                (directives, i + 1, insertionPoint, events, removeTransitions)
            in
            go events @@
            update alwaysReorder enableRemoveTransitions existing
              insertionPoint events current
          )
          (oldDirectives, 0, insertionPoint, events, [])
          newDirectives
      in
      directives >>? (
        fun directives ->
          for j = i to Array.length directives - 1 do
            Array.get directives j |. cleanup
          done
      );
      updatedDirectives, insertionPoint, events, removeTransitions

    and go2
        insertionPoint
        enableRemoveTransitions
        events
        oldIndex
        newIndex =
      let keys = Js.Dict.fromArray oldIndex in
      let reverse =
        Array.mapi (fun i key -> (key, i)) (Js.Dict.keys keys)
        |> Js.Dict.fromArray
      in
      let oldIndex = Array.copy oldIndex in
      let updatedIndex = Array.copy newIndex in

      let insertionPoint, events, removeTransitions =
        fold_lefti
          (fun (insertionPoint, events, removeTransitions) j (key, current) ->
             let set value = Array.set updatedIndex j (key, value) in
             let existing =
               match Js.Dict.get keys key with
                 Some value ->
                 Js.Dict.get reverse key >>?
                 (fun i -> Array.set oldIndex i (key, Skip));
                 value
               | None -> Skip
             in
             let go events = function
                 (d, newEvents, insertionPoint, isNew, newRemoveTransitions) ->
                 let events = events lor newEvents in
                 if isNew then
                   cleanup existing;
                 set d;
                 let removeTransitions =
                   removeTransitions @ newRemoveTransitions
                 in
                 (insertionPoint, events, removeTransitions)
             in
             update
               true
               enableRemoveTransitions
               existing
               insertionPoint
               events
               current
             |> go events
          ) (insertionPoint, events, []) newIndex
      in
      Array.iter cleanup (Array.map snd oldIndex);
      updatedIndex, insertionPoint, events, removeTransitions

    and update alwaysReorder enableRemoveTransitions next insertionPoint events =
      function
        Attribute (ns, name, value) as d -> (
          match next with
            Attribute (ns', name', value')
            when ns = ns' &&
                 name = name' &&
                 value = value' -> (d, 0, insertionPoint, false, [])
          | _ -> (
              match ns with
                Some ns -> setAttributeNS element ns name value
              | None -> setAttribute element name value
            );
            (d, 0, insertionPoint, true, [])
        )
      | ClassName name as d -> (
          match next with
            ClassName name' when name = name' ->
            (d, 0, insertionPoint, false, [])
          | _ -> addClassName element name;
            (d, 0, insertionPoint, true, [])
        )
      | Component (view, handler, state, _) -> (
          match next with
            Component (view', handler', state', Some (d, _)) as component ->
            if state == state' then
              let insertionPoint =
                if alwaysReorder then
                  insertNested element [| d |] insertionPoint
                else
                  getNextSibling d
              in
              (component, 0, insertionPoint, false, [])
            else
              let result, insertionPoint, events, removeTransitions =
                patch
                  ~enableRemoveTransitions
                  ~alwaysReorder
                  ?insertAt:insertionPoint
                  element
                  defaultNamespace
                  (Some (arrayOf d))
                  (view' state |. arrayOf)
              in
              let d = ofArray result events in
              let directive = Component (
                  view', handler', state, Some (d, events)
                ) in
              (directive, events, insertionPoint, false, removeTransitions)
          | _ ->
            let result, insertionPoint, events, removeTransitions =
              patch
                ~enableRemoveTransitions
                ~alwaysReorder
                ?insertAt:insertionPoint
                element
                defaultNamespace
                None
                (view state |. arrayOf)
            in
            let d = ofArray result events in
            let directive = Component (
                view, handler, state, Some (d, events)
              ) in
            (directive, events, insertionPoint, true, removeTransitions)
        )
      | EventListener (n, _) as d ->
        (d, 1 lsl n, insertionPoint, (
            match next with
              EventListener (n', _)
              when n = n' -> false
            | _ -> true
          ),
         []
        )
      | Index d -> (
          match next with
            Index d' ->
            let updated, insertionPoint, events, removeTransitions =
              go2 insertionPoint enableRemoveTransitions events d' d
            in
            (Index updated, events, insertionPoint, false, removeTransitions)
          | _ ->
            let directives = Array.map snd d in
            let updated, insertionPoint, events, removeTransitions =
              go1 false insertionPoint
                enableRemoveTransitions events None directives in
            let d =
              Array.mapi
                (fun i directive -> (Array.get d i |. fst, directive))
                updated
            in
            (Index d, events, insertionPoint, true, removeTransitions)
        )
      | Property (name, value) as d -> (
          match next with
            Property (name', value')
            when name = name' && value = value' ->
            (d, 0, insertionPoint, false, [])
          | _ ->
            setProperty element name value;
            (d, 0, insertionPoint, true, [])
        )
      | RemoveTransition (name, directives, _) as d -> (
          match next with
            RemoveTransition (name', directives', active) ->
            if enableRemoveTransitions && not active ||
               name = name' &&
               List.mem name notifyRemoveTransitions
            then (
              let updated, insertionPoint, events, removeTransitions =
                go1 alwaysReorder
                  insertionPoint true events (
                  if active && name = name' then
                    (Some directives')
                  else
                    None
                ) directives in (
                RemoveTransition (name, updated, true),
                events,
                insertionPoint,
                false,
                (if not active then
                   name::removeTransitions
                 else
                   removeTransitions)
              )
            ) else
              (d, 1 lsl eventToJs RemoveSelf, insertionPoint, false, [])
          | _ ->
            if enableRemoveTransitions then (
              let updated, insertionPoint, events, removeTransitions =
                go1 alwaysReorder
                  insertionPoint false events
                  None directives in (
                RemoveTransition (name, updated, true),
                events,
                insertionPoint,
                true,
                name::removeTransitions
              )
            ) else (d, 1 lsl eventToJs RemoveSelf, insertionPoint, true, [])
        )
      | Skip as d -> (
          d, 0, insertionPoint, (
            match next with
              Skip -> false
            | _ -> true
          ),
          []
        )
      | Style (name, value, important) as d -> (
          match next with
            Style (name', value', important')
            when name = name' && value = value' &&
                 important = important' ->
            (d, 0, insertionPoint, false, [])
          | _ ->
            setStyle element name value important;
            (d, 0, insertionPoint, true, [])
        )
      | Text (_, string) -> (
          match next with
            Text (Some oldTextNode, string') ->
            if string <> string' then
              setTextContent oldTextNode string;
            if alwaysReorder then
              insert element oldTextNode insertionPoint;
            (Text (Some oldTextNode, string), 0, insertionPoint, false, [])
          | _ ->
            let child = createTextNode string in
            insert element child insertionPoint;
            (Text (Some child, string), 0, insertionPoint, true, [])
        )
      | Thunk (state, fn, _) -> (
          match next with
            Thunk (state', fn', Some (d, _)) as thunk
            when
              fn == unsafe_identity fn' &&
              state == unsafe_identity state' ->
            let insertionPoint =
              if alwaysReorder then
                insertNested element [| d |] insertionPoint
              else
                getNextSibling d
            in
            (thunk, 0, insertionPoint, false, [])
          | t ->
            let d = fn state in
            let isNew, directives =
              match t with
                Thunk (_, _, Some (d, _)) -> false, Some (arrayOf d)
              | _ -> true, None
            in
            let result, insertionPoint, events, removeTransitions =
              go1
                alwaysReorder
                insertionPoint
                enableRemoveTransitions events directives (arrayOf d)
            in
            let updated = ofArray result events in
            (Thunk (state, fn, Some (updated, events)), events,
             insertionPoint, isNew, removeTransitions)
        )
      | Attached { detached = Some d } ->
        update alwaysReorder enableRemoveTransitions next insertionPoint events d
      | Attached _ -> (Skip, 0, insertionPoint, true, [])
      | Detached (namespace, selector, newDirectives) as detached -> (
          match next with
            Attached ({
                element = child;
                directives;
                detached = Some detached';
              } as attached) as t
            when
              detached == detached' ||
              sameVNode namespace selector attached ->
            if alwaysReorder then
              insert element child insertionPoint;
            if detached == detached' then
              let sibling = nextElementSibling child in
              (t, EventSet.childEventToParent attached.events,
               sibling, false, [])
            else
              let directives, sibling, events, removeTransitions =
                patch child namespace (Some directives) newDirectives
              in
              (Attached {
                  attached with
                  element = child;
                  directives;
                  events;
                }, EventSet.childEventToParent events, sibling,
               false, removeTransitions)
          | _ ->
            let vnode =
              create element insertionPoint
                (namespace =| defaultNamespace)
                selector newDirectives detached in
            let sibling = nextElementSibling vnode.element in
            (Attached vnode,
             EventSet.childEventToParent vnode.events, sibling, true, [])
        )
      | Wedge (xs, _) ->
        let isNew, directives = match next with
            Wedge (directives, _) -> false, Some directives
          | _ -> true, None
        in
        let updated, insertionPoint, events, removeTransitions =
          go1 alwaysReorder insertionPoint
            enableRemoveTransitions events directives xs
        in
        (Wedge (updated, Some events), events,
         insertionPoint, isNew, removeTransitions)

    and create parent next namespace selector directives detached =
      let namespace = match namespace with
          Some ns -> Some ns
        | None ->
          if isSVG selector then Some svgNS else None
      in
      let element = createElement namespace selector in
      insert parent element next;
      let (directives, _, events, _) =
        patch element namespace None directives in {
        element;
        namespace;
        selector;
        directives;
        detached = Some detached;
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
                 if EventSet.contains vnode.events RemoveSelf then
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
                   if EventSet.contains vnode.events RemoveChildren then
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
        ?removeTransitions element namespace directives callback =
      match eventName TransitionEnd with
        Some name ->
        let directives, _, events, removeTransitions =
          patch
            ~enableRemoveTransitions:true
            ?notifyRemoveTransitions:removeTransitions
            element
            namespace
            (Some directives)
            directives
        in
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
              (unsafe_identity event)
          in
          if List.exists (
              function
                Some property' -> property = property'
              | None -> true
            ) removeTransitions
          then
            !r >>? fun f -> removeEventListener target name f options;
            if EventSet.contains events RemoveSelf then
              removeVNodeOwnTransition
                ~removeTransitions
                element namespace directives callback
            else
              callback ()
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
        if EventSet.contains events RemoveSelf then
          removeVNodeOwnTransition child namespace directives remove
        else
          remove ()
      in
      if EventSet.contains events RemoveChildren then
        removeVNodeNested child directives next
      else
        next ()

    in
    go1 alwaysReorder insertAt enableRemoveTransitions 0

let start element selector view update state =
  let listeners = Array.make (Array.length browserEvents) None in
  let patch vnode update directives =
    let events = vnode.events in
    let vnode =
      let oldDirectives = match vnode.directives with
          [||] -> None
        | array -> Some array
      in
      let directives, _, events, _ =
        patch element vnode.namespace oldDirectives directives
      in {
        vnode with
        element;
        directives;
        events;
      }
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
            let name = eventName event in
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
    selector;
    element;
    directives = empty;
    detached = None;
    namespace = None;
  } in
  let r = ref vnode in
  let s = ref state in
  let rec go () =
    r := patch !r controller @@ view !s
  and controller message =
    s := update !s controller message;
    go ()
  in
  go ()

module Event = struct
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

module Export = struct
  type ('a, 'b) directive = ('a, 'b) t

  let attr ?namespace name value =
    Attribute (namespace, name, value)

  let className name =
    ClassName name

  let component view handler state =
    Component (view, handler, state, None)

  let cond b directive = if b then directive else Skip

  let empty = [||]

  let h ?namespace selector directives =
    Detached (namespace, selector, directives)

  let keyed array =
    Index array

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

  let thunk fn =
    fun state ->
      Thunk (state, fn, None)

  let wedge directives =
    Wedge (directives, None)

  let onAbort f = Event.event Abort f
  let onBeforeInput f = Event.input BeforeInput f
  let onBeforeUnload f = Event.event BeforeUnload f
  let onBlur f = Event.focus Blur f
  let onChange f = Event.event Change f
  let onClick f = Event.mouse Click f
  let onDblClick f = Event.mouse DblClick f
  let onDragEnd f = Event.drag DragEnd f
  let onDragEnter f = Event.drag DragEnter f
  let onDragExit f = Event.drag DragExit f
  let onDragLeave f = Event.drag DragLeave f
  let onDragOver f = Event.drag DragOver f
  let onDragStart f = Event.drag DragStart f
  let onDrop f = Event.drag Drop f
  let onFocus f = Event.focus Focus f
  let onFocusIn f = Event.focus FocusIn f
  let onFocusOut f = Event.focus FocusOut f
  let onInput f = Event.input Input f
  let onKeyDown f = Event.keyboard KeyDown f
  let onKeyPress f = Event.keyboard KeyPress f
  let onKeyUp f = Event.keyboard KeyUp f
  let onLoad f = Event.input Load f
  let onMouseDown f = Event.mouse MouseDown f
  let onMouseEnter f = Event.mouse MouseEnter f
  let onMouseLeave f = Event.mouse MouseLeave f
  let onMouseMove f = Event.mouse MouseMove f
  let onMouseOut f = Event.mouse MouseOut f
  let onMouseUp f = Event.mouse MouseUp f
  let onPopState f = Event.event PopState f
  let onReadyStateChange f = Event.event ReadyStateChange f
  let onResize f = Event.event Resize f
  let onScroll f = Event.uiEvent Scroll f
  let onSelect f = Event.event Select f
  let onSubmit f = Event.event Submit f
  let onTouchCancel f = Event.touch TouchCancel f
  let onTouchEnd f = Event.touch TouchEnd f
  let onTouchMove f = Event.touch TouchMove f
  let onTouchStart f = Event.touch TouchStart f
  let onTransitionCancel f = Event.transition TransitionCancel f
  let onTransitionEnd f = Event.transition TransitionEnd f
  let onTransitionRun f = Event.transition TransitionRun f
  let onTransitionStart f = Event.transition TransitionStart f
  let onUnhandledRejection f = Event.event UnhandledRejection f
  let onUnload f = Event.event Unload f
  let onWheel f = Event.wheel Wheel f
end
