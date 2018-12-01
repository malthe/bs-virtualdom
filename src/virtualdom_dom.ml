open Webapi.Dom

type eventOptions = <capture : bool; once : bool; passive : bool> Js.t

type 'a cb = 'a -> unit

external elementToDict : Element.t -> 'a Js.Dict.t = "%identity"

external addEventListener :
  Dom.eventTarget ->
  string ->
  'a Dom.event_like cb ->
  eventOptions -> unit =
  "addEventListener"
[@@bs.send]

external removeEventListener :
  Dom.eventTarget ->
  string ->
  'a Dom.event_like cb ->
  eventOptions -> unit =
  "removeEventListener"
[@@bs.send]

let addClassName element name = DomTokenList.add name (Element.classList element)
let appendElement parent child = Element.appendChild child parent
let appendNode parent child = Node.appendChild child parent
let createTextNode text = Document.createTextNode text document
let getElementById id = Document.getElementById id document
let getProperty element = elementToDict element |> Js.Dict.get
let getTagName = Element.tagName
let getId = Element.id
let getClassName = Element.className
let insertBefore parent element reference =
  Element.insertBefore element reference parent |> ignore
let nextElementSibling = Element.nextSibling
let nextTextSibling = Text.nextSibling
let removeAttribute element name = Element.removeAttribute name element
let removeAttributeNS element namespace name = Element.removeAttributeNS
    namespace name element
let removeClassName element name = DomTokenList.remove name (Element.classList element)
let removeNode parent child = Node.removeChild child parent |> ignore
let removeElement parent child = Element.removeChild child parent |> ignore
let removeProperty element key = Js.Dict.unsafeDeleteKey (elementToDict element) key [@bs]
let removeStyle element name =
  CssStyleDeclaration.removeProperty name
    (HtmlElement.style (Element.unsafeAsHtmlElement element)) |> ignore
let setAttribute el name value = Element.setAttribute name value el
let setAttributeNS el ns name value = Element.setAttributeNS ns name value el
let setTextContent = Text.setTextContent
let setProperty element = elementToDict element |> Js.Dict.set
let setStyle element name value important =
  CssStyleDeclaration.setProperty name value
    (if important then "important" else "")
    (HtmlElement.style (Element.unsafeAsHtmlElement element))

let createElement namespace selector =
  let open Js.String in
  let hash_idx = indexOf "#" selector in
  let dot_idx = indexOf "." selector in
  let hash = if hash_idx > 0 then hash_idx else length selector in
  let dot = if dot_idx > 0 then dot_idx else length selector in
  let tag = if hash_idx >= 0 || dot_idx >= 0 then
      slice ~from:0 ~to_:(Js.Math.min_int hash dot) selector
    else
      selector
  in
  let element = match namespace with
      Some namespace -> Document.createElementNS namespace tag document
    | None -> Document.createElement tag document
  in
  if hash < dot then
    setAttribute element "id" (
      slice ~from:(hash + 1) ~to_:dot selector
    );
  if dot_idx > 0 then
    setAttribute element "class" (
      replaceByRe [%bs.re "/\\./g"] " " (
        (sliceToEnd ~from:(dot + 1) selector)
      )
    );
  element

