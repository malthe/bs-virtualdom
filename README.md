# Virtual DOM

A virtual DOM library written in OCaml/BuckleScript with a focus on ease-of-use, immutability and performance.

It's got a small footprint. Just over 7KB compressed.

[![License: MIT](https://img.shields.io/badge/License-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)

## Table of contents

* [Introduction](#introduction)
* [Creating elements](#creating-elements)
* [Directives](#directives)
* [Events](#events)
* [Transitions](#transitions)
* [Structuring applications](#structuring-applications)
* [FAQ](#faq)

## Introduction

The library was designed from the ground up to support a functional programming model based on immutability. The user provides input and you translate that to messages which are then processed in a central update handler to feed a new state to the rendering loop.

You'll have to try hard to shoot yourself in the foot!

The library provides the following building blocks which are also _directives_:

- `h` function to create virtual nodes (or _vnodes_ for short).
- `component` function to create independent, reusable components.
- `thunk` function to cache directives.
- `wedge` function to insert multiple directives.
- `keyed` function to insert multiple _keyed_ directives.
- a number of directives that adds attributes, classes, events, properties, and styles to a virtual node.

In addition, `mount` is what you use to attach the main view function to an existing DOM element.

Below is an increment/decrement app. We've also implemented the [To-Do App](https://malthe.github.io/bs-virtualdom/examples/todo/) – see [source code](examples/todo).
```ocaml
open Virtualdom

type message =
  Delay of message * int
| Decrement
| Increase

let update state notify = function
  Delay (message, msecs) ->
  Js.Global.setTimeout (fun () -> notify message) msecs |. ignore;
  state
| Decrement -> state - 1
| Increment -> state + 1

let button title message =
  h "button" [|
    text title;
    onClick (fun _ -> Some message)
  |]

let view state = [|
  h "p" [|
    text "Number: ";
    text (string_of_int state)
  |];
  button "Increase" Increment;
  button "Decrease" Decrement;
  button "Increase (after 1 second)" (Delay (Increase, 1000))
|]

let () =
  let open Webapi.Dom in
  match Document.getElementById "container" document with
    Some target ->
    let _ = mount target view update 0 in ()
  | None -> ()
```

In this example, the _model_ is simply an integer counter, initially set to `0` (the last parameter in the `mount` call). In a real application, this would be a more complex value. We're assuming that the document has an element with the id `container` in it such as `<div id="container"></div>`.

The `onClick` handler receives a native JavaScript browser event (unused in the example, hence the underscore) and returns a message which is a type variable that's bound by the update function (in the example this is the `message` type).

The `update` function will often have to deal with asynchronous logic. This is illustrated in the example with the `Delay` message. It sets up a timer and when it finishes, uses the `notify` method to hook back into the event loop.

## Creating elements

The `h` function creates a vnode from a selector such as `"div#main.app-like"` and an array of _child directives_.
```ocaml
val h :
  ?namespace:string ->
  ?onInsert:(Dom.element -> 'a option) ->
  string ->
  'a directive array ->
  'a directive
```
The type variable `'a` is the message type (see the example in the introduction). The return value is a virtual node, but it's also a directive in its own right. This is how we add child nodes:
```ocaml
let greeting =
  h "div" [|
    h "span" [|
      text "Hello, ";
      h "em" [| text "Zaphod Beeblebrox" |]
    |]
  |]
```
An overview of the directives is presented in the next section, but it's important to understand how virtual nodes become real DOM elements and how they're kept in sync.

This library uses a reconciliation algorithm similar to [React](https://reactjs.org/docs/reconciliation.html), also known as "patch and diff". Basically, the library matches the old, _attached_ tree with the new, _detached_ tree and makes the required changes. Ideally, the minimum amount of changes required, but the algorithm is rather simple. To match an old node with a new one, it lines up the arrays of directives and makes at most one comparison. What this effectively means is that we need a special mechanism to deal with reorderings.

### Using keys

In a situation where we're reordering children and/or adding and removing them, we need to equip the patch and diff algorithm with a unique key for each child. The algorithm will still apply the reconciliation algorithm to keyed child nodes, but it will be able to do so without removing and creating the elements from scratch (why slows down our app and causes unnecessary reflowing.)

This mechanism is activated through the use of the `keyed` function.
```ocaml
val keyed : (Js.Dict.key * 'a directive) array -> 'a directive
```
It's like a wedge, but lets you specify a string for each directive (typically a vnode). This string is then used as the key in a lookup table in order to (possibly) locate the old directive and match it with the new one.

### Hooks

- `onInsert`

  The `onInsert` hook is called immediately after the patch cycle when an element is created and inserted into the DOM. The return value is an optional message. This is useful for example to start up an asynchronous initialization routine such as loading external data or starting an animation effect.

## Directives

You can mix and match directives; some affect the element itself such as `attr` and `className` while `h` and `text` add child nodes:

| Directive          | Description                       | Example                                      |
| :-------------     | :--------------------------       | :------------------------------------------- |
| `attr`             | Sets an attribute                 | `attr "href" "#"`                            |
| `className`        | Sets a class name                 | `className ("field-" ^ name)`                |
| `component`        | Inserts a component               | `component view handler state`               |
| `cond`             | Conditionally use directive       | `cond hidden (className "hidden")`           |
| `h`                | Adds child                        | (See example above.)                         |
| `keyed`            | Inserts multiple, keyed directive | `keyed children`                             |
| `prop`             | Sets a property                   | `prop "value" "42"`                          |
| `removeTransition` | Adds a remove transition stage    | `removeTransition directives`                |
| `style`            | Sets a style                      | `style ~important:true "border" "none"`      |
| `text`             | Adds a text node                  | `text "Hello"`                               |
| `thunk`            | Caches a directive                | `thunk f 42`                                 |
| `wedge`            | Inserts multiple directives       | `wedge children`                             |

Additional documentation can be found in the [module signature](src/virtualdom.mli).

## Events

The library comes with functions to bind to the most commonly used browser events.
```ocaml
val onClick :
  ?passive:bool ->
  (Dom.mouseEvent -> 'a option) -> 'a t
```
They're named exactly like their browser counterpart except for the camel-casing. In order to actually pull out information from the events, you can use [bs-webapi](https://www.npmjs.com/package/bs-webapi) which is already pulled in as a dependency of this library.

The return value of the event handler is `'a option` since not all browser events need to become user interface events. For example, if you're listening to the `keypress` event, then you're probably only interested in a subset of key codes.
```ocaml
onClick (
  fun event ->
    let open Webapi.Dom in
    Some (
      Clicked (
        MouseEvent.target event |. EventTarget.unsafeAsElement)
      )
    )
)
```
The example above assumes that `Clicked of Dom.element` is a message that's understood by the update function.

Note that the event system uses the bubbling nature of browser events and attaches event listeners only to the mounted root element (lazily, when required). It uses an internal dispatching system to invoke the matching event handlers defined in the virtual tree.

## Transitions

The library comes with support for staged remove transitions. Normally, the patch and diff algorithm simply removes elements that are no longer in the tree, but to improve the user experience, we often want to stage a transition first (or possibly multiple.)

The `removeTransition` directive is bound to the internal event of an element being removed from the tree. The directive is similar to `wedge` except it's only activated when the element is removed:

```ocaml
let node = h "div" [|
  text "Hello world";
  removeTransition [|
    style "opacity" "0";
    style "transition" "opacity 1.5s ease-out"
  |]
|]
```
The element will be removed from the document only when the transition ends. In the case of multiple style properties, it's possibly to be explicit and provide a `~name` argument, e.g. `~name:"opacity"`. The remove handler will then listen to specifically the end of a transition for the `opacity` style property.

It's possible to nest `removeTransition` directives to stage multi-layered transitions.

## Structuring applications

In a Virtual DOM application you'll usually have just one mounted tree. Thus, it's important to get the application structure right and use composition techniques to split up the codebase into logical modules.

There is some controversy on what's the right way to program this sort of application. The basic premise of the system is that an event always has to trickle up the tree in order to propagate changes in the other direction. But locally, where the event is fired, we often don't want the code to know too much about what's further up the tree. In other words, it's turtles all the way down, but each turtle shouldn't have to deal with the turtles before it.

Using _components_, we can hook into the event stream and use local state to transform a more specialized event into a more generic event. And conversely, a component also lets us specialize the data model in the other direction.

In addition, we get caching for free because of immutability.

### Components

Components are added using the `component` directive.
```ocaml
val component : ('b -> 'c directive) -> ('c -> 'a) -> 'b -> 'a directive
```
That is, a component is a directive of type `'a` which takes a _view_ function returning a directive of type `'c`, a _handler_ function that does one-way translation from `'c` into `'a`, and finally, a _state_ parameter.

Using the "same input, same output" philosophy, the component is only evaluated when the input changes (strict equality). Note that the _view_ function must remain constant.

From Elm's documentation (adapted to OCaml):

> Note: When are two values “the same” though? To optimize for performance, we use JavaScript’s === operator behind the scenes:
>
> * Structural equality is used for `int`, `float`, `string` and `bool`.
> * Reference equality is used for records, lists, custom types, dictionaries, etc.
>
> Structural equality means that 4 is the same as 4 no matter how you produced those values. Reference equality means the actual pointer in memory has to be the same. Using reference equality is always cheap O(1), even when the data structure has thousands or millions of entries. So this is mostly about making sure that using lazy will never slow your code down a bunch by accident. All the checks are super cheap!

### Thunks

The strangely named `thunk` directive is a simple caching mechanism.
```ocaml
val thunk : ('b -> 'a directive) -> 'b -> 'a directive
```
If the cache key (denoted by the type variable `'b`) changes (strict equality), the view function is called. The `thunk` directive is similar to Elm's [Html.Lazy](https://guide.elm-lang.org/optimization/lazy.html) module.

The main difference between thunks and components is that components are able to lift the message type. Like components, the view function must remain constant.

For example:
```ocaml
let renderItem item =
  h "div" [|
    className ("item-" ^ item.name);
    wedge (renderSubItems item.children)
  |]

let view state =
  h "div" (
    Array.map (thunk renderItem) state.items
  )
```
The `renderItem` function is defined outside the rendering loop.

### Static nodes

Using static nodes is the most simple way of optimizing your view functions. Simply predefine nodes outside of the rendering loop to avoid dynamic allocation altogether.
```ocaml
let button title message =
  h "button" [|
    text title;
    onClick (fun _ -> Some message)
  |]

let signupButton = button "Signup" SignupClicked
```
When you're using this statically allocated signup button in your view code, the library knows that it can safely skip over it when patching.

## FAQ

1. _Why isn't `onClick` getting called?_ Perhaps you've already detached the corresponding vnode from the tree. This would prevent the dispatcher from locating the event handler. Note that a remove transition does not prevent an element from being detached from the parent tree.

   Possible related answer:

   - [Listen to `mousedown` instead of `click`](https://stackoverflow.com/questions/10652852/jquery-fire-click-before-blur-event)
