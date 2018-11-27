# Virtual DOM

A virtual DOM library written in OCaml/BuckleScript with a focus on ease-of-use, immutability and performance.

It's got a small footprint. Just over 5KB compressed.

[![License: MIT](https://img.shields.io/badge/License-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)

## Table of contents

* [Introduction](#introduction)
* [Creating elements](#creating-elements)
* [Directives](#directives)
* [Events](#events)
* [Transitions](#transitions)
* [Structuring applications](#structuring-applications)

## Introduction

The library was designed from the ground up to support a functional, reactive application model. The user provides input and you translate that to messages which are then processed in a central update handler to feed a new state to the rendering loop.

You'll have to try hard to shoot yourself in the foot!

The library provides the following building blocks which are also _directives_:

- `h` function to create virtual nodes.
- `thunk` function to cache nodes.
- `wedge` function to insert multiple directives.
- a number of directives that adds attributes, classes, events, properties, and styles to a virtual node.

In addition, `mount` is what you use to attach the main view function to an existing DOM element.

Here's an increment/decrement example:

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
    onClick (fun _ -> message)
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
    Some target -> mount target view update 0
  | None -> ()
```

In this example, the _model_ is simply an integer counter, initially set to `0` (the last parameter in the `mount` call). In a real application, this would be a more complex value. We're assuming that the document has an element with the id `container` in it such as `<div id="container"></div>`.

The `onClick` handler receives a native JavaScript browser event (unused in the example, hence the underscore) and returns a message which is a type variable that's bound by the update function (in the example this is the `message` type).

The `update` function will often have to deal with asynchronous logic. This is illustrated in the example with the `Delay` message. It sets up a timer and when it finishes, uses the `notify` method to hook back into the event loop.

## Creating elements

The `h` function takes a tag name such as `"div#main.app-like"` and an array of directives:
```ocaml
val h : ?key:string -> ?namespace:string -> string -> Virtualdom.t array -> Virtualdom.t
```
The `key` argument is optional. If provided, it should be unique among the immediate children. We'll talk more about this in a bit. The return value is a virtual node, but it's also a directive in its own right. This is how we add child nodes:
```ocaml
let node = h "div" [|
  h "span" [|
    text "Hello, ";
    h "em" [| text "Zaphod Beeblebrox" |]
  |]
|]
```
An overview of the directives is presented in the next section, but it's important to understand how virtual nodes turn into real nodes and how they're updated.

This library uses a reconciliation algorithm similar to [React](https://reactjs.org/docs/reconciliation.html), also known as "patch and diff". Basically, the library matches the old tree with the new tree and makes the required changes. Ideally, the minimum amount of changes required, but the algorithm is rather simple. To match an old node with a new one, it lines up the arrays of directives and makes at most one comparison. What this effectively means is that we need a special mechanism to deal with reorderings.

This is where the optional `key` argument comes in.

In a situation where we're reordering children and/or adding and removing them, we need to equip the patch and diff algorithm with a unique key for each child. The algorithm will still apply the reconciliation algorithm to keyed child nodes, but it will be able to do so without removing and creating the elements from scratch (why slows down our app and causes unnecessary reflowing.)

## Directives

You can mix and match directives; some affect the element itself such as `attr` and `className` while `h` and `text` add child nodes:

| Directive      | Description                 | Example                                      |
| :------------- | :-------------------------- | :------------------------------------------- |
| `attr`         | Sets an attribute           | `attr "href" "#"`                            |
| `className`    | Sets a class name           | `className ("field-" ^ name)`                |
| `cond`         | Conditionally use directive | `cond hidden (className "hidden")`           |
| `h`            | Adds child                  | (See example above.)                         |
| `prop`         | Sets a property             | `prop "value" "42"`                          |
| `style`        | Sets a style                | `style ~important:true "border" "none"`      |
| `text`         | Adds a text node            | `text "Hello"`                               |
| `thunk`        | Caches a directive          | `thunk f 42`                                 |
| `wedge`        | Inserts multiple directives | `wedge children`                             |

Additional documentation can be found in the [module signature](src/virtualdom.mli).

## Events

The library comes with functions to bind to the most commonly used browser events.
```ocaml
val onClick : (Dom.mouseEvent -> 'a) -> 'a t
```
They're named exactly like their browser counterpart except for the camel-casing. In order to actually pull out information from the events, you can use [bs-webapi](https://www.npmjs.com/package/bs-webapi) which is already pulled in as a dependency of this library.
```ocaml
onClick (
  fun event ->
    let open Webapi.Dom in
    Clicked (MouseEvent.target event |. EventTarget.unsafeAsElement)
)
```
The example above assumes that `Clicked of Dom.element` is a message that's understood by the update function.

## Transitions

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
The element will be removed from the document only when the transition ends.

It's possible to nest `removeTransition` directives to create multi-layered transitions.

## Structuring applications

The key to good performance is to do as little work as possible in every iteration of the render loop. Work in the context of this library is both calling into view functions to create the virtual node structures and actually applying those structures on the DOM elements in the document.

### Using predefined nodes

This is the most simple way of optimizing your view functions. Simply move static nodes outside of the rendering loop to avoid dynamic allocation:

```ocaml
let button title message =
  h "button" [|
    text title;
    onClick (fun _ -> message)
  |]

let signupButton = button "Signup" SignupClicked
```

When you're using this predefined signup button in your view code, the library knows that it can safely skip over it when patching.

### Caching nodes with thunks

The strangely named `thunk` directive serves the purpose of avoiding calling into view functions using a caching mechanism. It's inspired by Elm's [Html.Lazy](https://guide.elm-lang.org/optimization/lazy.html) module.:
```ocaml
val thunk : ('a -> 'b Virtualdom.t) -> 'a -> 'b Virtualdom.t
```
Using the "same input, same output" philosophy, the thunk is only evaluated when the input changes. This comparison uses strict equality which basically means it has to be the exact same object, but importantly, the view function itself must be exactly the same as well.

As when using static nodes (see previous section), you must define the thunk's view function outside of the main rendering loop. Here's an example:
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

You can use thunks to cache nodes at multiple levels in your application. In fact, the requirement that the function must be defined statically, can be helpful in organizing your view code and avoid excessive nesting.

### Strict equality

From Elm's documentation (adapted to OCaml):

> Note: When are two values “the same” though? To optimize for performance, we use JavaScript’s === operator behind the scenes:
>
> * Structural equality is used for `int`, `float`, `string` and `bool`.
> * Reference equality is used for records, lists, custom types, dictionaries, etc.
>
> Structural equality means that 4 is the same as 4 no matter how you produced those values. Reference equality means the actual pointer in memory has to be the same. Using reference equality is always cheap O(1), even when the data structure has thousands or millions of entries. So this is mostly about making sure that using lazy will never slow your code down a bunch by accident. All the checks are super cheap!
