open Virtualdom

let valueOfEventTarget target =
  let open Webapi.Dom in
  match
    HtmlInputElement.ofElement (
      EventTarget.unsafeAsElement target
    )
  with
    Some input -> Some (HtmlInputElement.value input)
  | None -> None

module Todo = struct
  type id = int

  type task = {
    id : id;
    description : string;
    completed : bool;
    editing : bool;
  }

  type t = {
    input : string;
    tasks : task list;
    nextId : int;
  }

  type message =
      Add of string
    | RemoveCompleted
    | Remove of id
    | Edit of id * string
    | Input of string
    | Open of id
    | ToggleCompletion of id

  let init = {
    tasks = [];
    input = "";
    nextId = 0;
  }

  let key task = string_of_int task.id

  let create id description = {
    id;
    description;
    editing = false;
    completed = false;
  }

  let count t =
    List.fold_left (
      fun (i, j) t ->
        (if t.completed then i else i + 1), j + 1
    ) (0, 0) t.tasks

  let update t _ =
    let updateTask id f = {
      t with
      tasks =
        List.map
          (fun task ->
             if task.id = id then
               f task
             else
               task
          )
          t.tasks
    }
    in
    function
      Add s ->
      let id = t.nextId in
      let task = create id s in {
        input = "";
        tasks = task::t.tasks;
        nextId = id + 1
      }
    | RemoveCompleted -> {
        t with
        tasks =
          List.filter
            (fun task -> not task.completed)
            t.tasks
      }
    | Remove id -> {
        t with
        tasks =
          List.filter
            (fun task -> task.id <> id)
            t.tasks
      }
    | Edit (id, description) ->
      updateTask id
        (fun task -> {
             task with
             description
           })
    | Input s -> { t with input = s }
    | Open id ->
      updateTask id
        (fun task -> {
             task with editing = true
           })
    | ToggleCompletion id ->
      updateTask id
        (fun task -> {
             task with completed = not task.completed
           })

  let viewTask t =
    h "li" [|
      cond t.completed (
        className "completed"
      );
      cond t.editing (
        className "editing"
      );
      h "div.view" [|
        h "input.toggle" [|
          attr "type" "checkbox";
          cond t.completed (
            attr "checked" "";
          );
          onClick
            (fun _ -> Some (ToggleCompletion t.id))
        |];
        h "label" [|
          text t.description
        |];
        h "button.destroy" [|
          onClick (fun _ -> Some (Remove t.id))
        |]
      |]
    |]

  let view t = [|
    h "header.header" [|
      h "h1" [| text "todos" |];
      h "input.new-todo" [|
        attr "placeholder" "What needs to be done?";
        attr "autofocus" "";
        prop "value" t.input;
        onBlur (
          fun ev ->
            let open Webapi.Dom in
            Some (
              Input (
                match FocusEvent.target ev |. valueOfEventTarget with
                  Some value -> value
                | None -> ""
              )
            )
        );
        onKeyDown (
          fun ev ->
            let open Webapi.Dom in
            if KeyboardEvent.key ev = "Enter" then
              match KeyboardEvent.target ev |. valueOfEventTarget with
                Some value ->
                let s = value |. Js.String.trim in
                if s <> "" then
                  Some (Add s)
                else
                  None
              | None -> None
            else
              None
        )
      |];
    |];
    h "section.main" [|
      h "input#toggle-all.toggle-all" [|
        attr "type" "checkbox";
      |];
      h "label" [|
        attr "for" "toggle-all";
        text "Mark all as complete"
      |];
      h "ul.todo-list" [|
        keyed (
          Array.map
            (fun entry -> (
                 key entry,
                 thunk viewTask entry
               ))
            (t.tasks |> List.rev |> Array.of_list)
        )
      |]
    |];
    let remaining, total = count t in
    h "footer.footer" [|
      h "span.todo-count" [|
        h "strong" [|
          text (string_of_int remaining);
          text (" " ^ (if remaining = 1 then "item" else "items") ^ " left")
        |]
      |];
      cond (total - remaining > 0) (
        h "button.clear-completed" [|
          text "Clear completed";
          onClick (fun _ -> Some RemoveCompleted)
        |]
      )
    |]
  |]
end

let () =
  let open Webapi.Dom in
  match Document.getElementById "container" document with
    Some target -> mount target Todo.view Todo.update Todo.init
  | None -> ()
