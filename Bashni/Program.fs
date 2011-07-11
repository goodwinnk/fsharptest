let min_item_value = 0
let max_item_value = 6
let stick_length = 7
let MUST_DO_STEPS_PRIORITY = 100

type Item (color: int, number: int) =
    member this.Color with get () = color
    member this.Number with get () = number
    override this.ToString() =
        sprintf "i=%d:%d" color number

type Stick (items: Item list) =
    member this.Items with get () = items
    member this.TopItem = items.Head
    member this.IsEmpty = items.IsEmpty
    override this.ToString() =
        sprintf "s[%s]" (String.concat ", " (List.toSeq (List.map (fun item -> item.ToString()) items)))

type Field (sticks: Stick array) =
    member this.Sticks with get () = sticks
    override this.ToString() =
        sprintf "Field:\n%s" (String.concat "\n" (Array.toSeq (Array.map (fun stick -> stick.ToString()) sticks)))

type Move (fromIndex : int, toIndex: int) =
    member this.FromIndex with get () = fromIndex
    member this.ToIndex with get () = toIndex
    override this.ToString() =
        
        sprintf "(f: %d t: %d)" fromIndex toIndex
        

let i color number = new Item (color, number)

let move_items_sublist (field: Field) (index: int) : Item list =
    if (field.Sticks.[index].IsEmpty) then
        []
    else
        let rec items_aggregate (items: Item list) (result: Item list) = 
            match items with
            | [] -> List.rev result
            | item :: tail ->
                if (item.Color = result.Head.Color && item.Number = result.Head.Number + 1) then
                    items_aggregate tail (item :: result)
                else
                    List.rev result
           
        items_aggregate field.Sticks.[index].Items.Tail [field.Sticks.[index].TopItem]

let is_finished (field: Field) : bool = 
    let is_stick_finished index : bool =
        let stick = field.Sticks.[index]
        stick.IsEmpty || (stick.Items.Length = stick_length && (move_items_sublist field index).Length = stick_length)

    List.forall (fun stick -> is_stick_finished stick) [0 .. field.Sticks.Length - 1]

let is_valid_move (field: Field) (move: Move) =
    let toStick = field.Sticks.[move.ToIndex]
    let fromStick = field.Sticks.[move.FromIndex]
    move.ToIndex <> move.FromIndex && (toStick.IsEmpty ||
        (toStick.TopItem.Color = fromStick.TopItem.Color && toStick.TopItem.Number > fromStick.TopItem.Number))

let move_priority (field: Field) (move: Move) =
    let to_stick = field.Sticks.[move.ToIndex]    
    let from_stick = field.Sticks.[move.FromIndex]

    let moved_items = List.rev (move_items_sublist field move.FromIndex)
    let move_bottom_item = moved_items.Head

    if ((not to_stick.IsEmpty) && (not from_stick.IsEmpty) && to_stick.TopItem.Number = move_bottom_item.Number + 1) || 
        (to_stick.IsEmpty && move_bottom_item.Number = max_item_value && from_stick.Items.Length > moved_items.Length) then
        MUST_DO_STEPS_PRIORITY
    else
        0

let find_stick_moves (field: Field) (index: int) : Move list =
    if (field.Sticks.[index].IsEmpty) then
        []
    else
        Array.toList (
            Array.filter (fun move -> is_valid_move field move)
                (Array.mapi (fun stick_index stick -> new Move (index, stick_index)) field.Sticks))
                
let find_moves (field : Field) : Move list =
    List.fold 
        (fun listResult movesList -> List.append listResult movesList) 
        []     
        (Array.toList (Array.mapi (fun index stick -> find_stick_moves field index) field.Sticks))

let make_move (field : Field) (move : Move) : Field = 
    let new_field = new Field(Array.copy field.Sticks)
    let moved_items = move_items_sublist new_field move.FromIndex

    let rec skip_n list n = 
        match n with
        | 0 -> list
        | k ->
            match list with
            | [] -> []
            | head :: tail -> skip_n tail (n - 1)

    new_field.Sticks.[move.FromIndex] <- new Stick((skip_n new_field.Sticks.[move.FromIndex].Items moved_items.Length))
    new_field.Sticks.[move.ToIndex] <- new Stick((List.append moved_items new_field.Sticks.[move.ToIndex].Items))
    new_field  

let rec make_must_do_moves field history=
    let must_do_moves = List.filter (fun move -> (move_priority field move) >= MUST_DO_STEPS_PRIORITY) (find_moves field)
    if (must_do_moves.IsEmpty) then
        (field, history) 
    else
        make_must_do_moves (make_move field must_do_moves.Head) (must_do_moves.Head::history)
    
let make_move_in_branches (branches : (Field * Move list) list) =
    printfn "make_move_in_branches: %d" branches.Length
    List.collect 
        (fun history_pair ->
            match (history_pair) with
            | (field, history) -> 
                match (make_must_do_moves field []) with
                | (significant_field, local_history) -> 
                    let moves = find_moves significant_field
                    if moves.IsEmpty then
                        []
                    else
                        let full_history = List.append local_history history
                        List.map (fun move -> (make_move significant_field move, move::full_history)) moves
        )
        branches

let rec solve_iterate (branches : (Field * Move list) list) =
    if (branches.IsEmpty) then
        printfn "No branches!"
    else
        let _, history = branches.Head
        printfn "Iteration: %d" history.Length
        
    let is_finished_opt = List.tryFind (fun (field, history) -> is_finished field) branches
    match is_finished_opt with
    | Some ((field, history)) -> 
        printfn "Solved!!! %d" history.Length
        printfn "History:\n %A" (List.rev history)
    | None ->
        printfn "Next iteration!"
        solve_iterate (make_move_in_branches branches)
        ()

let solve (field : Field) = 
    solve_iterate [(field, [])]

// Brown 1
// Green 2
// Lilac 3
// Turquoies 4
// Red 5
// Blue 6

// 115587
let stick0 = new Stick ([ i 1 5; i 2 0; i 2 1; i 3 1; i 3 2; i 2 2])
let stick1 = new Stick ([ i 5 5; i 4 2; i 4 3; i 4 4; i 3 3; i 5 4])
let stick2 = new Stick ([ i 5 3; i 1 6; i 4 6; i 5 0; i 2 6])
let stick3 = new Stick ([ i 6 1; i 6 2; i 5 1; i 1 2; i 1 3])
let stick4 = new Stick ([ i 2 3; i 2 4; i 3 6; i 3 4; i 3 5])
let stick5 = new Stick ([ i 6 6; i 3 0; i 4 0; i 4 1; i 6 5])
let stick6 = new Stick ([ i 1 4; i 4 5; i 1 0; i 5 6; i 5 2])
let stick7 = new Stick ([ i 6 3; i 6 4; i 6 0; i 1 1; i 2 5])


// Brown 1
// Green 2
// Lilac 3
// Turquoies 4
// Red 5
// Blue 6
(*
// 216597
let stick0 = new Stick ([ i 5 3; i 6 5; i 3 0; i 1 0; i 1 1; i 1 2])
let stick1 = new Stick ([ i 4 6; i 4 5; i 6 0; i 2 0; i 3 2; i 3 3])
let stick2 = new Stick        ([ i 3 4; i 3 1; i 4 0; i 4 1; i 4 4])
let stick3 = new Stick        ([ i 6 1; i 1 6; i 5 0; i 5 1; i 5 2])
let stick4 = new Stick        ([ i 5 6; i 5 5; i 2 2; i 2 4; i 2 3])
let stick5 = new Stick        ([ i 3 5; i 2 6; i 4 2; i 4 3; i 5 4])
let stick6 = new Stick        ([ i 2 1; i 6 4; i 6 6; i 2 5; i 1 3])
let stick7 = new Stick        ([ i 3 6; i 1 4; i 1 5; i 6 2; i 6 3])
*)

let field = new Field ([| stick0; stick1; stick2; stick3; stick4; stick5; stick6; stick7 |])

solve field
