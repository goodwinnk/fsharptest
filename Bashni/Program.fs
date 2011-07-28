let min_item_value = 0
let max_item_value = 6
let stick_length = 7
let MUST_DO_STEPS_PRIORITY = 100

module List =
    ///<summary> 
    /// An extension method. Method will take the list and return a new list with n first elements skipped.
    ///</summary> 
    let rec skipn list n = 
        match n with
        | 0 -> list
        | k ->
            match list with
            | [] -> []
            | head :: tail -> skipn tail (n - 1)
        
// ====================================================================================
///<summary> 
/// Immutable representation for the item on the stick.
///</summary
type Item (color: int, number: int) =
    member this.Color with get () = color
    member this.Number with get () = number
    override this.ToString() =
        sprintf "i=%d:%d" color number

// ====================================================================================
///<summary> 
/// An abstraction for the stick with items. A list of items can be empty. Stick is immutable.
///</summary> 
type Stick = class
    val move_items_sublist : Item list
    val items : Item list
    val stringRepr : string

    ///<summary> 
    /// Construct new stick with predefined items on it
    ///</summary> 
    new (items : Item list) =
        { 
            items = items;
            move_items_sublist = Stick.get_move_items items;
            stringRepr = Stick.get_str_representation items;
        }

    ///<summary> 
    /// Construct new stick from the given one with moving top elements to another one
    ///</summary> 
    new (baseStick: Stick) = 
        {
            items = List.skipn baseStick.Items baseStick.MoveItemsSublist.Length;
            move_items_sublist = Stick.get_move_items (List.skipn baseStick.Items baseStick.MoveItemsSublist.Length)
            stringRepr = Stick.get_str_representation (List.skipn baseStick.Items baseStick.MoveItemsSublist.Length)
        }

    ///<summary> 
    /// Construct new stick from the given base stick with moving elements from another stick.
    ///</summary>
    new (baseStick: Stick, fromStick: Stick) =
        {
            items = List.append fromStick.MoveItemsSublist baseStick.Items;
            move_items_sublist = Stick.get_move_items (List.append fromStick.MoveItemsSublist baseStick.Items);
            stringRepr = Stick.get_str_representation (List.append fromStick.MoveItemsSublist baseStick.Items)
        } 

    static member private get_str_representation (items: Item list) =
        sprintf "s[%s]" (String.concat ", " (List.toSeq (List.map (fun item -> item.ToString()) items)))
        
    ///<summary> 
    /// Get a sublist of items which will be moved if user start his move from this stick
    ///</summary>
    static member private get_move_items (items: Item list) = 
        if (items.IsEmpty) then
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
           
            items_aggregate items.Tail [items.Head]

    member this.Items with get () = this.items
    member this.TopItem = this.items.Head
    member this.IsEmpty = this.items.IsEmpty
    member this.MoveItemsSublist : Item list = this.move_items_sublist

    ///<summary> 
    /// Checks if current stick is already finished.
    ///</summary>
    member this.IsFinished : bool  = this.IsEmpty || (this.items.Length = stick_length && this.MoveItemsSublist.Length = stick_length)

    override this.GetHashCode() = 
        this.stringRepr.GetHashCode()

    override this.ToString() =
        this.stringRepr

end
// ====================================================================================
///<summary> 
/// An single move - contains from and to indecies.
///</summary> 
type Move (fromIndex : int, toIndex: int) =
    member this.FromIndex with get () = fromIndex
    member this.ToIndex with get () = toIndex
    override this.ToString() =        
        sprintf "(f: %d t: %d)" fromIndex toIndex

///<summary> 
/// An abstraction for the field with sticks. Field is immutable.
///</summary> 
type Field (sticks: Stick array) =
    let moves : Move list = []
    
    member this.Item with get(i: int) = sticks.[i]
    member this.NumberOfSticks with get() = sticks.Length
    member this.StickList with get() = Array.toList sticks    
    member this.Moves with get () = moves

    ///<summary> 
    /// Checks if there is nothing to move on the board and so the game is finished.
    ///</summary>
    member this.IsFinished =
        Array.forall (fun (stick : Stick) -> stick.IsFinished) sticks // Why give type explicitly???

    ///<summary> 
    /// Checks if given move is valid.
    ///</summary>
    member this.IsValidMove (move : Move) =
        let toStick = this.[move.ToIndex]
        let fromStick = this.[move.FromIndex]
        move.ToIndex <> move.FromIndex && not fromStick.IsEmpty && (toStick.IsEmpty ||
            (toStick.TopItem.Color = fromStick.TopItem.Color && toStick.TopItem.Number > fromStick.TopItem.Number))

    ///<summary> 
    /// Give the priority of the move. Moves should be tried according their priority.
    ///</summary>
    member this.GetMovePriority (move: Move) =
        let to_stick = this.[move.ToIndex]    
        let from_stick = this.[move.FromIndex]

        let moved_items = List.rev (from_stick.MoveItemsSublist) // TODO: cache?
        let move_bottom_item = moved_items.Head

        if ((not to_stick.IsEmpty) && to_stick.TopItem.Number = move_bottom_item.Number + 1) (* && to_stick.TopItem.Number <= 2 *) then
            MUST_DO_STEPS_PRIORITY
        else
            0

    ///<summary> 
    /// Find a list of all possible moves for current field.
    ///</summary>
    member this.PossibleMoves = 
        let find_stick_moves (index: int) : Move list =
            if (this.[index].IsEmpty) then
                []
            else
                Array.toList (Array.filter (fun move -> this.IsValidMove move)
                    (Array.mapi (fun stick_index stick -> new Move (index, stick_index)) sticks))

        List.fold (fun listResult movesList -> List.append listResult movesList) []     
            (Array.toList (Array.mapi (fun index stick -> find_stick_moves index) sticks))

    member this.GetKey() =
        (String.concat ";" (Array.toSeq (Array.sort (Array.map (fun stick -> stick.GetHashCode().ToString()) sticks)))).GetHashCode()

    ///<summary> 
    /// Apply given move to the field and return new field.
    ///</summary>
    member this.ApplyMove (move : Move) =
        let new_sticks_array = Array.copy sticks
        new_sticks_array.[move.FromIndex] <- new Stick(this.[move.FromIndex])
        new_sticks_array.[move.ToIndex] <- new Stick(this.[move.ToIndex], this.[move.FromIndex])
        new Field(new_sticks_array)  

    override this.ToString() =
        sprintf "Field:\n%s" (String.concat "\n" (Array.toSeq (Array.map (fun stick -> stick.ToString()) sticks)))

// Solve strategy:
   // Check all while found one with one row

// ====================================================================================
let mutable global_set = Set<int> []

let rec make_must_do_moves (field : Field) history=
    let must_do_moves = List.filter (fun move -> (field.GetMovePriority move) >= MUST_DO_STEPS_PRIORITY) field.PossibleMoves
    if (must_do_moves.IsEmpty) then
        (field, history) 
    else
        make_must_do_moves (field.ApplyMove must_do_moves.Head) (must_do_moves.Head::history)
    
let make_move_in_branches (branches : (Field * Move list) list) =
    printfn "make_move_in_branches: %d" branches.Length
    List.collect 
        (fun history_pair ->
            match (history_pair) with
            | (field, history) -> 
                match (make_must_do_moves field []) with
                | (significant_field, local_history) -> 
                    let moves = significant_field.PossibleMoves
                    if global_set.Contains (significant_field.GetKey()) then
                        []
                    else
                        if moves.IsEmpty then
                            []
                        else 
                            global_set <- global_set.Add (significant_field.GetKey())
                            let full_history = List.append local_history history

                            if significant_field.IsFinished then
                                [significant_field, full_history]
                            else
                                List.map (fun move -> (significant_field.ApplyMove move, move::full_history)) moves
        )
        branches

let rec solve_iterate (branches : (Field * Move list) list) =
    if (branches.IsEmpty) then
        printfn "No branches!"
    else
        let _, history = branches.Head
        printfn "Iteration: %d" history.Length
        
    let is_finished_opt = List.tryFind (fun (field : Field, history) -> field.IsFinished) branches
    match is_finished_opt with
    | Some ((field, history)) -> 
        printfn "Solved!!! %d" history.Length
        printfn "History:\n %A" 
            (String.concat "\n" 
                (Array.toSeq 
                    ((Array.mapi (fun i move -> i.ToString() + " " + move.ToString()) (List.toArray (List.rev history))))
                )
            )
    | None ->
        printfn "Next iteration!"
        solve_iterate (make_move_in_branches branches)
        ()

let solve (field : Field) = 
    solve_iterate [(field, [])]

// =======================================================================================================

let i color number = new Item (color, number)
let s numbers colors = new Stick(List.map2 (fun number color -> i color number) numbers colors)

                    // Brown 1
                    // Green 2
                    // Lilac 3
                    // Turquoies 4
                    // Red 5
                    // Blue 6

// 115587 - 8
(*
let stick0 = new Stick ([ i 1 5; i 2 0; i 2 1; i 3 1; i 3 2; i 2 2])
let stick1 = new Stick ([ i 5 5; i 4 2; i 4 3; i 4 4; i 3 3; i 5 4])
let stick2 = new Stick ([ i 5 3; i 1 6; i 4 6; i 5 0; i 2 6])
let stick3 = new Stick ([ i 6 1; i 6 2; i 5 1; i 1 2; i 1 3])
let stick4 = new Stick ([ i 2 3; i 2 4; i 3 6; i 3 4; i 3 5])
let stick5 = new Stick ([ i 6 6; i 3 0; i 4 0; i 4 1; i 6 5])
let stick6 = new Stick ([ i 1 4; i 4 5; i 1 0; i 5 6; i 5 2])
let stick7 = new Stick ([ i 6 3; i 6 4; i 6 0; i 1 1; i 2 5])
*)

                    // Brown 1
                    // Green 2
                    // Lilac 3
                    // Turquoies 4
                    // Red 5
                    // Blue 6

// 317609 - 44
(*
let stick0 = s [4; 2; 3; 5; 4; 5] [4; 6; 2; 4; 3; 3]
let stick1 = s [0; 4; 4; 3; 0; 1] [1; 5; 1; 4; 6; 6]
let stick2 = s [0; 2; 3; 5; 2] [5; 1; 1; 5; 3]
let stick3 = s [6; 3; 3; 3; 4] [2; 3; 5; 6; 6]
let stick4 = s [2; 0; 1; 0; 6] [4; 2; 2; 3; 6]
let stick5 = s [2; 2; 6; 5; 1] [5; 2; 4; 6; 3]
let stick6 = s [6; 0; 1; 6; 1] [5; 4; 4; 3; 1]
let stick7 = s [1; 4; 5; 5; 6] [5; 2; 2; 1; 1]
*)


// Brown 1
// Green 2
// Lilac 3
// Turquoies 4
// Red 5
// Blue 6

// 216597 - 8
// Best 44

(*
let stick0 = new Stick ([ i 5 3; i 6 5; i 3 0; i 1 0; i 1 1; i 1 2])
let stick1 = new Stick ([ i 4 6; i 4 5; i 6 0; i 2 0; i 3 2; i 3 3])
let stick2 = new Stick        ([ i 3 4; i 3 1; i 4 0; i 4 1; i 4 4])
let stick3 = new Stick        ([ i 6 1; i 1 6; i 5 0; i 5 1; i 5 2])
let stick4 = new Stick        ([ i 5 6; i 5 5; i 2 2; i 2 4; i 2 3])
let stick5 = new Stick        ([ i 3 5; i 2 6; i 4 2; i 4 3; i 5 4])
let stick6 = new Stick        ([ i 2 1; i 6 4; i 6 6; i 2 5; i 1 3])
let stick7 = new Stick        ([ i 3 6; i 1 4; i 1 5; i 6 2; i 6 3])
*)


                        // Brown 1
                        // Green 2
                        // Lilac 3
                        // Turquoies 4
                        // Red 5
                        // Blue 6
                        // LightYellow 7
                        // Orange 8
                        // LightBlue 9
                        // LightBrown 10

(*
// 318216 - 12
let stick0 = new Stick ([ i 2 5; i 7 5; i 8 1; i 3 5; i 3 6; i 2 3])
let stick1 = new Stick ([ i 2 2; i 5 6; i 6 3; i 3 0; i 4 6; i 5 3])
let stick2 = new Stick ([ i 9 1; i 5 0; i 1 0; i 1 4; i 1 5; i 9 5])
let stick3 = new Stick ([ i 8 2; i 6 5; i 7 6; i 4 0; i 6 2; i 6 0])
let stick4 = new Stick ([ i 2 4; i 4 5; i 7 1; i 6 1; i 3 1; i 8 6])
let stick5 = new Stick ([ i 9 2; i 7 4; i 9 3; i 10 1; i 10 3; i 10 4])
let stick6 = new Stick ([ i 9 0; i 4 1; i 4 2; i 8 4; i 6 6; i 7 0])
let stick7 = new Stick ([ i 10 5; i 8 3; i 5 5; i 8 5; i 3 2; i 3 3])
let stick8 = new Stick ([ i 7 3; i 9 4; i 5 1; i 5 2; i 4 3; i 4 4])
let stick9 = new Stick ([ i 5 4; i 1 2; i 1 3; i 1 6; i 6 4; i 10 2])
let stick10 = new Stick       ([ i 1 1; i 7 2; i 10 6; i 2 6; i 10 0])
let stick11 = new Stick       ([ i 8 0; i 2 0; i 2 1; i 9 6; i 3 4])
*)

                        // LightBrown 0
                        // Brown 1
                        // Green 2
                        // Lilac 3
                        // Turquoies 4
                        // Red 5
                        // Blue 6
                        // LightYellow 7
                        // Orange 8
                        // LightBlue 9
(*                        
// 318164 - 12 Best - 79 Program - 79
let stick0  = s [5; 2; 1; 2; 3; 0] [8; 6; 7; 3; 0; 2]
let stick1  = s [4; 0; 4; 5; 4; 4] [3; 3; 4; 4; 8; 6]
let stick2  = s [5; 6; 1; 4; 2; 3] [6; 1; 3; 1; 2; 2]
let stick3  = s [1; 0; 0; 4; 0; 0] [5; 9; 5; 7; 7; 6]
let stick4  = s [6; 1; 2; 0; 6; 2] [3; 9; 9; 0; 8; 7]
let stick5  = s [1; 5; 6; 5; 4; 4] [0; 7; 7; 2; 5; 0]
let stick6  = s [3; 0; 4; 1; 5; 3] [7; 1; 9; 2; 5; 6]
let stick7  = s [5; 0; 1; 2; 4; 6] [1; 4; 4; 4; 2; 4]
let stick8  = s [5; 3; 3; 1; 1; 6] [3; 8; 1; 6; 8; 0]
let stick9  = s [3; 2; 3; 6; 5; 6] [4; 5; 5; 9; 9; 6]
let stick10 = s    [6; 6; 2; 3; 5] [   2; 5; 1; 9; 0]
let stick11 = s    [1; 2; 2; 0; 3] [   1; 8; 0; 8; 3]
*)

                        // LightBrown 0
                        // Brown 1
                        // Green 2
                        // Lilac 3
                        // Turquoies 4
                        // Red 5
                        // Blue 6
                        // LightYellow 7
                        // Orange 8
                        // LightBlue 9

// 317997 - 12 Best - 77 Program - 77
let stick0  = s [3; 0; 3; 4; 0; 0] [2; 7; 5; 5; 1; 9]
let stick1  = s [4; 5; 1; 4; 3; 5] [7; 4; 9; 6; 6; 5]
let stick2  = s [5; 6; 6; 4; 2; 6] [8; 0; 1; 3; 5; 8]
let stick3  = s [5; 3; 1; 3; 2; 0] [0; 8; 6; 3; 7; 8]
let stick4  = s [0; 2; 3; 4; 5; 1] [5; 8; 9; 9; 9; 2]
let stick5  = s [6; 4; 4; 2; 1; 2] [2; 4; 1; 4; 3; 3]
let stick6  = s [1; 0; 2; 3; 5; 5] [8; 3; 9; 1; 6; 2]
let stick7  = s [6; 1; 0; 6; 4; 3] [9; 4; 4; 6; 2; 7]
let stick8  = s [0; 4; 1; 2; 3; 5] [6; 8; 7; 2; 0; 1]
let stick9  = s [0; 5; 6; 1; 6; 6] [0; 7; 7; 5; 4; 5]
let stick10 = s    [4; 1; 2; 1; 2] [   0; 0; 0; 1; 1]
let stick11 = s    [5; 6; 0; 2; 3] [   3; 3; 2; 6; 4]



// let field = new Field ([| stick0; stick1; stick2; stick3; stick4; stick5; stick6; stick7 |])
let field = new Field ([| stick0; stick1; stick2; stick3; stick4; stick5; stick6; stick7; stick8; stick9; stick10; stick11 |])
    
solve field
