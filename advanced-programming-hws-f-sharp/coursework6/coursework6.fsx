(*
  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 6: Tail recursion

  ------------------------------------------------
  Name: Ondřej Schejbal
  Student ID: 214308IV
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework6.fsx in directory coursework6.

  The deadline for completing the above procedure is Sunday, November 7, 2021.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.
*)

(*
  Task 1:

  Write a function
  pHoldsForAllSequentialElements : (int -> int -> bool) -> int list -> bool

  that when calling the function

  pHoldsForAllSequentialElements p xs

  will check if predicate p holds for all consequtive elements in the list. If it does,
  the function should return true, else false.

  In the case there is less than two elements in the list it is assumed that
  the predicate does not hold as there are no sequential elements present.

  NB! You are required to implement the function in a tail recursive way using
  explicit recursion.
*)
let rec pHoldsForAllSequentialElements (p:int -> int -> bool) (xs:int list) : bool =
    match xs with
        | x::y::rest when p x y -> if rest.IsEmpty
                                   then true 
                                   else pHoldsForAllSequentialElements p (y::rest)
        | _ -> false
// pHoldsForAllSequentialElements (fun x y -> x < y) [2;3]


(*
  Task 2:

  Write a function
  createTwoTuplesOfList : 'a -> 'a list -> ('a * 'a) list
  that takes a list of 'a-s and returns a list of two element tuples of 'a-s that are taken
  sequentially from the list passed as the second argument to the function.
  In case the list has odd number of elements make the first argument of the
  function be the second element in the tuple. 
  Make sure your implementation uses explicit tail recursion.
*)
let createTwoTuplesOfList (additionalEl:'a) (arr:'a list) : ('a * 'a) list =
  let editedArr = if arr.Length % 2 <> 0 then arr @ [additionalEl] else arr

  let rec createTwoTouplesOfListsInner arr1 arr2 = 
    match arr1 with
    | x::y::rest -> createTwoTouplesOfListsInner rest ((x,y)::arr2)
    | _ -> arr2

  List.rev (createTwoTouplesOfListsInner editedArr [])
// createTwoTouplesOfLists 1 [1;2;3]


(*
  Task 3:

  Write a function
  createTwoTuplesOfListFold : 'a -> 'a list -> ('a * 'a) list
  that takes a list of 'a-s and returns a list of two element tuples of 'a-s that are taken
  sequentially from the list passed as the second argument to the function. In case
  the list has odd number of elements make the first argument of the function be the
  second element in the tuple. 
  Make sure your implementation uses List.fold or List.foldBack appropriately.
  Test yourself if this implementation appears to be tail recursive.
*)
let createTwoTuplesOfListFold (additionalEl:'a) (arr:'a list) =
  let editedArr = if arr.Length % 2 <> 0 then arr @ [additionalEl] else arr
  ([], (None:'a option)) |> List.foldBack (fun el (accArr, maybeNumber) -> match maybeNumber with
                                                                            | None -> (accArr, Some el)
                                                                            | Some s -> ((el, s)::accArr, None)
                          ) editedArr |> fst
// createTwoTuplesOfListFold 1 [1;2;3]


(*
  Task 4:

  Below you find the definition of a type Tr of leaf-labeled trees. Write a
  function
  
  medianAndAverageInTree : int Tree -> int * float
  
  that returns a pair where the first element is the median label in the
  given tree and the second is an average across all nodes.
  
  The median is the label for which the difference in counts of elements to the right and left is
    either 0 or
    the count of elements to the right is exactly 1 greater than the count of elements to the left.
  The average is the sum of all elements divided with the number of elements.
  Use continuation-passing style in your implementation and perform the operations
  in a single pass of the tree.
*)

type 'a Tr =
  | Lf   of 'a
  | Br of 'a Tr * 'a Tr

let rec medianAndAverageInTreeInner (tree: int Tr) (acc: int * int * int list -> int * int * int list) : int * int * int list =
  match tree with
    | Lf l -> acc (1, l, [l])
    | Br (tl, tr) ->  // printfn "tl: %A tr: %A" tl tr
                      medianAndAverageInTreeInner tl (fun (nodesCntL, sumL, listValuesL) ->
                                                      // printfn "nodesCntL: %A sumL: %A listValuesL: %A" nodesCntL sumL listValuesL
                                                      medianAndAverageInTreeInner tr (fun (nodesCntR, sumR, listValuesR) ->
                                                        // printfn "nodesCntR: %A sumR: %A listValuesR: %A" nodesCntR sumR listValuesR
                                                        acc(nodesCntL + nodesCntR, sumL + sumR, listValuesL @ listValuesR)))

let medianAndAverageInTree (tree: int Tr): int * float =
  let nodesCnt, sum, list =  medianAndAverageInTreeInner tree id
  // printfn "FINAL -> nodesCnt: %A sum: %A list: %A" nodesCnt sum list
  (list.[(nodesCnt - 1) / 2], float(sum) / float(nodesCnt))

// let x : int Tr = Br(Br(Lf(1), Lf(2)), Br(Lf(3), Lf(4)))
// medianAndAverageInTree x