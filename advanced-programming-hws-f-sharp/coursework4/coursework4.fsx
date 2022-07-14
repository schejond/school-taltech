(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: FSharpON

  ------------------------------------
  Name: Ondřej Schejbal
  Tallinn University of Technology Student ID
  or Uni-ID: 214308IV
  ------------------------------------

  Answer the questions below. Your answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060-2021 under your name,
  into a file coursework4/coursework4.fsx.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or
  directory is incorrect it will not be graded.

  Your solution must not use mutable state and imperative features
  like for loops.
*)


(*
The ECMA-404 standard specifies a textual syntax for structured data
interchange.

The specification is available here:
https://www.ecma-international.org/wp-content/uploads/ECMA-404_2nd_edition_december_2017.pdf

The goal of this coursework is to develop a partial implementation of
this specification. In particular, our first goal is to define in F#
the datatype(s) suitable for representing the abstract syntax tree of
this data interchange format. The second goal is to define some
operations on this representation.
*)

// We have the following type alias.

// Task 1
// Define the type `Ecma` for representing the possible values of the
// ECMA-404 interchange format.
//
// The type must satisfy the constraint `equality`.
// type Number =
//   | Float of float
//   | Int of int
//   | Double of double
type Name = string
type Ecma =
  | Number of float
  // | Number of Number
  | Text of string
  | Logical of bool
  | Array of Ecma list
  | Object of (Name * Ecma) list //Map<Name, Ecma>
  | Empty

// Define the following functions for creating ECMA-404
// representations of the given data

// Define the function
//
// mkObject : unit -> Ecma
//
// that creates a representation for an empty object structure.
let mkObject () : Ecma =
  Object ([])

// Define the function
//
// mkNumber : float -> Ecma
// that creates a representation for the given floating-point number.
let mkNumber (f:float) : Ecma =
  Number f
  // Number (Float f)

// Define the function
//
// mkBool : bool -> Ecma
//
// that creates a representation for the given Boolean value.
let mkBool (b:bool) : Ecma =
  Logical b

// Define the function
//
// mkString : string -> Ecma
//
// that creates a representation for the given string value.
let mkString (s:string) : Ecma =
  Text s

// Define the function
//
// mkArray : Ecma list -> Ecma
//
// that creates a representation for an array whose elements are
// represented by the given list of `Ecma` values.
let mkArray (ecmaList:Ecma list) : Ecma =
  Array (ecmaList)
// mkArray [Number 2.3; Logical true; Object (Map.empty)]

// Define the function
//
// mkNull : unit -> Ecma
//
// that creates a representation of the ECMA-404 `null` value.
let mkNull () : Ecma =
  Empty


// Task 2
// Define the function
//
//   addNameValue : Name * Ecma -> Ecma -> Ecma
// 
//   so that
// 
//   addNameValue (n, v) e
// 
// evaluates to an ECMA representation e' that is:
// - equal to e if e is not an object representation
//
// - a representation for the object e extended with the name-value 
//   pair (n, v), otherwise.
let addNameValue (n:Name, v:Ecma) (e:Ecma) : Ecma =
  match e with
    | Object o -> Object(o @ [(n, v)])
    | _ -> e

// Define the function
// 
//   addValue : Ecma -> Ecma -> Ecma
// 
// so that
// 
//   addValue v e
// 
// evaluates to an ECMA representation e' that is:
// - equal to e if e is not an array representation
// 
// - a representation for the array e with the value v added as the last
//   element, otherwise.
let addValue (v:Ecma) (e:Ecma) : Ecma =
  match e with
    | Array e -> Array (e @ [v])
    | _ -> e


// Task 3
// Define the function
// 
//   countValues : Ecma -> int
// 
// that counts the number of ECMA values in the given representation.
// 
// Keep in mind that both objects and arrays are themselves values and
// may contain other values inside.
//
// Furthermore, the following should hold:
//
//   1 + countValues e <= countValues (addValue v e)             // if e is an array representation
//
//   1 + countValues e <= countValues (addNameValue (n, v) e)    // if e is an object representation
let rec countValues (e:Ecma) : int =
  match e with
    | Array arr -> (arr |> List.fold (fun acc el -> (countValues el) + acc) 1)
    | Object o -> (o |> List.fold (fun acc (k, v) -> (countValues v) + acc) 1)
    | _ -> 1
// countValues (Array ([Number 2.3; Logical true; Logical true; Object ([])])) // --> 5
// countValues (Number 3.0) // --> 1
// countValues (Array ([Number 2.3; Logical true; Logical true; Object ([]) ; Array([Empty])])) // --> 7
// let person: (Name * Ecma) list =
//     [ "name", Text "Adam"
//       "age", Number 33.3 ]
// let capitals: (Name * Ecma) list =
//     [ "Argentina", Empty
//       "France ", Text "Paris"
//       "Chili", Object person
//       "Malaysia", Text "Kuala Lumpur"
//       "Switzerland", Text "Bern" ]
// let e: Ecma = Object(capitals)
// countValues e // --> 8

// Task 4
type Path = Name list
// Define the function
// 
//   listPaths : Ecma -> Path list
// 
// that computes the full path for all the values in the given ECMA
// representation.
// 
// A path is just a list of names that take us from the root of the
// representation to a particular value.
// 
// For arrays, we consider the same path to take us to the array and to
// all of the elements in the array. Thus, for an array, we include the
// path to it and its elements only once in the result. 
// 
// If `e : Ecma` represents the following structure
// 
//   {
//     "abc" : false,
//     "xs"  : [ { "a" : "a" }, 1.0, true, { "b" : "b" }, false ],
//     "xyz" : { "a" : 1.0,
//               "b" : { "b" : "b" } },
//     "ws"  : [ false ]
//   }
// 
// then  `listPaths e` should result in
// 
//   [
//     [];
//     ["abc"];
//     ["xs"];
//     ["xs"; "a"];
//     ["xs"; "b"];
//     ["xyz"];
//     ["xyz"; "a"];
//     ["xyz"; "b"];
//     ["xyz"; "b"; "b"];
//     ["ws"]
//   ]
// 
// The ordering of paths in the result list matters:
// - paths to (sub)values in an array respect the order of elements in
//   the array
// 
// - paths to values in an object respect the order in which the values
//   were added to the object (most recently added appears last).
//
// Note that the empty list denotes the path to the root object.
let rec listPathsUsingPrefix (e: Ecma) (prefix: Path) : Path list =
  match e with
    | Object o -> o |> List.collect (fun (key, value) ->
                                        let newPrefix = prefix @ [ key ]
                                        [newPrefix] @ listPathsUsingPrefix value newPrefix)
    | Array arr -> arr |> List.collect(fun el -> listPathsUsingPrefix el prefix)
    | _ -> []

let listPaths (e: Ecma) : Path list =
  [] :: match e with
        | Object _ -> listPathsUsingPrefix e []
        | Array _ -> listPathsUsingPrefix e []
        | _ -> []
// listPaths (Logical true) // -> [[]]
// listPaths (Array ([Number 1.0; Logical true; Number 2.0; Array([Number 1.0; Logical false])])) // -> [[]]
// listPaths (Object ([])) // -> [[]]
// listPaths (Array ([ Object ([]) ]) ) // -> [[]]
// listPaths (mkString "abc")
// listPaths (Object(["a", Object (["a1", Number 1.0]) ; "b", Number 3.0 ; "c", Object (["c1", Object (["c2", Number 6.0])])])) // -> [[]; ["a"]; ["a"; "a1"]; ["b"]]

// Task 5
// Define the function
// 
//   show : Ecma -> string
// 
// that computes a string representation of the given ECMA representation
// in such a way that the ordering requirements from the previous task are
// respected.
// 
// The result should not contain any whitespace except when this
// whitespace was part of a name or a string value.
let rec show (e:Ecma) : string =
  match e with
    | Array arr -> "[" + ("]"
                           |> List.foldBack (fun el acc ->
                                                  show el + (if String.length acc <> 1 then "," else "") + acc
                                            ) arr
                          )
    | Object arr -> "{" + ("}"
                            |> List.foldBack (fun (k,v) acc ->
                                                          "\"" + k + "\":" +
                                                          (show v) + (if String.length acc <> 1 then "," else "") + acc
                                             ) arr
                          )
    | Number n -> string n
    | Logical b -> (string b).ToLower()
    | Text s -> "\"" + s + "\""
    | Empty -> "null"
// let x = Array ([Number 1.0; Text "Ahoj"; Logical false ; Array ([Number 1.0; Logical false]) ])
// let x = Object ([("text", Array ([Number 1.0; Logical false; Number 1.0; Number 1.0 ; Array ([]);Object ([])])); ("jojo", Number 1.0)])
// let x = Object([])
// show x


// Task 6
// Define the function
// 
//   delete : Path list -> Ecma -> Ecma
// 
// so that
// 
//   delete ps e
// 
// evaluates to a representation `e'` that is otherwise the same as `e` but
// all name-value pairs with paths in the path list `ps` have been removed.
//
// When the user attempts to delete the root object, delete should throw
// an exception. Hint: use `failwith` in the appropriate case.
let concatObjects (o1:Ecma) (o2:Ecma) : Ecma =
  match o1, o2 with
    | Object a, Object b -> Object(a@b)
    | _ -> failwith "received unexpected ecma types"

let rec deleteInner (e: Ecma) (prefix: Path) (pl: Path list) : Ecma =
  match e with
    | Object ((name, value) :: xs) ->
        let newPrefix = prefix @ [ name ]

        // let valuePathList :Path list = [ newPrefix ] @ listPathsUsingPrefix value newPrefix @ listPathsUsingPrefix (Object xs) prefix
        if (pl |> List.contains (newPrefix))
        then deleteInner (Object(xs)) prefix pl
        else concatObjects (Object ( [(name, deleteInner value newPrefix pl)]) ) (deleteInner (Object xs) prefix pl)//  :: Object([ listPathsUsingPrefix2 ( Object(xs) ) prefix pl ]))
    | Array (x::xs) ->
        Array([(deleteInner x prefix pl)] @ [deleteInner (Array(xs)) prefix pl])
    | _ -> e

let delete (pathList: Path list) (e:Ecma) : Ecma =
  match e with
    | Object([]) -> failwith "Cannot delete root object"
    | Object _ -> deleteInner e [] pathList
    | _ -> e

// Task 7
// Define the function
// 
//   withPath : Path list -> Ecma -> Ecma list
// 
// so that
// 
//   withPath ps e
// 
// evaluates to a list of object representations consisting of those
// objects in `e` that are represented by a path from the list `ps`.
// 
// The result list must respect the ordering requirements from Task 4.
let withPath (pathList: Path list) (e:Ecma) : Ecma list =
  // pathList
  // |> List.fold (fun acc el -> []) []
  []
  // failwith "todo"