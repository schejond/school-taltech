(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 5: Performing queries on FSharpON data structures

  ------------------------------------
  Name: Ondřej Schejbal
  Tallinn University of Technology Student ID
  or Uni-ID: 214308IV
  ------------------------------------

  Answer the questions below. You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060-2021 under your name,
  into a file coursework5/coursework5.fsx.

  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or
  directory is incorrect it will not be graded.

  Your solution must not use mutable state and imperative features
  like for loops.
*)


(*
For introduction to FSharpON please check coursework4.fsx for references.

In this coursework we continue with the topic of trees and object notation. This
time the task is, given a description of a set of values in an Ecma,
how to retrieve, modify and delete those values. This is a bit similar
to questions 6 and 7 in coursework 4.

The following material of basic definitions is taken from CW4. You are
free to use your solution to CW4 or try something different.
*)

type Name = string

type Ecma =
    | Number of float
    | Text of string
    | Logical of bool
    | Array of Ecma list
    | Object of (Name * Ecma) list //Map<Name, Ecma>
    | Empty

// 0. Define data structure(s) for representing FsharpON (same as in CW4)
// type Ecma = ... uncomment and write definition here ...
let mkObject () : Ecma = Object([])
let mkNumber (f: float) : Ecma = Number f
let mkBool (b: bool) : Ecma = Logical b
let mkString (s: string) : Ecma = Text s
let mkArray (ecmaList: Ecma list) : Ecma = Array(ecmaList)
let mkNull () : Ecma = Empty
// Define the function
//
//   addNameValue : Name * Ecma -> Ecma -> Ecma
//
// so that
//
//   addNameValue (n, v) e
//
// evaluates to an ECMA representation e' that is:
// - equal to e if e is not an object representation
//
// - a representation for the object e extended with the name-value
//   pair (n, v), otherwise.
let addNameValue (n: Name, v: Ecma) (e: Ecma) : Ecma =
    match e with
    | Object o -> Object(o @ [ (n, v) ])
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
let addValue (v: Ecma) (e: Ecma) : Ecma =
    match e with
    | Array e -> Array(e @ [ v ])
    | _ -> e

////////////////////////////////////////////////////////////////////////
// The content of this coursework begins here
// You are given a type of expressions.
type BExpr =
    | True
    | Not of BExpr
    | And of BExpr * BExpr
    | Or of BExpr * BExpr
    | HasKey of Name
    | HasStringValue of string
    | HasNumericValueInRange of (float * float)
    | HasBoolValue of bool
    | HasNull
(*
The type BExpr is just a discriminated union. The intended
interpretation of values of type BExpr is as predicates on values of
type Ecma.

 - True: evaluates to true on any Ecma

 - Not b: evaluates to true on precisely those Ecma for which b
          evaluates to false

 - And (b1, b2): evaluates to true on precisely those Ecma for which
                 both b1 and b2 evaluate to true

 - Or (b1, b2): evaluates to true on precisely those Ecma for which at least
                one of b1 and b2 evaluates to true

 - HasKey k: evaluates to true on precisely those Ecma that are objects and
             that contain a key k.

 - HasStringValue s: evaluates to true on precisely those Ecma that are
            either Ecma strings with value s, objects that contain a value s,
            or arrays that contain a value s.

 - HasNumericValueInRange (xmin,xmax): evaluates to true on precisely those Ecma
               that either are
                  numeric Ecma with value in closed range xmin,xmax,
                  objects with a numeric value in closed range xmin,xmax,
                  arrays with a numeric value in closed range xmin,xmax.

  - HasBoolValue b: evaluates to true on precisely those Ecma that are either
                    Boolean Ecma with value b,
                    objects that contain a Boolean value b,
                    arrays that contain a Boolean value b.
  - HasNull : evaluates to true on precisely those Ecma that are either
                    null Ecmas,
                    objects that contain a null value,
                    arrays that contain a null value.
*)
// Here is a type of selector expressions.
type Selector =
    | Match of BExpr
    | Sequence of Selector * Selector
    | OneOrMore of Selector
(*
The type Selector is just a discriminated union. The intended
interpretation of values of type Selector on values of type Ecma is as
sets of values in that Ecma. We also refer to the set of values
described by s : Selector as the set of values selected by s.

 - Match b: the singleton set consisting of the root value if the
            expression b evaluates to true.
            Empty set otherwise.

 - Sequence (s, s'): the set consisting of those values in the Ecma tree
                     that are selected by the selector s' starting from
                     any child value of a value that is selected by the
                     selector s (starting from the root value).

                     In other words, first determine the set of values
                     selected by s (starting from the root value). For
                     every child c of a value in this set, determine
                     the set of values selected by s' (starting from c)
                     and take the union of such sets as the result.

                     In other words, start from the root value with the
                     selector s. For the values that it selects,
                     continue with selector s' from their child values
                     and collect the results.

 - OneOrMore s: select the values selected by the selector s and, in
                addition, from the child values of the values in this
                set select the values selected by OneOrMore s.

                Thus, you can think of the values selected by OneOrMore s
                as the union of the following sets:
                - values selected by s
                - values selected by Sequence (s, OneOrMore s)
*)


// 1. Translate the following informal descriptions into values of
// type BExpr and Selector.
//
// Define the values b1, b2 and b3 of type BExpr so that:
//
//  - b1 evaluates to true on those Ecma that are object values
//    containing the keys "blue" and "left" but do not have the key "red".
//
//  - b2 evaluates to true on those Ecma that are numeric values with
//    the value in the range [-5, 5).
//
//  - b3 evaluates to true on those Ecma that have the string value "b3"
//    or that are object values which have the key "b3".
//
// Define the values s1, s2 and s3 of type Selector so that:
//
//  - s1 selects all object values with key "abc" that are at depth 3
//
//  - s2 selects all values v such that v is a child of some value
//    and all of the ancestors of v have the string value "xyz"
//
//  - s3 selects all values v such that:
//    * v is a child of a value t
//    * t does not have a string value "xyz"
//    * t is the root value
//
// We consider the root value to be at depth 1.
let b1 =
    And(And(HasKey("blue"), HasKey("left")), Not(HasKey("red")))

let b2 =
    And(HasNumericValueInRange(-5.0, 5.0), Not(HasNumericValueInRange(5.0, 5.0)))

let b3 = Or(HasKey("b3"), HasStringValue("b3"))

let s1 =
    Sequence(Match True, Sequence(Match True, Match(HasKey("abc"))))

let s2 =
    Sequence(OneOrMore(Match(HasStringValue("xyz"))), Match True)

let s3 =
    Sequence(Match(Not(HasStringValue("xyz"))), Match True)


// 2. Define the function
//
// eval : BExpr -> Ecma -> Logical
//
// which evaluates the given expression on the given Ecma.
//
// Evaluating a BExpr only considers properties of the root value of
// the Ecma and its immediate child values that are leaves (if there are any).
//
// In other words, for any  b : BExpr  and  e : Ecma
//
//    eval b e = eval b e'
//
// where e' is e with all of its non-leaf child values replaced
// with the representation for null.
let isEcmaSuitableToEcmaRepresentation (value: Ecma) (e: Ecma) : bool =
    match value, e with
    | Text t1, Text t2 -> t1 = t2
    | Number n, Array ([ Number xmin; Number xmax ]) -> xmin <= n && n <= xmax
    | Logical b1, Logical b2 -> b1 = b2
    | Empty, Empty -> true
    | _ -> false

let rec eval (expr: BExpr) (e: Ecma) : bool =
    match expr, e with
    | True, _ -> true
    | Not b, e -> not (eval b e)
    | And (b1, b2), e -> (eval b1 e) && (eval b2 e)
    | Or (b1, b2), e -> (eval b1 e) || (eval b2 e)

    | HasKey k, Object o -> o |> List.exists (fun (key, _) -> key = k)

    | HasStringValue s, Text t -> s = t
    | HasStringValue s, Object o ->
        o
        |> List.exists (fun (_, v) -> isEcmaSuitableToEcmaRepresentation v (Text s))
    | HasStringValue s, Array arr ->
        arr
        |> List.exists (fun el -> isEcmaSuitableToEcmaRepresentation el (Text s))

    | HasNumericValueInRange (xmin, xmax), Number n -> xmin <= n && n <= xmax
    | HasNumericValueInRange (xmin, xmax), Object o ->
        o
        |> List.exists (fun (_, v) -> isEcmaSuitableToEcmaRepresentation v (Array([ (Number xmin); (Number xmax) ])))
    | HasNumericValueInRange (xmin, xmax), Array arr ->
        arr
        |> List.exists (fun el -> isEcmaSuitableToEcmaRepresentation el (Array([ (Number xmin); (Number xmax) ])))

    | HasBoolValue b, Logical l -> b = l
    | HasBoolValue b, Object o ->
        o
        |> List.exists (fun (_, v) -> isEcmaSuitableToEcmaRepresentation v (Logical b))
    | HasBoolValue b, Array arr ->
        arr
        |> List.exists (fun el -> isEcmaSuitableToEcmaRepresentation el (Logical b))

    | HasNull, Empty -> true
    | HasNull, Object o ->
        o
        |> List.exists (fun (_, v) -> isEcmaSuitableToEcmaRepresentation v Empty)
    | HasNull, Array arr ->
        arr
        |> List.exists (fun el -> isEcmaSuitableToEcmaRepresentation el Empty)

    | _ -> false

// let x = Object([("klic",Text "ahoj")])
// let y = HasStringValue "ashoj"
// eval y x


type Description =
    | Key of string
    | Index of int

type Path = Description list
// 3. Define the function
//
// select : Selector -> Ecma -> (Path * Ecma) list
//
// that computes the set of values in the given Ecma described by the
// given Selector. The result is a list of pairs where the second
// component is a selected value and the first component is the full path
// to this value.
//
// The path to the root value is the empty list.
//
// If you follow a child value of an object, then you add the key of
// that value to the path. If you follow a child of an array, then you
// add the index of that value in the array to the path (the oldest value
// has index 0).
//
// The order of values in the result list must respect the order of values
// in the given Ecma. More precisely, in the result list:
// - a value must appear before any of its children
// - a value must not appear before its older siblings and their
//   descendants
//
// This task is similar to evaluating a BExpr on an Ecma. The difference is
// that instead of a BExpr we have a Selector and instead of a Logical we
// compute a (Path * Ecma) list. In this case we also consider child
// values.
let rec selectInner (s: Selector) (e: Ecma) (p: Path) : (Path * Ecma) list =
    // printfn "objekt: %A a selektor: %A Path: %A" e s p
    match s with
    | Match expr -> if (eval expr e) then [ p, e ] else []
    | Sequence (s, s') ->
        let x = selectInner s e p

        match x with
        | [] -> []
        | arr ->
            arr
            |> List.fold
                (fun resultList (p', e') ->
                    resultList
                    @ match e' with
                      | Object o ->
                          o |> List.collect (fun (key, value) -> selectInner s' value (p' @ [ Key(key) ]))
                      | Array eArr ->
                          fst (
                              eArr
                              |> List.fold
                                  (fun (arrInner, i) el -> (arrInner @ (selectInner s' el (p' @ [ Index(i) ])), i + 1))
                                  ([], 0)
                          )
                      | _ -> selectInner s' e' p'

                    // | Array (x :: xs) ->
                    //     selectInner s' x ( p' @ [ Index(0) ])
                    //     @ selectInner s' (Array(xs)) p'
                )
                []

    | OneOrMore s ->
        let x = selectInner s e p

        x
        @ match e with
          | Object _ -> selectInner (Sequence(s, OneOrMore s)) e p
          | Array _ -> selectInner (Sequence(s, OneOrMore s)) e p
          | _ -> []

let select (s: Selector) (e: Ecma) : (Path * Ecma) list = selectInner s e []

// 4. Define the function
//
// update :  (string -> string)
//        -> (float  -> float)
//        -> Selector
//        -> Ecma
//        -> Ecma
//
// such that
//
//    update su nu s e
//
// evaluates to an Ecma that is otherwise the same as e except that,
// for the values selected by s, the string values and numeric values
// of that value have been updated according to the functions su and nu.
let updateTextOrNumber (su: string -> string) (nu: float -> float) (e: Ecma) : Ecma =
    match e with
    | Text t -> Text(su t)
    | Number n -> Number(nu n)
    | _ -> e

let updateEcma (su: string -> string) (nu: float -> float) (e: Ecma) : Ecma =
    match e with
    | Object o -> Object(List.map (fun (name, e') -> (name, updateTextOrNumber su nu e')) o)
    | Array l -> Array(List.map (fun e' -> updateTextOrNumber su nu e') l)
    | _ -> updateTextOrNumber su nu e

// let concatObjects (o1:Ecma) (o2:Ecma) : Ecma =
//   match o1, o2 with
//   | Object(x), Object(y) -> Object(x@y)
//   | _ -> printfn "o1:%A\no2:%A" o1 o2
//          failwith "Expected to obtain 2 objects"
// let rec innerUpdate (su: string -> string) (nu: float -> float) (e:Ecma) (path:Path) (ecmaToMatch:Ecma) : Ecma =
//   match path with
//   | [] -> if e = ecmaToMatch then updateEcma su nu e else e
//   | [x] -> match e with
//             | Object((key,value)::xs) -> if Key(key) = x && value = ecmaToMatch
//                                          then concatObjects (Object([key, (updateEcma su nu value)])) (innerUpdate su nu (Object(xs)) path ecmaToMatch)
//                                          else concatObjects (Object([key, value])) (innerUpdate su nu (Object(xs)) path ecmaToMatch)
//             | Object([]) -> Object([])
//             | Array arr -> match x with
//                             | Index i -> Array(arr |> List.mapi (fun idx el -> if i = idx then updateEcma su nu el else el))
//                             | _ -> failwith "Unexpected scenario 0"
//             | _ -> e
//   | h::tail -> match e with
//                 | Object([]) -> Object([])
//                 | Object((key,value)::xs) -> if Key(key) = h
//                                              then concatObjects (Object([key, (innerUpdate su nu value tail ecmaToMatch)])) (innerUpdate su nu (Object(xs)) path ecmaToMatch)
//                                              else concatObjects (Object([key,value])) (innerUpdate su nu (Object(xs)) path ecmaToMatch)
//                 | Array arr -> Array(arr |> List.mapi (fun i el -> if Index(i) = h then innerUpdate su nu el tail ecmaToMatch else el))
//                 | _ -> failwith "Unexpected scenario 1"
// let update (su:(string -> string)) (nu:(float -> float)) (s:Selector) (e:Ecma) : Ecma =
//   let pathEcmaList = select s e
//   pathEcmaList |> List.fold (fun accumulatedE (path, e') -> innerUpdate su nu accumulatedE path e') e

// let suFun (s: string) = s + " CHANGED"
// let nuFun (n: float) = n - 50.0

let rec updateInnerNew (su: string -> string) (nu: float -> float) (sel: Selector) (e: Ecma) (doUpdate: bool) : Ecma * bool =
    match sel with
    | Match n ->
        let evalRes = eval n e

        if evalRes && doUpdate then
            updateEcma su nu e, true
        else
            e, evalRes
    | Sequence (s, s') ->
        let x = updateInnerNew su nu s e false

        if not (snd (x)) then
            e, false
        elif not doUpdate then
            x
        else
            match fst (x) with
            | Object o ->
                Object(
                    o
                    |> List.map (fun (key, e') -> (key, fst (updateInnerNew su nu s' e' doUpdate)))
                ),
                true
            | Array arr ->
                Array(
                    arr
                    |> List.map (fun e' -> fst (updateInnerNew su nu s' e' doUpdate))
                ),
                true
            | _ -> e, true
    | OneOrMore s ->
        let x = updateInnerNew su nu s e true
        updateInnerNew su nu (Sequence(s, OneOrMore s)) (fst (x)) true

let update (su: (string -> string)) (nu: (float -> float)) (s: Selector) (e: Ecma) : Ecma =
    let x = updateInnerNew su nu s e true
    if not (snd (x)) then e else fst (x)

// let objE2 = Number 6.6
// Object([ "a", Number 1.0; "b", Logical false ])
// let objE2 =
//     Object(["a", Number 1.0;
//             "b", Logical false;
//             "c", Object([
//                 "d", Number 1.5;
//                 "x", Array([
//                     Logical true;
//                     Number 2.0
//                     ]);
//                 "z", Logical false
//                 ]);
//             "z", Text "abcdef"
//     ])

// let seque = Sequence(Match (Not True), Match True)
// let seque = Sequence(Match True, Match (HasKey "d"))
// let seque = Sequence(Match True, Sequence (Match (HasKey "d"), Match (HasNumericValueInRange (1.0, 3.0))))
// let seque = Sequence(Match (Not(True)), Match (Not(True)))
// let seque = Sequence(Match (Not(True)), Match (Not(True)))
// let seque = Sequence(Sequence(Match (HasKey "c"), Match True), Match True)

// let updRes =  update suFun nuFun seque objE2
// let updNewRes = updateNew suFun nuFun seque objE2
// printfn "Old: \n%A" updRes
// printfn "New: \n%A" updNewRes
// if (updRes = updNewRes) then printfn "%s" "THEY MATCH" else printfn "%s" "THEY DIFFER"
// select seque objE2

// 5. Define the function
//
// delete : Selector -> Ecma -> Ecma option
//
// which removes from the given Ecma all values that are selected by
// the given Selector. Removing a value means removing the entire
// subtree rooted at that value.
//
// The result should be `None` when after the delete operation there
// is no `Ecma` value left. Otherwise use `Some`.
let rec innerDelete (sel: Selector) (e: Ecma) (doDelete: bool) : Ecma option = // todo or ecma option
    // printfn "Mam selektor: %A e: %A doDelete: %A" sel e doDelete
    match sel with
    | Match n -> //printfn "MATCH eval = %A" (eval n e) ;
        if eval n e then
            if doDelete then None else Some e
        else Some e
    | Sequence (s, s') ->
        // printfn "SEQUENCE, posilam x"
        let x = innerDelete s e false
        // printfn "\tx = %A" x
        match x with
        | None -> Some e
        | Some xx ->
            match xx with // muze byt i x, meli by se =
            | Object o -> //printfn "\tFiltruju object pro e = %A" e
                let filteredO =
                    (o
                     |> List.fold
                         (fun acc (key, value) ->
                             let y = innerDelete s' value doDelete
                             //    printfn "\tPro value = %A se mi vratilo y = %A" value y
                             acc
                             @ match y with
                               | Some filteredVal -> [ (key, filteredVal) ]
                               | _ -> [])
                         [])
                //    printfn "FILTERED OBJ: %A" filteredO
                if filteredO |> List.isEmpty then
                    None
                else
                    Some(Object(filteredO))
            | Array arr -> //printfn "\tFiltruju array pro e = %A" e
                let filteredArr =
                    (arr
                     |> List.fold
                         (fun acc el ->
                             let y = innerDelete s' el doDelete
                             //   printfn "\tPro el = %A se mi vratilo y = %A" el y
                             acc
                             @ match y with
                               | Some filteredVal -> [ filteredVal ]
                               | _ -> [])
                         [])
                // printfn "FILTERED OBJ: %A" filteredArr
                if filteredArr |> List.isEmpty then
                    None
                else
                    Some(Array(filteredArr))
            | _ -> //printfn "\t _ vetev, pracuji s e = %A Vracime Some e"
                Some e
    | OneOrMore s ->
        let x = innerDelete s e true

        match x with
        | Some value -> innerDelete (Sequence(s, OneOrMore s)) value true
        | _ -> None

let delete (s: Selector) (e: Ecma) : Ecma option =
    innerDelete s e true
// delete seque objE2

// 6. Using the function update, define the functions
//
//   toZero : float -> Selector -> Ecma -> Ecma
//
// and
//
//   truncate : int -> Selector -> Ecma -> Ecma
//
// so that
//
//   toZero x s e
//
// evaluates to an Ecma that is otherwise the same as e except that the
// values selected by s have each of their numeric values y replaced by 0
// if y is in the range [-x, x].
//
//   truncate n s e
//
// evaluates to an Ecma that is otherwise the same as e except that the
// values selected by s have each of their string values y truncated to
// length n.
//
// These functions should not be defined recursively; define them in
// terms of update.
let toZero (x: float) (s: Selector) (e: Ecma) : Ecma =
    let nu (y: float) : float = if -x <= y && y <= x then 0.0 else y
    update id nu s e

let truncate (n: int) (s: Selector) (e: Ecma) : Ecma =
    let su (y: string) : string =
        match y with
            | null -> y
            | y when y.Length <= n -> y
            | _ -> y.[0..n - 1]

    update su id s e


// 7. Using the function update, define the function
//
//   mapEcma : (string -> string) -> (float -> float) -> Ecma -> Ecma
//
// such that
//
//   mapEcma f g e
//
// evaluates to an Ecma obtained by updating every value in the
// given Ecma value according to f and g.
//
// This function should not be defined recursively; define it in
// terms of update.
let mapEcma (su: string -> string) (nu: float -> float) (e: Ecma) : Ecma =
    update su nu (OneOrMore(Match True)) e
