(*
  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 9: asynchronous programming

  ------------------------------------
  Name: Ondřej Schejbal
  Student ID: 214308IV
  ------------------------------------

  Answer the questions below. Your answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060-2021 under your name,
  into a file coursework9/coursework9.fsx
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or
  directory is incorrect, then it will not be graded.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

  NB! Do not delete the stubs we have provided! If you did not manage
  to complete the implementation then keep the incomplete stub and
  make sure that it still is typeable as required by the task.

  The submission deadline is Sunday, December 19.
*)

// Our representation of complex numbers.
type Complex = double * double

(*
   Question 1

   The Mandelbrot set.
   
   Formal definition:
   https://en.wikipedia.org/wiki/Mandelbrot_set#Formal_definition

   Basic properties:
   https://en.wikipedia.org/wiki/Mandelbrot_set#Basic_properties

   Define the function

   mandelbrot : int -> Complex -> bool

   so that 'mandelbrot n' is the characteristic function of the
   approximation of the Mandelbrot set by n iterations of the
   quadratic map.

   In other words, 'mandelbrot n' is a function which decides whether
   a complex number c belongs to the "n-th approximation" of the
   Mandelbrot set.

   In other words, define the function so that given

   n : int
   c : Complex

   'mandelbrot n c' evaluates to true precisely when, according to n
   iterations of the quadratic map, c belongs to the Mandelbrot set.

   The quadratic map is given in "Formal definition" and a condition
   for deciding whether c belongs to Mandelbrot (upto n iterations) is
   given in "Basic properties".

   Here are some hints:
   - Start by computing the z_1, z_2, ..., z_n for c. Based on this
     sequence of values decide whether c belongs to the (approximation
     of the) Mandelbrot set or not.

   - How to add and multiply complex numbers?

   - What is the absolute value of a complex number?
*)
let add (c1:Complex) (c2:Complex) : Complex =
   fst(c1) + fst(c2), (snd(c1) + snd(c2))

let multiply (c1:Complex) (c2:Complex) : Complex =
   let r1 = fst(c1)
   let r2 = fst(c2)
   let i1 = snd(c1)
   let i2 = snd(c2)
   r1 * r2 - i1 * i2, r1 * i2 + r2 * i1

let absolute (c:Complex) : double =
   let x_2 = fst(c) * fst(c)
   let y_2 = snd(c) * snd(c)
   sqrt (x_2 + y_2)

let mandelbrot (n:int) (c:Complex) : bool =
   let rec mandelbrotInner (n:int) (z_n:Complex) : bool =
      match n with
      | 0 -> (absolute z_n) <= 2.0
      | n when n > 0 && (absolute z_n) <= 2.0 -> mandelbrotInner (n - 1) (add (multiply z_n z_n) c)
      | _ -> false
   mandelbrotInner n (0.0, 0.0)
// for i in 0..20 do if not (mandelbrot i (-1.0, 0.0)) then failwith "chyba"


(*
   Question 2

   Define the function
 
   divide : int -> int -> (int * int) seq

   so that given

   m : int
   n : int

   'divide m n' evaluates to a sequence of ranges

   (s_1, e_1), ..., (s_m, e_m)

   so that:
   * s_1 = 0 and e_m = n - 1

   * s_{i + 1} = e_i + 1


   That is to say that:   
   * for every x : int such that 0 <= x < n there exists an i such
     that s_i <= x <= e_i

   * the sequence of (s_i, e_i) covers 0, ..., n - 1 without overlap,
     i.e., if s_i <= x <= e_i and s_j <= x <= e_j, then i = j.

   You may think of n as the number of things we have and m as the
   number of buckets among which we distribute them.

   Try to divide fairly.
*)
let divide (m:int) (n:int) : (int * int) seq =
   let mm = min m n
   let quotient = n / mm
   let remainder = n % mm
   // printfn "quotient=%A a remainder=%A pro mm=%A" quotient remainder mm
   seq {
      for i in 0..remainder-1 do
         // printfn "hej"
         // printfn "%A*(%A+1), (%A+1)*(%A+1) - 1" i quotient quotient i
         // printfn "%A , %A" (i*(quotient+1)) ((i+1)*(quotient+1) - 1)
         yield i*(quotient+1), (i+1)*(quotient+1) - 1
      for i in remainder..mm-1 do
         // printfn "hej2"
         // printfn "i = %A => %A , %A" i (i*quotient + remainder) ((i+1)*quotient-1 + remainder)
         yield i*quotient + remainder, (i+1)*quotient-1 + remainder
   }
// divide 13 3 |> Seq.toList
// divide 3 14 |> Seq.toList
// divide 3 6 |> Seq.toList


(*
   Question 3

   Define the function
   
   mandelbrotAsync :  int
                   -> int
		   -> (int -> unit)
		   -> (int -> unit)
		   -> Complex []
		   -> Async<bool []>

   so that given

   m      : int
   n      : int
   start  : int -> unit
   finish : int -> unit
   cs     : Complex []

   'mandelbrotAsync m n s e cs' creates an asynchronous computation
   which computes an array of booleans with the same size as cs. An
   element in this array is true precisely when the complex number at
   the corresponding position in cs belongs to the Mandelbrot set
   (according to n iterations).

   Use the 'divide' function to divide the work into m chunks (ranges)
   and then evaluate those in parallel using the Async mechanism.

   Whenever this function starts computing on a chunk of input (s, e),
   the function must apply the function 'start' to 's'. When it has
   completed working on the chunk, then it must apply 'finish' to 'e'.
*)
let mandelbrotAsync (m:int) (n:int) (start: int -> unit)
                    (finish: int -> unit) (cs: Complex[]) : Async<bool []> =
   async {
      let! results =
         divide m (Array.length cs)
         |> Seq.map (fun (s, e) ->
               Async.FromContinuations (fun (cont, _, _) ->
                  start s
                  cs.[s..e] |> Array.map (mandelbrot n) |> cont
                  finish e
               )
            )
         |> Async.Parallel
      return Array.concat results
   }


(*
  Question 4

  Define the function

  display : int -> bool [] -> string

  so that given

  n  : int
  bs : bool []

  'display n bs' represents the Boolean array bs as rectangle of
  characters with true represented by '*' and false represented by
  '.'.

  The parameter n : int is the length of the rows in the string
  representation.

  For example, if
  
  n  = 2 
  bs = [| true; false; false; false; true; true; false |]

  then

  display n bs = "*.\n..\n**\n.."

  (Note the handling of missing values in the example.)
*)
let getSymbolForBoolean (b:bool) : string =
   match b with
   | true  -> "*"
   | false -> "."
let rec generateDotStringOfGivenSize (n:int) : string =
   match n with
   | 0            -> ""
   | x when x > 0 -> "." + generateDotStringOfGivenSize (x-1)
   | _            -> failwith "Can\'t handle negative values"
let display (n:int) (bs:bool []) : string =
   let foldBoolsToString (acc:int*string) (el:bool) : int*string =
      let symbol_to_add = getSymbolForBoolean el
      let i, acc_string = acc
      if i + 1 = n
      then 0, acc_string + symbol_to_add + "\n"
      else i + 1, acc_string + symbol_to_add

   let _, converted_arr = bs
                          |> Array.toList
                          |> List.fold foldBoolsToString (0, "")

   let bs_len_mod_n = bs.Length % n
   if bs_len_mod_n <> 0
   then converted_arr + generateDotStringOfGivenSize (n - bs_len_mod_n)
   else converted_arr.[0..converted_arr.Length-2]
// display 3 [| true; false; true; false; true; false; true; false; true|]
// display 5 [|true; false; false; false; true; true; false|]
// display 2 [|true; false; false; false; true; true; false|]


(*
  You may use the function display to display the Mandelbrot pattern
  that you compute using mandelbrotAsync.

  For example, you can take an array of complex numbers representing a
  rectangular grid from (-3, -3) to (1, 3) with steps of 0.2 and then
  use display with an appropriate row length.
*)


// The next questions are about observables.
//
// You are not allowed to write any mutable code. Solve these using
// the combinators from FSharp.Control.Observable module.

(*
   Question 5

   Define the function

   accumulate : ('t -> 'a -> 't * 'u option) -> 't -> IObservable<'a> -> IObservable<'u>

   so that given

   f   : 't -> 'a -> 't * 'u option
   t   : 't
   obs : IObservable<'a>

   'accumulate f t obs' accumulates observable events of obs into an
   accumulator of type 't and emits an observable event u when
   'snd (f acc a)' evaluates to 'Some u' for an observed event 'a'.
*)
let accumulate (f:'t -> 'a -> 't * 'u option) (t:'t) (obs:System.IObservable<'a>) : System.IObservable<'u> =
   obs
   |> Observable.scan (fun (acc, _) a -> f acc a) (t, None)
   |> Observable.choose snd


(*
   Question 6

   Define the function

   chunks : int -> IObservable<'a> -> IObservable<'a list>

   so that given

   n   : int
   obs : IObservable<'a>

   'chunks n obs' is the observable of chunks (or pieces) of length n
   of the observable obs.

   If n = 3 and obs emits events a_1, a_2, a_3, ... then

   'chunks n obs' emits [a_1; a_2; a_3], [a_4; a_5; a_6], ...
*)
// let chunks (n:int) (obs:System.IObservable<'a>) : System.IObservable<'a list> =
//    obs
//    |> Observable.scan (fun (_, partial_list) el -> if partial_list |> List.length = n
//                                                    then partial_list, [el]
//                                                    else [], partial_list @ [el]) ([], [])
//    |> Observable.map (fun (acc, _) -> acc)
//    |> Observable.filter (List.isEmpty >> not)
let chunks (n:int) (obs:System.IObservable<'a>) : System.IObservable<'a list> =
    let emitN t a =
        match List.length t with
        | m when m = (n - 1) -> ([], Some(t @ [ a ]))
        | _ -> (t @ [ a ], None)

    accumulate emitN [] obs
// let timer = new System.Timers.Timer(2.0)
// timer.Elapsed
//   |> Observable.scan (fun n _ -> n + 1) 0
//   |> chunks 1
//   |> Observable.add (printfn "%A")
// timer.Start()
// System.Threading.Thread.Sleep(350)
// timer.Stop()
// timer.Close()


(*
   Question 7

   Define the function

   sliding : int -> IObservable<'a> -> IObservable<'a list>

   so that given

   n   : int
   obs : IObservable<'a>

   'sliding n obs' is the observable of sliding windows of length n
   over the observable obs.

   If n = 3 and obs emits events a_1, a_2, a_3, ... then

   'sliding n obs' emits [a_1; a_2; a_3], [a_2; a_3; a_4], ...
*)
// let sliding (n:int) (obs:System.IObservable<'a>) : System.IObservable<'a list> =
//    obs
//    |> Observable.scan (fun (out_val, idx, accumulated_vals) el -> if accumulated_vals |> List.length >= n
//                                                                   then accumulated_vals.[idx..idx+n], idx + 1, accumulated_vals @ [el]
//                                                                   else [], idx, accumulated_vals @ [el]) ([], 0, [])
//    |> Observable.map (fun (out, _, _) -> out)
//    |> Observable.filter (List.isEmpty >> not)
let sliding (n:int) (obs:System.IObservable<'a>) : System.IObservable<'a list> =
    let slideEmit t a =
        match List.length t with
        | m when m = (n - 1) -> (t @ [ a ], Some(t @ [ a ]))
        | m when m = n -> ((List.tail t) @ [ a ], Some((List.tail t) @ [ a ]))
        | _ -> (t @ [ a ], None)

    accumulate slideEmit [] obs
// let timer = new System.Timers.Timer(2.0)
// timer.Elapsed
//   |> Observable.scan (fun n _ -> n + 1) 0
//   |> sliding 3
//   |> Observable.add (printfn "%A")
// timer.Start()
// System.Threading.Thread.Sleep(300)
// timer.Stop()
// timer.Close()


(*
   Question 8

   Define the function

   limit : IObservable<unit> -> IObservable<'a> -> IObservable<'a>

   so that given

   clock : IObservable<unit>
   obs   : IObservable<'a>

   'limit clock obs' emits events from obs.

   The observable starts in the emit mode: the next observed event of
   obs can be emitted. After that the observable transitions to
   discard mode: all observed events of obs are discarded until an
   event from clock is observed after which the observable transitions
   back to emit mode and the process repeats.
*)
type LimitState<'a> =
   | Emit of 'a
   | Discard
   | Restart

let limit clock obs =
   let ticks = clock |> Observable.map (fun _ -> Restart)
   let data = obs |> Observable.map (fun x -> (Emit(x)))
   Observable.merge ticks data
      |> Observable.scan (fun p n ->
         match p, n with
         | Restart, Emit(x) -> Emit(x)
         | _, Restart -> Restart
         | _ -> Discard
         ) Restart
      |> Observable.choose (function x -> match x with
                                          | Emit(x) -> Some(x)
                                          | _ -> None)


(*
   Question 9

   Define the function

   alarm : int -> int -> IObservable<unit> -> IObservable<int> -> IObservable<unit>

   so that given

   n         : int
   threshold : int
   clock     : IObservable<unit>
   obs       : IObservable<int>

   'alarm n threshold clock obs' emits an event when the average value
   of last n events of obs has been greater than the threshold.

   Furthermore, after emitting an event, this observable goes into
   silent mode until the next clock event is observed.

   In silent mode, even if there is cause for an alarm, no event is
   emitted.
*)
let alarm (n:int) (threshold:int) (clock:System.IObservable<unit>) (obs:System.IObservable<int>) =
   obs
   |> Observable.scan (fun items next -> let tokeep = min (n-1) (List.length items)
                                         next :: (items |> List.take tokeep)) []
   |> Observable.choose (fun items -> if (List.sum items) > (List.length items) * threshold
                                      then Some()
                                      else None)
    |> limit clock
