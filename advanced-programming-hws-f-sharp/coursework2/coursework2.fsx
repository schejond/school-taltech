(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists and tuples, recursion, combination of functions

  ------------------------------------
  Name: Ondřej Schejbal
  Tallinn University of Technology Student ID
  or Uni-ID: 214308IV
  ------------------------------------

  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060-2021 under your name, into a file coursework2/coursework2.fsx.

  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.

  Deadline for submitting the solution is September 24 AoE, 2021.
*)

// You are given a type BibliographyItem that has the following structure:
// string list * string * (int * int) * int
// The meaning of the tuple elements is as follows:
// * The first field represents the list of author names where each name is in the format
//   "Lastname, Firstname1 Firstname2" (i.e. listing all first names after comma)
// * The second field represents the title of the publication
// * The third field represents a pair containing the starting page number and ending page number of the paper.
// * The fourth field represents the year of publication

type BibliographyItem = string list * string * (int * int) * int

// 1. Create a value bibliographyData : BibliographyItem list that contains
// at least 7 different publications on your favourite topic from https://dblp.uni-trier.de/
// Please note that you need not read the whole papers, just pick 7 papers that look interesting to you from the database.

let bibliographyData =
    [ BibliographyItem(
        [ "Ramakrishna, Bairi"
          "Ambha, A"
          "Ganesh, Ramakrishnan" ],
        "Learning to Generate Diversified Query Interpretations using Biconvex Optimization",
        (733, 739),
        2013
      )
      BibliographyItem(
          [ "Fabbro, Mark, A."
            "Cantoni, Michael" ],
          "Scalable stability analysis for homogeneous networks of dynamical systems interacting via aperiodic sampled-data channels",
          (2863, 2868),
          2018
      )
      BibliographyItem(
          [ "Ohmann, Christian"
            "Eich, Hans-Peter"
            "Sancho, Juan, J."
            "Diaz, Carlos"
            "Faba, G."
            "Oliveri, N."
            "Clamp, Susan"
            "Cavanillas, J., M.;"
            "Coello, E." ],
          "European and Latin-American Countries Associated in a Networked Database of Outstanding Guidelines in Unusual Clinical Cases (ELCANO)",
          (59, 63),
          1999
      )
      BibliographyItem(
          [ "Fabbro, Mark, A."
            "Shames, Iman"
            "Cantoni, Michael" ],
          "Analysis of Model and Iteration Dependencies in Distributed Feasible-Point Algorithms for Optimal Control Computation",
          (186, 193),
          2018
      )
      BibliographyItem(
          [ "Pinciroli, Francesco"
            "Mottadelli, Sara"
            "Vinci, Maurizio"
            "Fabbro, Luigi"
            "Gothager, Klas" ],
          "A Knowledge Management System for New Drug Submission by Pharma-Industries",
          (241, 245),
          2004
      )
      BibliographyItem(
          [ "Dreibholz, Thomas"
            "Zhou, Xing"
            "Fa, Fu" ],
          "Multi-path TCP in Real-World Setups - An Evaluation in the NORNET CORE Testbed",
          (617, 622),
          2015
      )
      BibliographyItem(
          [ "Lucanin, Drazen"; "Fabek, Ivan" ],
          "A visual programming language for drawing and executing flowcharts",
          (1679, 1684),
          2011
      ) ]

// 2. Make a function compareLists : string list -> string list -> int that takes two string lists and
// returns
// * Less than zero in case the first list precedes the second in the sort order;
// * Zero in case the first list and second list occur at the same position in the sort order;
// * Greater than zero in case the first list follows the second list in the sort order;
// You need to use the support honouring the default culture for ordering strings, i.e. the built in
// compare does not work properly by default. Look at the
// documentation of the .Net comparison for strings: System.String.Compare
// If the first authors are the same
// then the precedence should be determined by the next author.
// Please note that your implementation should be recursive over the input lists.
//
// The sort order in .Net is defined using System.Globalization.CultureInfo:
// https://docs.microsoft.com/en-us/dotnet/api/system.globalization
// Please note that your solution should not force a particular sort order!
// notes:
//  build in compare vs System.String.Compare()  <-- USE THE SECOND ONE
//  build in compare is not culture sensitive

let rec compareLists (a: string list) (b: string list) : int =
    match a, b with
    | [], [] -> 0
    | [ x ], [ y ] -> System.String.Compare(x, y)
    | [], [ y ] -> -1
    | [], y :: ys -> -1
    | [ x ], [] -> 1
    | x :: xs, [] -> 1
    | x :: xs, y :: ys when System.String.Compare(x, y) = 0 -> compareLists xs ys
    | x :: xs, y :: ys when System.String.Compare(x, y) <> 0 -> System.String.Compare(x, y)
// compareLists ["Abc"; "AA"; "AA"; "AA"; "xx"] ["Abc"; "AA"; "AA"; "AA"; "xz"]

// System.Threading.Thread.CurrentThread.CurrentCulture <-
// System.Globalization.CultureInfo("en-US")
// System.String.Compare("u","z")
// System.Threading.Thread.CurrentThread.CurrentCulture <-
// System.Globalization.CultureInfo("et-EE")
// System.String.Compare("u","z")

// 3. Make a function
// compareAuthors : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors.
// Use solution from task 2.
let fstItemFromBibliography (a, _, _, _) = a
let sndItemFromBibliography (_, b, _, _) = b
let thirdItemFromBibliography (_, _, c, _) = c
let fourthItemFromBibliography (_, _, _, d) = d

let compareAuthors (a: BibliographyItem) (b: BibliographyItem) : int =
    compareLists (fstItemFromBibliography a) (fstItemFromBibliography b)
// compareAuthors bibliographyData.[5] bibliographyData.[6]


// 4. Make a function
// compareAuthorsNumPages : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors and if the authors are
// the same then according to the number of pages in publication.
let comparePageNumbers (a: BibliographyItem) (b: BibliographyItem) : int =
    let aPageCount =
        snd (thirdItemFromBibliography a)
        - fst (thirdItemFromBibliography a) // + 1 not necessary

    let bPageCount =
        snd (thirdItemFromBibliography b)
        - fst (thirdItemFromBibliography b) // + 1 not necessary

    aPageCount.CompareTo(bPageCount)
// comparePageNumbers bibliographyData.[5] bibliographyData.[0] // .[5] < .[0] => -1

let compareYears (a: BibliographyItem) (b: BibliographyItem) : int =
    (fourthItemFromBibliography a)
        .CompareTo(fourthItemFromBibliography b)


let compareAuthorsNumPages (a: BibliographyItem) (b: BibliographyItem) : int =
    let authorComparisonResult = compareAuthors a b

    if authorComparisonResult = 0 then
        comparePageNumbers a b
    else
        authorComparisonResult

// compareAuthorsNumPages bibliographyData.[5] bibliographyData.[6]


// 5. Make a function
// sortBibliographyByNumPages : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the number of pages in the
// publication in ascending order.
// If two items are at the same level in the sort order, their order should be preserved.
let rec sortBibliographyByNumPages (arr: BibliographyItem list) : BibliographyItem list =
    match arr with
    | [] -> []
    | x :: xs ->
        let lessEl =
            xs
            |> List.filter (fun item -> (comparePageNumbers item x) <= 0)
            |> sortBibliographyByNumPages

        let biggerEl =
            xs
            |> List.filter (fun item -> (comparePageNumbers item x) > 0)
            |> sortBibliographyByNumPages

        List.concat [ lessEl; [ x ]; biggerEl ]
// sortBibliographyByNumPages bibliographyData

// 6. Make a function
// sortBibliographyByAuthorNumPages : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the authors and number of pages in the publication in ascending order
// If two items are at the same level in the sort order, their order should be preserved.
let rec sortBibliographyByAuthorNumPages (arr: BibliographyItem list) : BibliographyItem list =
    match arr with
    | [] -> []
    | x :: xs ->
        let prevEl =
            xs
            |> List.filter (fun item -> (compareAuthors item x) < 0)
            |> sortBibliographyByAuthorNumPages

        let eq =
            arr
            |> List.filter (fun item -> (compareAuthors item x) = 0)
            |> sortBibliographyByNumPages

        let postEl =
            xs
            |> List.filter (fun item -> (compareAuthors item x) > 0)
            |> sortBibliographyByAuthorNumPages

        List.concat [ prevEl; eq; postEl ]

// 7. Make a function
// groupByAuthor : BibliographyItem list -> (string * BibliographyItem list) list
// where the return list contains pairs where the first element is the name of a single
// author and the second element a list of bibliography items that the author has co-authored.
let rec expandAuthorBookDictionaryWithBookForGivenAuthor
    (dic: (string * BibliographyItem list) list)
    (author: string)
    (book: BibliographyItem)
    : (string * BibliographyItem list) list =
    match dic with
    | [] -> [ (author, [ book ]) ]
    | x :: xs ->
        if fst (x) = author then
            if List.contains book (snd (x)) then
                dic
            else
                List.concat [ [ (author, List.concat [ snd (x); [ book ] ]) ]
                              xs ]
        else
            List.concat [ [ x ]
                          expandAuthorBookDictionaryWithBookForGivenAuthor xs author book ]

let rec expandAuthorBookDictionaryByAllAuthorsOfBook
    (dic: (string * BibliographyItem list) list)
    (authors: string list)
    (book: BibliographyItem)
    : (string * BibliographyItem list) list =
    match authors with
    | [] -> dic
    | x :: xs ->
        expandAuthorBookDictionaryByAllAuthorsOfBook
            (expandAuthorBookDictionaryWithBookForGivenAuthor dic x book)
            xs
            book

let rec groupByAuthorByBibItems
    (arr: BibliographyItem list)
    (dic: (string * BibliographyItem list) list)
    : (string * BibliographyItem list) list =
    match arr with
    | [] -> dic
    | x :: xs ->
        groupByAuthorByBibItems xs (expandAuthorBookDictionaryByAllAuthorsOfBook dic (fstItemFromBibliography x) x)

let groupByAuthor (arr: BibliographyItem list) : (string * BibliographyItem list) list = groupByAuthorByBibItems arr []

// let b = bibliographyData.[0]
// groupByAuthor [b;b;b]
// groupByAuthor bibliographyData
