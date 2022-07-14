(*
  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------
  Coursework 0: Getting started
  ------------------------------------
  Name: Ondřej Schejbal
  Student ID: 214308IV
  ------------------------------------
  Answer the questions below. You answers to questions 2--8 should be
  correct F# code written after the question. The F# code for question
  1 is written for you and serves as an example. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.
  This coursework will NOT be graded but we encourage you to do it,
  you will not succeed in this course if you don't practice, and
  there's no time like the present! Also, you may find that parts of
  it appear in later courseworks. *)

// 0. Find your way to the fsharp interactive (fsi) command prompt.
// I.e. log in to a lab machine and start Visual Studio, install
// VSCode/Ionide and .net 5.0 on your laptop, etc.


// 1. Load the following function into fsi
let greeting name = printfn "Hello: %s" name

// 2. Run the function greeting and say hello to yourself.
greeting "Ondřej"

// 3. Create a value myName : string that contains your name.
let myName = "Ondřej"

// 4.Define
// splitAtChar : text:string -> sep:char -> list<string>
// is equivalent to
// splitAtChar : text:string -> sep:char -> string list

let splitByChar (text:string) = Seq.toList text
// splitByChar myName

let splitAtChar (text:string) (sep:char) =
  Array.toList(
    text.Split sep
  )

// let mutable k = [1]
// k <- List.append k [2]
// k

// 5. Write a function splitAtSpaces in such a way that it uses splitAtChar
let splitAtSpaces (text:string) =
  splitAtChar text ' '
splitAtSpaces "Hi Ondro, how are you"

let splitAtSpaces2 (text:string) = 
  let mutable words = []
  let mutable word = ""
  for char in splitByChar text do
    if char = ' '
    then
      words <- List.append words [word]
      word <- ""
    else
      word <- word + string char
  if word.Length > 0
  then words <- List.append words [word]
  words

// 6. Define sentenceCount : text:string -> int
let getSentenceList (text:string) = Array.toList (
  if text.[text.Length - 1] = '.'
    then text.Remove(text.Length - 1).Split '.'
  else text.Split '.'
)

let sentenceCount (text:string) =
  let sentences = getSentenceList text
  sentences.Length
// sentenceCount "Ondra has an apple. Pavel doesn't. Third sentence."

// 7. Define stats : text:string -> unit
// which prints the same stats as showWordCount and
// the number of sentences and average length of sentences
// hint: try float: int -> float
let wordCount (text:string) =
  let words    = splitAtSpaces text
  let wordSet  = Set.ofList words
  let numWords = words.Length
  let numDups  = words.Length - wordSet.Count
  numWords, numDups

let showWordCount (text:string) =
  let numWords, numDups = wordCount text
  printfn "--> %d words in text" numWords
  printfn "--> %d duplicate words" numDups

let rec countCharOccurences (text : string)(c : char) : int = 
  if text.Length = 0
    then 0
  else
    if text.[text.Length - 1] = c
      then countCharOccurences (text.Remove(text.Length - 1)) (c) + 1
    else countCharOccurences (text.Remove(text.Length - 1)) (c)

let sentenceAvgLength (text:string) : float =
  let sentences = getSentenceList text
  let numWords, numDups = wordCount text
  let totalLength = numWords// - countCharOccurences text '.' -> puvodne jsem totiz pocital, ze delka vety == pocet znaku
  (float totalLength) / (float sentences.Length)

let showSentencesStats (text:string) =
  let sentences = sentenceCount text
  let sentenceLenAvg = sentenceAvgLength text
  printfn "--> %d sentences in text" sentences
  printfn "--> %f avg sentence length" sentenceLenAvg

let stats (text:string) =
  showWordCount text
  showSentencesStats text

// stats "Ondra has an apple. Pavel doesn't. Third sentence."

// 8. Use the 'http' function from the lecture to download the file
// http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt as a string
// NOTE: you cannot use this function in tryfsharp. Instead you can
// paste the text into your file as a string and process it locally
open System.IO
open System.Net
let http (url: string) = 
    let req = System.Net.WebRequest.Create url
    let resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()
    resp.Close()
    html

let webInfo = http "http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt"

// 9. run stats on the downloaded file
stats webInfo