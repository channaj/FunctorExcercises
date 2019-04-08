open System

let notImplemented () = raise <| NotImplementedException ()

// Implement map for option
let mapOption : ('a -> 'b) -> 'a option -> 'b option =
  fun fn opt ->
    match opt with
    | Some x -> Some (fn x)
    | None -> None


// Implement map for list
// let mapList : ('a -> 'b) -> 'a list -> 'b list =
//   fun fn lst -> 
//     seq {
//       for i in lst do
//         yield  (fn i)
//     } 
//     |> Seq.toList

let rec mapList : ('a -> 'b) -> 'a list -> 'b list =
     fun fn lst -> 
       //notImplemented ()
        match lst with
        | a::b -> fn a :: mapList fn b 
        | [] -> []
    
let rec mapList2 : ('a -> 'b) -> 'a list -> 'b list =
     fun fn lst -> 
       //notImplemented ()
        match lst with
        | x::b -> (fn x) :: mapList2 fn b 
        | [] -> []
        //| [] -> [fn a]
    

// Implement map for Result
let mapResult : ('a -> 'b) -> Result<'a, 'c> -> Result<'b, 'c> =
  fun fn res ->
    match res with
    | Ok a -> fn a |> Ok
    | Error e -> Error e


// Implement map for Result's error
let mapResultError : ('a -> 'b) -> Result<'c, 'a> -> Result<'c, 'b> =
  fun fn res ->
    match res with
    | Ok a -> Ok a
    | Error e -> fn e |> Error


// Implement bimap for Result. You should be able to implement it using functions you've defined previously
let bimapResult : ('a -> 'b) -> ('c -> 'd) -> Result<'a, 'c> -> Result<'b, 'd> =
  fun first second res ->
    match res with
    | Ok a -> first a |> Ok
    | Error e -> second e |> Error


// Implement bimap for Result. You should be able to implement it using functions you've defined previously
let bimapResult2 : ('a -> 'b) -> ('c -> 'd) -> Result<'a, 'c> -> Result<'b, 'd> =
  fun first second res ->
    match res with
    | Ok a -> mapResult first (Ok a)
    | Error e -> mapResultError second (Error e)

// Implement bimap for Result. You should be able to implement it using functions you've defined previously
let bimapResult3 : ('a -> 'b) -> ('c -> 'd) -> Result<'a, 'c> -> Result<'b, 'd> =
  fun first second res ->
    mapResult first res |> mapResultError second

 // Implement map for choice 3
let mapChoice3 : ('a -> 'b) -> Choice<'a, 'c, 'd> -> Choice<'b, 'c, 'd> =
  fun fn lst ->
    match lst with
    | Choice1Of3 a -> fn a |> Choice1Of3
    | Choice2Of3 b -> Choice2Of3 b
    | Choice3Of3 c -> Choice3Of3 c

// Implement map for Function
type Function<'Input, 'Output> = Function of ('Input -> 'Output)

let _mapFunction : ('a -> 'b) -> ('x -> 'a) -> ('x -> 'b) =
  fun fn f ->
      f >> fn 

let mapFunction : ('a -> 'b) -> Function<'x, 'a> -> Function<'x, 'b> =
  fun fn f ->
      match f with | Function ff -> Function (ff >> fn)


let __mapFunction : ('a -> 'b) -> Function<'x, 'a> -> Function<'x, 'b> =
  fun fn (Function f) ->
       Function (f >> fn)


// Implement map for Async using computation expressions
let mapAsync : ('a -> 'b) -> 'a Async -> 'b Async =
  fun fn a -> async {
     let! va = a
     return fn va
  }

// Without using the compiler, what is the type of somethingElse?
let stringLength : string -> int = String.length
let something = None
let somethingElse = mapOption stringLength something


// What is another way of writing this function using less maps?
let lists =
  [ 1; 2; 3; 4 ]
  |> mapList ((+) 1)
  |> mapList (fun i -> i.ToString())

let listsLessMaps =
  [ 1; 2; 3; 4 ]
  |> mapList (fun i -> (i + 1).ToString())

// Is it possible to implement mapper below such that the length of outputList changes? No
let mapper x = notImplemented ()
let outputList input =
  mapList mapper input


// What would be the value of endingResult below. Don't run it and find out, do it in your head.
// Ok "Channa"
let startingResult : Result<string, unit> = Ok "Channa"
let id : 'a -> 'a = fun x -> x
let endingResult = mapResult id startingResult


// Is this function Functor's map function? Explain why yes or no.
let mapOptionInts : (int -> int) -> int option -> int option =
  fun fn opt -> notImplemented () // You don't need to see the implementation


// Given the following types and functions, write an implementation for lengthOfContent
type MyAsync<'a> = YouDontNeedToKnow
type HttpResult =
  { Verb : string
    Uri : Uri
    Headers : Map<string, string list>
    Content : string }

let mapMyAsync : ('a -> 'b) -> MyAsync<'a> -> MyAsync<'b> = fun fn x -> notImplemented ()
let httpGet : Uri -> MyAsync<HttpResult> = notImplemented ()
let stringLength' : string -> int = String.length

let lengthOfContent : Uri -> MyAsync<int> =
  fun uri ->
    notImplemented ()


// How could you refactor refactorMe to use maps?
let readFile : string -> Async<byte[]> = notImplemented ()
let writeFile : string -> string -> Async<unit> = notImplemented ()

let refactorMe = async {
  let! bytes = readFile @"C:\Temp\Nice file.txt"
  let decodedFile = System.Text.Encoding.UTF8.GetString bytes
  let wordsFromFile = decodedFile.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)

  let! bytes = readFile @"C:\Temp\Another nice file.txt"
  let decodedFile2 = System.Text.Encoding.UTF8.GetString bytes
  let wordsFromFile2 = decodedFile2.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)

  let uniqueWords =
    Seq.append wordsFromFile wordsFromFile2
    |> Set.ofSeq
  do!
    String.Join (Environment.NewLine, uniqueWords)
    |> writeFile (@"C:\Temp\All unique words.txt")

  return Set.count uniqueWords
}