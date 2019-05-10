#load "./FunctorExercises.fsx"

open System

let notImplemented () = raise <| NotImplementedException ()

// https://gist.github.com/daniel-chambers/a4a55e75576e0610238f812f99df593c

// Try to do each of these without using the compiler, then check your result
// after you've written it. This means don't use VSCode with Ionide either ;)

// Implement pure for option
let pureOption : 'a -> 'a option =
  Some

// Implement apply for option
let applyOption : ('a -> 'b) option -> 'a option -> 'b option =
  fun fn x ->
    match fn, x with
    | Some fn', Some x' -> Some (fn' x')
    | None,     Some _  -> None
    | Some _, None      -> None
    | None, None        -> None

// Without using the compiler, what is the type of pureStringLength?
// (string -> int) option
let stringLength : string -> int = String.length
let pureStringLength = pureOption stringLength


// Without using the compiler, what is the type of pureLabelNumber?
// (string -> int -> string) option
let labelNumber : string -> int -> string =
  fun label num -> sprintf "%s: %i" label num
let pureLabelNumber = pureOption labelNumber


// Without using the compiler, what is the type of oneApply?
// (int -> string) option
let myLabel : string option = Some "The Meaning of Life, the Universe, and Everything"
let oneApply = applyOption pureLabelNumber myLabel


// Without using the compiler, what is the type of twoApplys? What is its value?
// string option
// Some "The Meaning of Life, the Universe, and Everything: 42"
let myNum : int option = Some 42
let twoApplys = applyOption oneApply myNum


// Use pureOption and applyOption to use mkFullName with maybeFirstName
// and maybeSurname to get maybeFullName
let maybeFirstName : string option = Some "Jim"
let maybeSurname : string option = Some "Pelletier"
let mkFullName firstName surname = sprintf "%s %s" firstName surname
let maybeFullName : string option =
    applyOption (applyOption (pureOption mkFullName) maybeFirstName) maybeSurname

// That was probably messy. If we implemented applyOption as an operator
// can you clean that up?
let maybeFullName' : string option =
  let inline (<*>) fn x = applyOption fn x
  pureOption mkFullName <*> maybeFirstName <*> maybeSurname

// Copy in your implementation of mapOption from FunctorExercises
let mapOption : ('a -> 'b) -> 'a option -> 'b option =
  fun fn opt ->
    match opt with
    | Some a -> Some (fn a)
    | None -> None


// Without using the compiler, what is the type of oneMap?
// (int -> string) option
let labelNumber' : string -> int -> string =
  fun label num -> sprintf "%s: %i" label num
let myLabel' : string option = Some "The Meaning of Life, the Universe, and Everything"
let oneMap = mapOption labelNumber' myLabel'


// Without using the compiler, what is the type of oneMapOneApply? What is its value?
// string option
// Some "The Meaning of Life, the Universe, and Everything: 42"
let myNum' : int option = Some 42
let oneMapOneApply = applyOption oneMap myNum'


// If we define mapOption as an operator (<!>), can you clean up maybeFullName' even further?
// Hint: you can implement this wholly in terms of map and apply
let maybeFullName'' : string option =
  let inline (<!>) fn x = mapOption fn x
  let inline (<*>) fn x = applyOption fn x
  mkFullName <!> maybeFirstName <*> maybeSurname


// To prove every Applicative is a Functor, implement Functor's map for option
// (mapOptionViaApplicative) using only pure and apply
// How does it prove every Applicative is a Functor

// As per applicative laws, every Applicative must implement pure and apply. If we can implement map using pure and apply that means every applicative is a functor too.

let mapOptionViaApplicative : ('a -> 'b) -> 'a option -> 'b option =
  fun fn opt ->
     let inline (<*>) fn x = applyOption fn x
     pureOption fn <*> opt


// Implement the not implemented functions below then refactor mkAddress to use
// functor and applicative functions for option.
// Hint: You'll want to use the <!> and <*> operators to make it readable

type StreetNumber = StreetNumber of int
type Street = Street of string
type Suburb = Suburb of string
type Postcode = Postcode of string

type Address =
  { StreetNumber : StreetNumber
    Street : Street
    Suburb : Suburb
    Postcode : Postcode }

let stringToInt : string -> int option =
  fun str ->
    match Int32.TryParse str with
    | (true, result) -> Some result
    |  _             -> None
    
let emptyStringToOption : string -> string option =
  fun str ->
    if String.IsNullOrWhiteSpace str then None else Some str

let mkStreetNumber str = stringToInt str |> mapOption StreetNumber
let mkStreet str = emptyStringToOption str |> mapOption Street
let mkSuburb str = emptyStringToOption str |> mapOption Suburb
let mkPostcode str = emptyStringToOption str |> mapOption Postcode

let mkAddress' : StreetNumber -> Street -> Suburb -> Postcode -> Address =
  fun streetNo street suburb postcode -> {
   StreetNumber = streetNo
   Street = street
   Suburb = suburb
   Postcode = postcode
  }
  
let mkAddress : string -> string -> string -> string -> Address option =
  fun streetNo street suburb postcode ->
   let inline (<!>) fn x = mapOption fn x
   let inline (<*>) fn x = applyOption fn x
   mkAddress' <!> mkStreetNumber streetNo <*> mkStreet street <*> mkSuburb suburb <*> mkPostcode postcode

// Copy in your implementation of Functor map for Result from Functor Exercises
let mapResult : ('a -> 'b) -> Result<'a, 'c> -> Result<'b, 'c> =
  fun fn res ->
    match res with
    | Ok a    -> Ok <| fn a 
    | Error e -> Error e

// Implement pure for Result
let pureResult : 'a -> Result<'a, 'e> = Ok

// Implement apply for Result
let applyResult : Result<('a -> 'b), 'e> -> Result<'a, 'e> -> Result<'b, 'e> =
  fun fn x ->
    match fn, x with
    | Ok fn' , Ok x'  -> pureResult <| fn' x'
    | Ok _ , Error e  -> Error e
    | Error e, _      -> Error e

//let bindResult : ('a -> Result<'b, 'e>) -> Result<'a, 'e> -> Result<'b, 'e> = 
//  fun fn x ->
//   match x with
//     | Ok x' -> fn x'
//     | Error e -> Error e

// let applyRsult' : Result<('a -> 'b), 'e> -> Result<'a, 'e> -> Result<'b, 'e> = 
//   fun fn x ->
//     notImplemented () // Could be better than above

// Implement calculateCommissionAmount, where commission for an introducer is
// calculated as a percentage of the loan amount, with a minimum payable
// commission of $1000 regardless of loan amount
type IntroducerId = int
type LoanId = int

let calculateCommissionAmount : decimal -> decimal -> decimal =
  fun commissionPercentage loanAmount ->
    let calculatedCommssionAmount = loanAmount * commissionPercentage
    let minimumCommissionAmount = 1000m
    if calculatedCommssionAmount < minimumCommissionAmount then minimumCommissionAmount else calculatedCommssionAmount


// Use the (fake) database query functions below to get the data you need
// to perform the above commission calculation and return the amount.
// Use the functor and applicative functions for Result to achieve this
type SqlError =
  | QueryTimeout
  | OtherError of exn
  
let getCommissionPercentageForIntroducerFromDb : IntroducerId -> Result<decimal, SqlError> =
  fun id -> Ok 2.5m
  
let getLoanAmountFromDb : LoanId -> Result<decimal, SqlError> =
  fun id -> Ok 500000m

let getCommissionAmount : IntroducerId -> LoanId -> Result<decimal, SqlError> =
  fun introducerId loanId ->
    let inline (<!>) fn x = mapResult fn x
    let inline (<*>) fn x = applyResult fn x
    calculateCommissionAmount 
    <!> getCommissionPercentageForIntroducerFromDb introducerId 
    <*> getLoanAmountFromDb loanId

type Validation<'a, 'e when 'e : comparison>  =
  | Success of 'a
  | Failure of 'e Set

// Implement Functor map for the Validation type above
let mapValidation : ('a -> 'b) -> Validation<'a, 'e> -> Validation<'b, 'e> =
  fun fn x ->
    match x with
    | Success x' -> Success <| fn x'
    | Failure e  -> Failure e

// Implement pure for the Validation type above
let pureValidation : 'a -> Validation<'a, 'e> = Success

// Implement apply for the Validation type above. The difference between
// Validation and Result is that Validation should accumulate errors in
// the Failure Set, whereas Result simply uses the first error and discards
// the rest
let applyValidation : Validation<('a -> 'b), 'e> -> Validation<'a, 'e> -> Validation<'b, 'e> =
  fun fn x ->
   match fn, x with
   | Success fn', Success x' -> Success <| fn' x'
   | Success fn', Failure xe -> Failure xe
   | Failure fne, Success _ -> Failure fne
   | Failure fne, Failure xe -> Failure <| Set.union fne xe

// Implement the following validation functions
type ValidationError =
  | Required of name : string
  | MustBeAnInteger of name : string
  | InvalidPostcode
  | MustBeABoolean of name : string
  with 
    override this.ToString () =
      match this with
      | Required name -> sprintf "%s is required" name
      | MustBeAnInteger name -> sprintf "%s must be an integer" name
      | InvalidPostcode -> "PostCode is invalid"
      | MustBeABoolean name -> sprintf "%s must be a boolean" name

let validateInt : string -> string -> Validation<int, ValidationError> =
  fun name str ->
    match Int32.TryParse str with
    | (true, i) -> Success i
    | (false,_) -> Failure <| Set.singleton (MustBeAnInteger name)

let validateStringRequired : string -> string -> Validation<string, ValidationError> =
  fun name str ->
    if String.IsNullOrWhiteSpace str then Failure <| Set.singleton (Required name) else Success str

let validatePostcode : string -> Validation<string, ValidationError> =
  fun str ->
   validateStringRequired "Postcode" str

let mkStreetNumber' str = validateInt "StreetNumber" str |> mapValidation StreetNumber
let mkStreet' str = validateStringRequired "Street" str |> mapValidation Street
let mkSuburb' str = validateStringRequired "Suburb" str |> mapValidation Suburb
let mkPostcode' str = validatePostcode str |> mapValidation Postcode

// Implement validateAddress using functor and applicative functions for Validation
let validateAddress : string -> string -> string -> string -> Validation<Address, ValidationError> =
  fun streetNo street suburb postcode ->
   let inline (<!>) fn x = mapValidation fn x
   let inline (<*>) fn x = applyValidation fn x
   mkAddress' <!> mkStreetNumber' streetNo <*> mkStreet' street <*> mkSuburb' suburb <*> mkPostcode' postcode

// Implement validateBool, then implement validateResidence using functor and applicative functions
// Hint: you should be able to compose with your previous validateAddress function
type Residence =
  { Address : Address
    YearsOccupied : int
    IsPrimary : bool }

let validateBool : string -> string -> Validation<bool, ValidationError> =
  fun name str ->
    match Boolean.TryParse str with
    | (true, b) -> Success b
    | _         -> Failure <| Set.singleton (MustBeABoolean name)

let mkYearsOccupied str = validateInt "YearsOccupied" str
let mkIsPrimary str = validateBool "IsPrimary" str

let mkResidence : Address -> int -> bool -> Residence =
  fun address yearsOccupied isPrimary -> {
   Address = address
   YearsOccupied = yearsOccupied
   IsPrimary = isPrimary
  }
  
let validateResidence : string -> string -> string -> string -> string -> string -> Validation<Residence, ValidationError> =
  fun streetNo street suburb postcode yearsOccupied isPrimary ->
    let inline (<!>) fn x = mapValidation fn x
    let inline (<*>) fn x = applyValidation fn x
    mkResidence <!> validateAddress streetNo street suburb postcode <*> mkYearsOccupied yearsOccupied <*> mkIsPrimary isPrimary 
    
// TODO: Async, List, ZipList -------------------------------------------------------------------------------------------------

// Which built-in function implements applicative's pure? Implement pureAsync by using it
let pureAsync : 'a -> Async<'a> =
  fun x -> async { return x }

// Implement apply for Async (use a F# async computation expression)
let applyAsync : Async<'a -> 'b> -> Async<'a> -> Async<'b> =
  fun fn x -> async {
    let! fn' = fn
    let! x' = x
    return fn' x'
  }

// Copy in your implementation of mapAsync from Functor Exercises
let mapAsync : ('a -> 'b) -> Async<'a> -> Async<'b> =
  fun fn x -> async {
    let! a = x
    return (fn a)
  }

//let aaa = pureAsync >> applyAsync

let bindAsync : ('a -> Async<'b>) -> Async<'a> -> Async<'b> =
  fun fn x -> async {
   let! a = x
   return! fn a
  }

// let rec mapList : ('a -> 'b) -> 'a list -> 'b list =
//      fun fn lst -> 
//         match lst with
//         | a::b -> fn a :: mapList fn b 
//         | [] -> []
        
let rec bindList : ('a -> 'b list) -> 'a list -> 'b list =
  fun fn x -> 
    let rec bindList' xs (ys : 'b list) =
      match xs with
      | x' :: xs' -> bindList' xs' (ys @ fn x')
      | [] -> ys
    bindList' x []

// In Functor Exercises you refactored 'refactorMe' to use mapAsync
// Using your refactored solution from last time, can you continue refactoring it
// to use applicative functions as well as functor functions?
let readFile : string -> Async<byte[]> = fun x -> notImplemented ()
let writeFile : string -> string -> Async<unit> = fun x -> notImplemented ()

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

let getWordsFromFile : string -> Async<string[]> =
  fun filePath -> 
   readFile filePath 
   |> mapAsync (fun bytes ->
                  let words = System.Text.Encoding.UTF8.GetString bytes
                  words.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries))

let refactoredMe = async {
  let (<!>) = mapAsync
  let (<*>) = applyAsync

  let! uniqueWords =
    Seq.append 
    <!> getWordsFromFile @"C:\Temp\Nice file.txt"
    <*> getWordsFromFile @"C:\Temp\Another nice file.txt"

  do!
    String.Join (Environment.NewLine, uniqueWords)
    |> writeFile (@"C:\Temp\All unique words.txt")

  return  uniqueWords |> Set.ofSeq |> Set.count
 }

// Implement getHardcodedUris using functor & applicative functions for async
// Note that the order of HttpResponses returned should match the order
// of the uris
type HttpResponse = Unimportant
let httpGet : Uri -> Async<HttpResponse> = 
  fun uri -> pureAsync Unimportant


// let applyTest uris =
//   let (<!>) = mapAsync
//   let (<*>) = applyAsync
//   let ([ x; y; z ]) = uris
  
//   let blah : HttpResponse -> HttpResponse -> HttpResponse -> HttpResponse list = 
//     fun a b c -> [a;b;c]
   
//   let aa = blah <!> httpGet x
//   pureAsync blah <*> httpGet x <*> httpGet y <*> httpGet z




// TODO : Make tail recursive 
let getHardcodedUris : Async<HttpResponse list> = 
  let uris = [ Uri "https://www.google.com"; Uri "https://fsharp.org"; Uri "https://portal.azure.com" ]
  let (<!>) = mapAsync
  let (<*>) = applyAsync

 // let ([ x; y; z ]) = uris
  // (fun a b c -> [a;b;c]) <!> httpGet x <*> httpGet y <*> httpGet z

  let rec getUris u = 
      match u with
      | x::t -> List.append 
                <!> (List.singleton <!> httpGet x)
                <*> getUris t
      | [] -> async { return List.empty }
  getUris uris
            
let cons : 'a -> 'a list -> 'a list =
  fun a al -> a :: al
// Does your solution to getHardcodedUris generalise to an arbitrary list of Uris?
// Demonstrate this by implementing getAllUris. You should only require functor and applicative
// functions for async to do this
let getUris : Uri list -> Async<HttpResponse list> =
  fun uris ->
    let (<!>) = mapAsync
    let (<*>) = applyAsync
    
    let rec getUris' (fn: 'a -> Async<'b>) (xs: 'a list) (ys: Async<'b list>) = 
      match xs with
      | x':: xs' -> 
        getUris' fn xs' (cons <!> fn x' <*> ys)
      | [] -> ys
    getUris' httpGet uris (pureAsync [])



// Can you generalise your solution further, such that it doesn't care about Uris and HttpResponses?
// FYI: This particular function is called 'traverse'
let traverseAsync : ('a -> Async<'b>) -> 'a list -> Async<'b list> =
  fun fn xs ->
    let (<!>) = mapAsync
    let (<*>) = applyAsync
    
    let rec traverseAsync' (xs: 'a list) (ys: Async<'b list>) = 
      match xs with
      | x'::xs' -> 
        traverseAsync' xs' (cons <!> fn x' <*> ys)
      | [] -> ys
    traverseAsync' xs (pureAsync [])

// Implement traverse for Option
let traverseOption : ('a -> 'b option) -> 'a list -> 'b list option =
  fun fn xs ->
    let (<!>) = mapOption
    let (<*>) = applyOption
    
    let rec traverseOption' (xs: 'a list) (ys:'b list option) = 
      match xs with
      | x'::xs' -> 
        traverseOption' xs' (cons <!> fn x' <*> ys)
      | [] -> ys
    traverseOption' xs (Some [])


// TODO :
// Describe the similarities between traverseAsync and traverseOption. What is
// different between both implementations and what is the same?

// They both take a world-crossing-function of ('a -> strucure<'b>) and a list of type 'a as input and out put structure<'b list>
// 


// Sequence is a variation upon traverse. Implement this for async by reusing your traverseAsync function
let sequenceAsync : Async<'a> list -> Async<'a list> =
  fun listOfAsyncs ->
    traverseAsync id listOfAsyncs


// Implement pure for list
let pureList : 'a -> 'a list =
  fun x -> [x]


// Copy in your implementation of functor's map for list from Functor Exercises
let rec mapList : ('a -> 'b) -> 'a list -> 'b list =
     fun fn lst -> 
        let rec rmap lst' acc =
             match lst' with
             | x::xs -> rmap xs (fn x :: acc)
             | [] -> acc
        List.rev <| rmap lst []


// Implement apply for list
// HINT: The usual implementation of applicative for list creates a cross-product of the
// applied lists. For example (and note the ordering out the output!):
// (fun a b -> (a, b)) <!> [1;2] <*> [3;4] = [(1,3);(1,4);(2,3);(2,4)]
let applyList : ('a -> 'b) list -> 'a list -> 'b list =
  fun fns xs ->
    let rec applyList' fns' acc =
      match fns' with
      | fn'' :: fns'' -> applyList' fns'' (acc @ mapList fn'' xs) 
      | [] -> acc
    applyList' fns []

// Using functor and applicative for list, generate a list of all possible loan
// interest rate dimensions (implement 'loanInterestRateDimensions')
type RiskGrade = AAA | AA | A | BPlus | B | BMinus
type Product = Sharp | Star | Free
type LvrRange =
  { From : int
    To : int }

let validLvrRanges : LvrRange list =
  [ { From = 0; To = 60 }
    { From = 60; To = 80 }
    { From = 80; To = 90 }
    { From = 90; To = 95 } ]

let getDimension : Product -> RiskGrade -> LvrRange -> (Product * RiskGrade * LvrRange) = 
  fun p r l -> (p, r, l)

let loanInterestRateDimensions : (Product * RiskGrade * LvrRange) list =
  let riskGrades = [AAA; AA; A; BPlus; B; BMinus]
  let products = [Sharp; Star; Free]
  let (<*>) = applyList
  let (<!>) = mapList
  getDimension <!> products <*> riskGrades <*> validLvrRanges

// Because F# is a strict-evaluation language, lists cannot be infinite. However,
// .NET and therefore F# has the IEnumerable<T> (or 'a seq in F#) interface to
// represent lazy and potentially infinite sequences. Unfortunately, writing
// implementations of that interface manually is painful and mutable (especially lazy
// implementations). Luckily, F# has "Sequence Expressions" to help write these and
// get the compiler to generate the necessary lazy and mutable machinery for you.
// Please read the "Sequence Expressions" section of the F# documentation and come back:
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/sequences#sequence-expressions

// Using what you've learned about sequence expressions (seq {} and yield),
// implement functor's map for seq
let mapSeq : ('a -> 'b) -> 'a seq -> 'b seq =
  fun fn xs ->
      seq { for a in xs -> fn a }


// Implement pure for seq
let pureSeq : 'a -> 'a seq =
  fun x ->
    seq { yield x }


// Implement apply for sequence using the cross-product implementation of applicative
let applySeq : ('a -> 'b) seq -> 'a seq -> 'b seq =
  fun fns xs ->
    seq { for x in xs do
            for fn in fns -> fn x }

// Reimplement 'loanInterestRateDimensions' but this time as a sequence to make sure
// your implementation produces consistent results with your list applicative
// implementation from above
let loanInterestRateDimensionsAsSeq : (Product * RiskGrade * LvrRange) seq =
  let (<*>) = applySeq
  let (<!>) = mapSeq
  let riskGrades = [AAA; AA; A; BPlus; B; BMinus]
  let products = [Sharp; Star; Free]
  getDimension <!> products <*> riskGrades <*> validLvrRanges

// "Zipping" two lists together can be visualised by thinking of how a zipper
// zips the two sides of the zip together into one strip.
// Example: zipToTupleList [1;2;3] [4;5] = [(1,4);(2,5)]
// Implement zipToTupleList yourself (don't use the built-in List.zip function)
let zipToTupleList : 'a list -> 'b list -> ('a * 'b) list =
  fun xs ys ->
    let rec step xs' ys' acc =
      match xs', ys' with
      | (x'' :: xs'', y'' :: ys'') -> step xs'' ys'' ((x'', y'') :: acc)
      | [], _ -> acc
      | _ , [] -> acc
    step xs ys []


// Can you generalise your zipToTupleList function such that it can produce
// any structure, not just tuples?
let zipLists : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list =
  fun fn xs ys ->
    let rec step xs' ys' acc =
      match xs', ys' with
      | (x'' :: xs'', y'' :: ys'') -> step xs'' ys'' (fn x'' y'' :: acc)
      | [], _ -> acc
      | _ , [] -> acc
    step xs ys []


// Re-implement zipToTupleList in terms of the generalised zipLists function
let zipToTupleList' : 'a list -> 'b list -> ('a * 'b) list =
  fun as' bs' ->
    zipLists (fun a b -> a, b) as' bs'


// Implement zipSequences without cheating and converting any of the sequences to lists.
// HINT: You may use Seq.take in order to only enumerate some of a sequence
// without resorting to mutable variables and while loops
let zipSequences'' : ('a -> 'b -> 'c) -> 'a seq -> 'b seq -> 'c seq =
  fun fn xs ys ->
    let (<*>) = applySeq
    let (<!>) = mapSeq
    
    let take seq index = Seq.take 1 (Seq.rev (Seq.take index seq))
    let rec step i acc =
      if Seq.length xs > i && Seq.length ys > i then
        let c = fn <!> take xs (i+1) <*> take ys (i+1)
        step (i+1) (Seq.append c acc)
      else
        Seq.rev acc
    step 0 []


let zipSequences' : ('a -> 'b -> 'c) -> 'a seq -> 'b seq -> 'c seq =
  fun fn xs ys ->
    let rec step i acc =
      if Seq.length xs > i && Seq.length ys > i then
        let c = Seq.singleton <| fn (Seq.item i xs) (Seq.item i ys) 
        step (i+1) (Seq.append c acc)
      else
        Seq.rev acc
    step 0 []

// One of the powers of sequences is that they can be infinite since they can
// be lazily generated. Implement zipSequences so that it works with infinite
// sequences. This means you can't cheat and convert the sequences to lists!
// HINT: Unfortunately due to the design of the IEnumerable<T> interface,
// you'll need to use some mutability to achieve this. However, the mutability
// is unavoidable and limited to this function, and the function will still be
// a pure function, so we'll let it pass this time! :)
let zipSequences : ('a -> 'b -> 'c) -> 'a seq -> 'b seq -> 'c seq =
  fun fn xs ys ->
    seq {
      use xs' = xs.GetEnumerator ()
      use ys' = ys.GetEnumerator ()
      while (xs'.MoveNext () && ys'.MoveNext ()) do
        yield fn xs'.Current ys'.Current
    }

let a = [1;2;3;4;5];
let b = [10;20;30;40];
let fn a' b' = a'*b'

let test () = zipSequences fn a b

// What are the disadvantages of your zipSequences implementation versus your zipLists
// implementation?

// What are the advantages of your zipSequences implementation versus your zipLists
// implementation?


// There are multiple ways you could implement the applicative pattern for
// sequences. Previously you implemented applicative by creating the cross product
// of the applied sequences. Another way is 'zipping' the applied sequences together.
// For example:
// (fun a b -> (a, b)) <!> [1;2] <*> [3;4;5] = [(1,3);(2,4)]
type ZipList<'a> = ZipList of 'a seq     

// Implement map for ZipList
let mapZipList : ('a -> 'b) -> ZipList<'a> -> ZipList<'b> =
  fun fn (ZipList as') ->
    as' |> mapSeq fn |> ZipList

// Implement apply for ZipList
let applyZipList : ZipList<'a -> 'b> -> ZipList<'a> -> ZipList<'b> =
  fun (ZipList fns) (ZipList xs) ->
    seq {
      use fns' = fns.GetEnumerator ()
      use xs' = xs.GetEnumerator ()
      while (fns'.MoveNext () && xs'.MoveNext ()) do
        yield fns'.Current xs'.Current
    } |> ZipList

// Implement pure for ZipList
let pureZipList : 'a -> ZipList<'a> =
  fun x -> 
      seq { while true do yield x }
      |> ZipList
      
// One of the laws of applicatives is that functor's map and applicative's pure and apply
// must work consistently. More specifically
// fn <!> x
// MUST produce the same results as
// pure fn <*> x
// Does this law hold true for your implementation of map, pure and apply?
// Implement the following poor man's unit test to find out.
// If your ZipList implementation fails the test, go back and fix it.
let applicativeAndFunctorConsistencyLawTest : unit =
  let x = seq { 2..4} |> ZipList
  let fn x' = x' * x' 
  let (ZipList mapResults) = mapZipList <| fn <| x
  let (ZipList pureThenApplyResults) = applyZipList <| (pureZipList fn) <| x
  if Seq.toList mapResults <> Seq.toList pureThenApplyResults then
    failwithf "Oh no, your implementation is wrong! Map: %A PureThenApply: %A" mapResults pureThenApplyResults
  else
    printfn "It's working !!"

// Why do you think we can't implement ZipList using the F# list type internally
// (ie. why are we using seq?)
(*
  ZipList pure implementation requires to return an infinite enumerable structure in order to make sure it complies with all applicative laws. 
  While F# seq enables us to implement infinite enumerable structures, F# list type must have a predefined length.
  This makes it impossible to use list type as the internal structure of the ZipList implementation.
*)

// Implement the indexes function such that it returns an ascending sequence
// of integers starting from 0 up to but not including the value of the count arg
// Example: indexes 5 = [0;1;2;3;4]
let indexes : int -> int seq =
  fun count ->
    seq { 0..count-1 }


// Implement Seq.take yourself using your ZipList functor and applicative functions
// HINT: you will need to use the indexes function you just implemented
let seqTake : int -> 'a seq -> 'a seq =
  fun count xs ->
    let (<*>) = applyZipList
    let (<!>) = mapZipList
    let (ZipList output) = (fun a _ -> a) <!> (ZipList xs) <*> ZipList (indexes count)
    output

// Implement Seq.filter yourself using sequence expressions
let seqFilter : ('a -> bool) -> 'a seq -> 'a seq =
  fun predicate xs ->
    seq { for x in xs -> if (predicate x) then Some x else None } |> Seq.choose id

// Implement Seq.skip yourself using your ZipList functor and applicative functions
// HINT: use your indexes and seqFilter function
let seqSkip : int -> 'a seq -> 'a seq =
  fun count xs ->
    let toSkip = seqTake count xs 
    xs |> seqFilter (fun x -> not (toSkip |> Seq.contains x)) 