open System

let notImplemented () = raise <| NotImplementedException ()

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
// As per applicative lows, every Applicative must implement pure and apply. If we can implement map using pure and apply that means every applicative is a functor too.
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
    | Ok fn' , Ok x'     -> pureResult <| fn' x'
    | Ok fn' , Error e   -> Error e
    | Error e, Ok x'     -> Error e
    | Error e, Error _   -> Error e // ????

//let bindResult : ('a -> Result<'b, 'e>) -> Result<'a, 'e> -> Result<'b, 'e> = 
//  fun fn x ->
//   match x with
//     | Ok x' -> fn x'
//     | Error e -> Error e

let applyRsult' : Result<('a -> 'b), 'e> -> Result<'a, 'e> -> Result<'b, 'e> = 
  fun fn x ->
    notImplemented () // Could be better than above

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
   | Failure fne, Success x' -> Failure fne
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
    
// TODO: Async, List, ZipList
