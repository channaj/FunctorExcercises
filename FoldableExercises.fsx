open System

let notImplemented () = raise <| NotImplementedException ()


// Try to do each of these without using the compiler, then check your result
// after you've written it. This means don't use VSCode with Ionide either ;)

type MyList<'a> =
  | Cons of 'a * MyList<'a>
  | EndOfList


// The repeat function creates a list that contains the `x` parameter `count` times.
// eg. repeat 3 "a" = Cons ("a",Cons ("a",Cons ("a",EndOfList)))
// Implement this function using recursion.
let rec repeat : int -> 'a -> MyList<'a> =
  fun count x ->
    if count > 0 then
     Cons (x, repeat (count-1) x)
    else
     EndOfList


// Is your function tail recursive? You can check by using a large count number
// like 30000 and seeing if it blows the stack.
// Make your implementation tail recursive below. If it is already tail recursive,
// copy your solution here and edit your previous one to be _not_ tail recusive
// for comparison.
let repeatTailRecursive : int -> 'a -> MyList<'a> =
  fun count x ->
    let rec step lst i =
      if i > 0 then
        step (Cons (x, lst)) (i-1)
      else
        lst
    step EndOfList count

// Implement `countDownFrom` which counts down to zero from the number you pass in
// using recursion.
// eg. countDownFrom 3 = Cons (3,Cons (2,Cons (1,Cons (0,EndOfList))))
let countDownFrom : int -> MyList<int> =
  fun start ->
    let rec step i =
      if i >= 0 then
        Cons (i,step (i-1))
      else
        EndOfList 
    step start 


// Is your `countDownFrom` tail recursive? If not, make it so below.
// If it is already tail recursive, copy your solution here and edit
// your previous one to be _not_ tail recusive for comparison.
let countDownFromTailRecursive : int -> MyList<int> =
  fun start ->
    let rec step lst i =
     if i <= start then
      step (Cons (i, lst)) (i+1)
     else
      lst
    step EndOfList 0


// Implement the sum function that adds up all the ints in the list
// and returns the total. Use recursion.
let sum : MyList<int> -> int =
  fun ints ->
    let rec step lst =
      match lst with
      | EndOfList -> 0
      | Cons (x, xs) -> x + step xs
    step ints


// Make your `sum` tail recursive if it is not already.
// If it is already tail recursive, copy your solution here and edit
// your previous one to be _not_ tail recusive for comparison.
let sumTailRecursive : MyList<int> -> int =
  fun ints ->
    let rec step lst i =
      match lst with
      | EndOfList -> i
      | Cons (x, xs) -> step xs (i+x)
    step ints 0


// Implement a function that takes a list and returns it in reverse
// order. Make sure your implementation is tail recursive.
let reverseList : MyList<'a> -> MyList<'a> =
  fun lst ->
    let rec step lst' out =
      match lst' with
      | EndOfList -> out
      | Cons (x, xs) -> step xs (Cons (x, out))
    step lst EndOfList

// Implement the partition function using tail recursion. Partition uses a
// predicate function to split a list into two sublists. The order of the
// list elements should be retained.
// For example:
// let isOdd x = x % 2 = 1
// partition isOdd (Cons (3,Cons (4,Cons (5,EndOfList)))) = (Cons (3, Cons (5, EndOfList)), Cons (4, EndOfList))
// HINT: You will need to use your reverseList function
let partition : ('a -> bool) -> MyList<'a> -> MyList<'a> * MyList<'a> =
  fun predicate lst ->
    let rec step lst' out =
      match lst', out with
      | EndOfList, _ -> out
      | Cons (x, xs), (o1, o2) -> 
        if (predicate x) then
          step xs (Cons (x, o1), o2)
        else
          step xs (o1, Cons (x, o2))
    let (a,b) = step lst (EndOfList, EndOfList)
    (reverseList a, reverseList b)

// let isOdd x = x % 2 = 1
// let lst = countDownFromTailRecursive 10
// let test = partition isOdd lst

// You might have noticed a common way all these different tail-recursive functions work.
// They all walk the list recursively and accumulate some state they pass forward
// through the recursion. At the end of the list, they return that final state.
// We can extract this commonality; This general pattern is called a fold.
// Implement the fold function for MyList, tail recursively.
let fold : ('state -> 'a -> 'state) -> 'state -> MyList<'a> -> 'state =
  fun folder initialState lst ->
    let rec step state lst' =
      match lst' with
      | EndOfList -> state
      | Cons (x, xs) -> step (folder state x) xs
    step initialState lst


// Reimplement sum using fold
let sumUsingFold : MyList<int> -> int =
  fun lst ->
    fold (+) 0 lst

// let lst = countDownFromTailRecursive 5
// let total = sumUsingFold lst

// Reimplement reverseList using fold
let reverseListUsingFold : MyList<'a> -> MyList<'a> =
  fun lst ->
   fold (fun x y -> Cons (y, x)) EndOfList lst


// Reimplement partition using fold
let partitionUsingFold : ('a -> bool) -> MyList<'a> -> MyList<'a> * MyList<'a> =
  fun predicate lst ->
    let (a,b) = 
      fold 
        (fun (o1, o2) x ->
          if (predicate x) then 
            Cons (x, o1), o2
          else 
            o1, Cons (x, o2)
        ) (EndOfList, EndOfList) lst
    (reverseList a, reverseList b)

// let isOdd x = x % 2 = 1
// let lst = countDownFromTailRecursive 10
// let test = partitionUsingFold isOdd lst

// A different version of partition that would indicate its behaviour better
// in the types and also be more generic would use a Choice type instead of
// a bool. Let's try a version that can partition three ways.
// Implement this function using a fold.
let betterPartition : ('a -> Choice<'b,'c,'d>) -> MyList<'a> -> MyList<'b> * MyList<'c> * MyList<'d> =
  fun makeChoice lst ->
    let (a, b, c) = 
      fold 
        (fun (o1, o2, o3) x ->
          match makeChoice x with
          | Choice1Of3 a -> (Cons (a,o1), o2, o3)
          | Choice2Of3 b -> (o1, Cons (b,o2), o3)
          | Choice3Of3 c -> (o1, o2, Cons (c,o3))
        ) (EndOfList, EndOfList, EndOfList) lst
    (reverseList a, reverseList b, reverseList c)

let chooser x = 
  if x > 7 then Choice1Of3 <| sprintf "%A > 7" x
  elif x % 2 = 0 then Choice2Of3 (x,"Even")
  else Choice3Of3 x

let lst = countDownFromTailRecursive 10
let test = betterPartition chooser lst

// Folds are not limited to linked lists. You can fold over many different types.
// Write a fold for an array using tail recursion.
let foldArray : ('state -> 'a -> 'state) -> 'state -> 'a[] -> 'state =
  fun folder initialState arr ->
    notImplemented ()


// Write an array to MyList function using your foldArray function. The
// order of items in the array must be preserved.
let arrayToMyList : 'a[] -> MyList<'a> =
  fun arr ->
    notImplemented ()


// This is a weird (but simple) one. Write a fold for the option type.
// This sort of fold is not used in practice very much, however it demonstrates
// that the concept of a fold generalises beyond just lists.
let foldOption : ('state -> 'a -> 'state) -> 'state -> 'a option -> 'state =
  fun folder initialState opt ->
    notImplemented ()


// Implement the 'defaultValue' function using your foldOption function
// eg. defaultValue 12 None     = 12
//     defaultValue 12 (Some 1) = 1
let defaultValue : 'a -> 'a option -> 'a =
  fun def opt ->
    notImplemented ()