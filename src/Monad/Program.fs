type Validator<'a> = 
    | Valid of 'a 
    | Invalid of string 

[<AutoOpen>]
module Validator = 

    //return 'a -> E<'a>
    let ret x = 
        Valid x

    // input: bind (Valid 5, (5 => Valid (5.ToString())) : output => Valid "5" 
    //bind E<'a> -> ('a -> E<'b>) -> E<'b>
    // E<'a> -> ('a -> E<'b>) -> E<'b>
    let bind (x : Validator<'a>) (f : 'a -> Validator<'b>) =    
        match x with 
        | Valid valid -> f valid 
        | Invalid str -> Invalid str

    let map f x = 
        match x with 
        | Valid valid -> Valid (f valid)
        | Invalid str -> Invalid str

    let apply (fres : Validator<'a -> 'b>) (xres : Validator<'a>) : Validator<'b> = 
        match fres, xres with 
        | Valid f, Valid x -> Valid (f x)
        | Valid f, Invalid x -> Invalid x 
        | Invalid f, Valid x -> Invalid f 
        | Invalid f, Invalid x -> Invalid (f + "," + x)

    let (>>=) = bind

    let (<!>) = map

    let (<*>) = apply
    
    (*let map f x = 
        x >>= (f >> Valid)
        
    let apply f x =
        bind f (fun ff -> 
            let map x = bind x (ff >> Valid)
            map x)*)

   



let numberIsEven (x : int) = 
    if x % 2 = 0 then 
        Valid x 
    else 
        Invalid "Number expected to be even"

let numberIsPositive (x : int) = 
    if x > 0 then 
        Valid x 
    else 
        Invalid "Number expected to be positive"

let transformNumber (x : int) = 
    x.ToString() |> Valid



let lameFunc x = 
    let res = numberIsEven x 
    match res with 
    | Valid r -> 
        let res2 = numberIsPositive r 
        match res2 with 
        | Valid r2 -> 
            let res3 = transformNumber r2
            match res3 with
            | Valid r3 -> Valid r3
            | Invalid str -> Invalid str
        | Invalid str -> Invalid str 
    | Invalid str -> Invalid str         

let coolFunc x = 
    ret x
    >>= numberIsEven
    >>= numberIsPositive
    >>= transformNumber

let mapFunc = 
    ret 5 
    |> Validator.map (fun x -> x + 2)
    |> Validator.map (fun c -> c.ToString())


let isWheelsValid (wheelnr : int) (wheels : int) = 
    if wheelnr <> wheels then 
        Invalid $"expected {wheels} got {wheelnr}"
    else 
        Valid wheels

let isSteeringValid (steering : bool) = 
    if steering then 
        Valid steering 
    else 
        Invalid "Dont have steering"

let orElse validator1 validator2 = 
    match validator1 with 
    | Valid v1 -> 
        Valid v1 
    | Invalid str -> 
        validator2

let (<|>) = orElse

type Bike = {
    Wheels : int 
    Steering : bool 
    Pedals : int 
    Model : string 
}

module Bike = 
    let private make wheels steering pedals model = {
        Wheels = wheels 
        Steering = steering 
        Pedals = pedals 
        Model = model
    }

    let Create wheels steering pedals model = 
        make  
        <!> (isWheelsValid 2 wheels <|> isWheelsValid 3 wheels)
        <*> isSteeringValid steering 
        <*> Valid pedals 
        <*> Valid model


let addMultipleBikes = 
    [
        Bike.Create 2 true 2 "momas"
        Bike.Create 3 true 2 "dbs"
        Bike.Create 2 true 2 "gekko"
    ]

let sequence list = 
    let cons head tail = head :: tail 
    let initState = ret [] 
    let folder head tail = 
        ret cons <*> (id head) <*> tail 
    List.foldBack folder list initState

[<EntryPoint>]
let main argv = 
    let bike = Bike.Create 3 true 2 "dbs"
    
    printfn "%A" addMultipleBikes

    addMultipleBikes 
    |> sequence
    |> fun bikes -> 
        match bikes with 
        | Valid b -> printfn "bikes were valid!"
        | Invalid str -> printfn "bikes were invalid!"

    0


    
