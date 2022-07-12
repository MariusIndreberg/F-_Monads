type Validator<'a> = 
    | Valid of 'a 
    | Invalid of string 


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

type C = {
    D : Validator<int>
} with 
    static member Create d = {
        D = d
    }

type B = {
    C : Validator<C>
} with 
    static member Create c = {
        C = c
    }


type A = {
    B : Validator<B>
} with 
    static member Create b = {
        B = b
    }

let createA = {
    B = Valid { 
        C = Valid { 
            D = Valid 2 
        } 
    }
}

let getNumber (a : A) = 
    match a.B with 
    | Valid b -> 
        match b.C with 
        | Valid c ->
            match c.D with 
            | Valid d -> 
                Valid d 
            | Invalid str -> Invalid str 
        | Invalid str -> Invalid str 
    | Invalid str -> Invalid str 



[<EntryPoint>]
let main argv = 

    0


    
