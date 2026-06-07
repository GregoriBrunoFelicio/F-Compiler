let filter list fn =
    let rec loop list aux =
        match list with
         // |head::tail when fn head = true -> loop tail aux@[head]
        // |head::tail when fn head = false -> loop tail aux
        |head::tail ->
               if fn head = true then
                   loop tail [head]@aux
               else
                   loop tail aux
        |[] -> aux
    loop list []
    

let partition list fn =
    let rec loop list (aux1, aux2) =
         match list with
         | head::tail ->
                if fn head = true then
                    loop tail (head::aux1, aux2)
                else
                    loop tail (aux1, head::aux2)
         | [] -> aux1, aux2

    loop list ([], [])


let flatten list =
   let rec loop list aux =
        match list with
        |head::tail -> 
            loop tail (aux@head)
        |[] -> aux

   loop list []


type PaymentType =
    | Cash
    | Card of string
    | Pix of string


let processPayment (paymentType: PaymentType) =
    match paymentType with
    | Cash -> "Payment using Cash" 
    | Card flag -> $"Payment using Card {flag}"
    | Pix pix ->  $"Payment using Pix {pix}"

let summarizePayments payments =
    let rec loop payments (totalCash, totalCard, totalPix) =
        match payments with
        | head::tail ->
            match head with
            | Cash -> 
                let cs = totalCash + 1
                loop tail (cs, totalCard, totalPix)
            | Card _ ->
                let c = totalCard + 1
                loop tail (totalCash, c, totalPix)
            | Pix _ ->
                let p = totalPix + 1
                loop tail (totalCash, totalCard, p)
        | [] -> totalCash, totalCard, totalPix
    loop payments (0, 0, 0)
    
    
