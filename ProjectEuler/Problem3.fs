module Problem3
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Math

let problem3_firstTry () =
    let testPrime (possiblePrime:int64) =
        let console = sprintf "Checking For Prime: %i" possiblePrime
        System.Console.WriteLine(console)

        let sqrRootOfPrime = int64(System.Math.Sqrt(float(possiblePrime)))
            
        {1L .. sqrRootOfPrime}
        |> PSeq.forall(fun divisor -> 
            let console = sprintf "Checking For Prime: %i with Divisor: %i" possiblePrime divisor
            System.Console.WriteLine(console)

            match divisor with
            | 1L -> true
            | x when divisor = possiblePrime -> true
            | _ -> possiblePrime % divisor > 0L)

    let factorSeq (ofNumber:int64) =
        {(ofNumber/2L) .. -1L .. 1L}
        |> PSeq.filter(fun testNumber ->
            try
                let console = sprintf "Checking if Factor: %i" testNumber
                System.Console.WriteLine(console)

                ofNumber % testNumber = 0L
            with
            | ex -> failwithf "Exception Checking For Factors OfNumber:%i TestNumber:%i Exception:%s" ofNumber testNumber (ex.ToString()))

    factorSeq 600851475143L
    //factorSeq 13195L
    //factorSeq 15L
    //factorSeq 13195L
    |> PSeq.find (fun factor ->
        let console = sprintf "Test Factor %i" factor
        System.Console.WriteLine(console)
        testPrime factor)
        

let problem3_secondTry () =
    let testPrime (possiblePrime:int64) =
        let sqrRootOfPrime = int64(System.Math.Sqrt(float(possiblePrime)))
            
        //{sqrRootOfPrime .. -1L .. 1L}
        {1L .. sqrRootOfPrime}
        |> Seq.forall(fun divisor -> 
            match divisor with
            | 1L -> true
            | x when divisor = possiblePrime -> true
            | _ -> possiblePrime % divisor > 0L)

    let factorOf number testFactor =
        number % testFactor = 0L

    let generatePrimes (maximum:int64) =
        {maximum .. -1L .. 1L}
        |> Seq.filter(fun possiblePrime -> testPrime possiblePrime)
    
    let largestPrimeFactors ofNumber=
        (ofNumber / 2L)
        |> generatePrimes
        |> Seq.find(fun prime ->
            System.Console.WriteLine("Testing Prime:" + (prime.ToString()))
            factorOf ofNumber prime)

    let result =
        600851475143L
        //13195L
        |> largestPrimeFactors 

    result.ToString()

let problem3_thirdTry () =
    
    let createSieveOfAtkin (limit: int64) =

        let sqrtLimit =
            limit
            |> float
            |> sqrt
            |> int64

        let initialResults = [ 2L ; 3L ; 5L ]

        let resultSet = ref Set.empty

        {6L .. (limit - 5L)}
        |> Seq.filter(fun value -> PSeq.forall (fun initial -> value % initial <> 0L) initialResults)
        |> Seq.filter(fun value ->
                Set.toSeq (!resultSet)
                |> PSeq.forall(fun previouslyFoundPrime ->
                        {1L .. limit}
                        |> PSeq.forall(fun index -> not(index * (previouslyFoundPrime * previouslyFoundPrime) = value))
                    )
            )
        |> Seq.filter(fun value ->
                
                let r = value % 60L
                let isPrime = ref false

                if r % 4L = 1L then
                    isPrime :=
                        let points =
                            {1L .. sqrtLimit}
                            |> PSeq.map (fun x ->
                                {1L .. sqrtLimit}
                                |> PSeq.choose(fun y ->
                                    match (4L * (x * x)) + (y * y) = value with
                                    | true -> Some(x, y)
                                    | _ -> None))
                            |> PSeq.concat
                            |> PSeq.toList

                        List.length points
                        |> fun x -> x % 2 = 1

                if r % 6L = 1L then
                    isPrime :=
                        let points =
                            {1L .. sqrtLimit}
                            |> PSeq.map (fun x ->
                                {1L .. sqrtLimit}
                                |> PSeq.choose(fun y ->
                                    match (3L * (x * x)) + (y * y) = value with
                                    | true -> Some(x, y)
                                    | _ -> None))
                            |> PSeq.concat
                            |> PSeq.toList

                        List.length points
                        |> fun x -> x % 2 = 1

                if r % 12L = 11L then
                    isPrime :=
                        let points =
                            {1L .. sqrtLimit}
                            |> PSeq.map (fun x ->
                                {1L .. sqrtLimit}
                                |> PSeq.choose(fun y ->
                                    match (x > y) && ((3L * (x * x)) - (y * y)) = value with
                                    | true -> Some(x, y)
                                    | _ -> None))
                            |> PSeq.concat
                            |> PSeq.toList
                        
                        List.length points
                        |> fun x -> x % 2 = 1
                    
                !isPrime)
        |> Seq.append initialResults
        |> Seq.map(fun prime ->
                
                System.Console.WriteLine("Prime: " + (prime.ToString()))

                resultSet := Set.add prime !resultSet
                prime)
        |> Seq.append [1L]

    let number = 600851475143L

    let primes =
        createSieveOfAtkin (number / 2L)

    let maxPrimeFactor =
        primes
        |> Seq.filter(fun prime -> number % prime = 0L)
        |> Seq.max

    maxPrimeFactor

let problem3_fourthTry () =
    let testPrime (possiblePrime:int64) =
        let sqrRootOfPrime = int64(System.Math.Sqrt(float(possiblePrime)))
            
        {1L .. sqrRootOfPrime}
        |> Seq.forall(fun divisor -> 
            match divisor with
            | 1L -> true
            | x when divisor = possiblePrime -> true
            | _ -> possiblePrime % divisor > 0L)

    let factorSeq ofNumber =
        {2L .. (ofNumber/2L)}
        |> Seq.filter(fun testNumber -> ofNumber % testNumber = 0L)
        |> Seq.map(fun factor1 -> (factor1, (ofNumber / factor1)))
        |> Seq.takeWhile(fun (factor1, factor2) -> factor1 <= factor2)
        |> Seq.fold (fun acc (factor1, factor2) -> factor1 :: factor2 :: acc) []
        |> Seq.sort
        |> Seq.toList
        |> List.rev

    factorSeq 600851475143L
    |> Seq.find (fun factor -> testPrime factor)
