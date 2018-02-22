open Program.CommonTop
open Program.CommonData

open System
open Expecto.ExpectoFsCheck
open Expecto




module Tests = 


    open System
    open Expecto.ExpectoFsCheck
    open Expecto
    open Program.Memory

    //Note: id == identity function
    //requires:
    //inputTransformFn: any-func to transform input data, use id if no func needed
    //outputTransformFn: any-func to transform output data, use id if no func needed
    //testFn: The actual function to be tested
    //testListName: Name of the testList
    //listOfIOPairs: A tuple list as [(inp0, out0) ; (inp1, out1) ; ... ] which contain raw input/output

    //Returns compound Tests as Test

    let makeExpectoTestList inputTransformFn outputTransformFn testFn testListName listOfIOPairs =
        
        //single test case
        let makeOneTest (index) (input) (output) = 
            testCase (sprintf "%s:%d" "SubTest" index) <| fun () -> 
                Expect.equal (input |> inputTransformFn |> testFn) (output |> outputTransformFn) ""
        
        //index all pairs
        listOfIOPairs
        |> List.indexed
        |> List.map (fun (i, (inp, out)) -> (makeOneTest i inp out))
        |> Expecto.Tests.testList (testListName)



    [<Tests>]
    let t1 = makeExpectoTestList id id id "Sample String Identity Test" [
                ("Haaris", "Haaris")
                ("a", "a")
                ("cat", "cat")
    ]
    [<Tests>]
    let t2 = makeExpectoTestList id id id "Sample Integer Identity Test" [
                (1,1)
                (-1,-1)
    ]    
    [<Tests>]
    let t3 = makeExpectoTestList id id (tokenize) "Operands Tokenize Tests" [
                //("R0[R1]",Ok([Token.OP1("R0"); LBRA; Token.OP2("R1"); RBRA; END]))
                ("R0", Ok([OP("R0") ; END]))
                ("R0[R1]", Ok([OP("R0"); LBRA; OP("R1"); RBRA; END]))
                ("R0[R1#4]", Ok([OP("R0"); LBRA; OP("R1"); HASH; OFFSET("4"); RBRA; END]))
                ("R0[R1#4]!", Ok([OP("R0"); LBRA; OP("R1"); HASH; OFFSET("4"); RBRA; EXCL; END]))
                ("R0[R1]#4", Ok([OP("R0"); LBRA; OP("R1"); RBRA; HASH; OFFSET("4"); END]))
                ("R0[R1]#4error", Error("LexerMatchFailed at: 'error'"))
    ]

    [<Tests>]
    let t4 = makeExpectoTestList id id (tokenize >> tokenValidate) "Tokens Validate Tests" [
                //("R0[R1]",Ok([Token.OP1("R0"); LBRA; Token.OP2("R1"); RBRA; END]))
                ("R0", Error("InvalidSyntaxError: Too few or too many arguments"))
                ("R0[R1]", Ok([OP("R0"); LBRA; OP("R1"); RBRA; END]))
                ("R0[R1#4]", Ok([OP("R0"); LBRA; OP("R1"); HASH; OFFSET("4"); RBRA; END]))
                ("R0[R1#4]!", Ok([OP("R0"); LBRA; OP("R1"); HASH; OFFSET("4"); RBRA; EXCL; END]))
                ("R0[R1]#4", Ok([OP("R0"); LBRA; OP("R1");  RBRA; HASH; OFFSET("4");  END])) 
               
                ("R0[R1", Error("InvalidSyntaxError: Too few or too many arguments"))
                ("R0[R1#4]6#", Error("InvalidSyntaxError: Incorrect order or too many args"))
                ("R0[R1#4]!!", Error("InvalidSyntaxError: Incorrect order or too many args"))
                ("R0[R1!!]#41", Error("InvalidSyntaxError: Too few or too many arguments"))
    ]


[<EntryPoint>]
let main _ =
    printfn "Ssss %A" 1
    let test = parseLine None (WA(0u))
    //test "LDR R0, [R1, #4]" |> ignore
    //test "LDR R0, [R1, #4]!" |> ignore
    //test "LDR R0, [R1], #4" |> ignore
    test "LDR R0, [R1], #4asd" |> ignore
    printfn "result: %A" (test "LDR R0, [R1]")
    Tests.runTestsInAssembly Tests.defaultConfig [||] |> ignore
    0 // return an integer exit code