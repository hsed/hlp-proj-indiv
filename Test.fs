open Program.CommonTop
open Program.CommonData

open System
open Expecto.ExpectoFsCheck
open Expecto
open Program.Memory



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
        |> List.map (fun (i, (inp, out)) -> (makeOneTest (i+1) inp out))
        |> Expecto.Tests.testList (testListName)



    [<Tests>]
    let t1 = makeExpectoTestList id id id "Sample String Identity Test" [
                ("Haaris", "Haaris")
    ]
    [<Tests>]
    let t2 = makeExpectoTestList id id (tokenize) "Operands Tokenize Tests" [
                //("R0[R1]",Ok([Token.OP1("R0"); LBRA; Token.OP2("R1"); RBRA; END]))
                ("R0", Ok([OP("R0") ; END]))  //shuld be fine at this stage not later
                ("R0,[R1]", Ok([OP("R0"); COMMA; LBRA; OP("R1"); RBRA; END]))
                ("R0,[R1,#4]", Ok([OP("R0"); COMMA; LBRA; OP("R1"); COMMA; HASH; OFFSET("4"); RBRA; END]))
                ("R0[R1#4]!", Ok([OP("R0"); LBRA; OP("R1"); HASH; OFFSET("4"); RBRA; EXCL; END])) //shuld be fine at this stage not later
                ("R0[R1]#4", Ok([OP("R0"); LBRA; OP("R1"); RBRA; HASH; OFFSET("4"); END]))
                ("R0[R1]#4error", Error("LexerMatchFailed at: 'error'"))
    ]

    [<Tests>]
    let t3 = makeExpectoTestList id id (makeLiteral) "Valid Literal Tests" [
                ("-245", Ok({K=244u ; R=0; I=true}))
                ("123", Ok({K=123u ; R=0; I=false}))
                ("0xFFFFFFF0", Ok({K=240u ; R=2; I=true}))
                ("0x0FFFFFF0", Ok({K=255u ; R=2; I=true}))
                ("0xFFF", Error("LiteralParseError: Literal must be producible by rotating right an 8-bit word within a 32-bit word") )
                ("4294967252", Error("LiteralParseError: Literal must be a valid signed int32 number"))
    ]

    [<Tests>]
    let t4 = makeExpectoTestList makeLiteral id (Result.map(litValue)) "Valid LiteralVal (Inversion) Tests" [
                ("-44", Ok(4294967252u))
                ("44", Ok(44u))
                ("0xFFFFFFF0", Ok(0xFFFFFFF0u))
                ("0x0FFFFFF0", Ok(0x0FFFFFF0u))
                ("0xFFF", Error("LiteralParseError: Literal must be producible by rotating right an 8-bit word within a 32-bit word") )
                ("4294967252", Error("LiteralParseError: Literal must be a valid signed int32 number"))
    ]

    [<Tests>]
    let t5 = makeExpectoTestList tokenize id (makeOperands) "Tokens Validate Tests" [
                   ("R0[R1]",Error("InvalidSyntaxError: Operands must be comma seperated"))
                   ("R0", Error("InvalidSyntaxError: Too few or too many arguments"))
                   ("R0[R1#4]", Error("InvalidSyntaxError: Operands must be comma seperated"))
    ]

[<EntryPoint>]
let main _ =
    printfn "Ssss %A" 1
    let test = parseLine None (WA(0u))
    test "LDR R0, [R1], #4asd" |> ignore
    printfn "result: %A" (test "LDR R0, [R1]")


    printfn "maptryfind: %A" (Map.tryFind (uint32(120)) allowedLiterals)
    //printfn "literal3: %A" (makeLiteral("4294967252"))
    Tests.runTestsInAssembly Tests.defaultConfig [||] |> ignore
    0 // return an integer exit code