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
    open Program.CommonTop
    open Program.CommonLex

    ////helper functions

    let makeLineDataNoLabel words = 
        match words with
            | opc :: operands -> 
                Ok{ OpCode=opc; Operands=String.concat "" operands;Label=None; LoadAddr=WA(0u); SymTab=None}
            | _ -> Error("Error during testing, please specify opc :: operands type of string, without labels")

    
    /// remove comments from string
    let removeComment (txt:string) =
        txt.Split(';')
        |> function 
            | [|x|] -> x //if only one element in array, no comment found so return that
            | [||] -> "" //if linehas only comma and nothing else then line is empty
            | lineWithComment -> lineWithComment.[0] //if line has comment, return LHS which will be actual code
    
    /// split line on whitespace into an array
    let splitIntoWords ( line:string ) =
        line.Split( ([||] : char array), 
            System.StringSplitOptions.RemoveEmptyEntries)
        // line.Split( ([|' '; ','|] : char array), 
        //     System.StringSplitOptions.RemoveEmptyEntries) //removes whitespace

    let parseFn (strList : string list) = 
        strList
        |> makeLineDataNoLabel
        |> function
           | Ok(x) -> match parse(x) with
                      | Some(res) -> res
                      | None -> Error("The test string did not contain a supported opcode.")
           | Error(x) -> Error(x) // generally shouldn't happen

    let memQuickInstr (opRoot, opSuff,cond, op1,op2,opAddr,(opOff : Offset option))  = {
            PInstr = {
                        InsOpCodeRoot = opRoot;
                        InsOperand = {
                                        Op1 = op1;
                                        Op2 = op2;
                                        OpAddr = opAddr;
                                        OpOff = opOff;
                        };
                        InsOpCodeSuffix = opSuff;
                        InsClass = MEMSINGLE;
            };
            PLabel = None; //keep same always
            PSize = 4u;    //keep same always
            PCond = cond;
    }

    //this can be directly pipelines to parseFn
    //the parse error will flow monadically
    let execMemFn (instrResult : Result<Parse<Program.Memory.Instr>,string>) (memMap: MachineMemory<Program.Memory.Instr>) (dataPath: DataPath) =
        match instrResult with
        | Ok(instr) -> (execute instr memMap dataPath)
        | Error(x) -> Error(x)
    //Note: id == identity function
    //requires:
    //inputTransformFn: any-func to transform input data, use id if no func needed
    //outputTransformFn: any-func to transform output data, use id if no func needed
    //testFn: The actual function to be tested
    //testListName: Name of the testList
    //listOfIOPairs: A tuple list as [(inp0, out0) ; (inp1, out1) ; ... ] which contain raw input/output

    let parseAndExec (x , memMap, dataPath) = 
        let parsedInstr = x |> splitIntoWords |> Array.toList |> parseFn
        (execMemFn parsedInstr memMap dataPath)
    

    let checkCond (fl, c) : bool =
        isCondTrue fl c

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
                ("R0,[R1,R3]", Ok([OP("R0"); COMMA; LBRA; OP("R1"); COMMA; OP("R3"); RBRA; END]))
                ("R0[R1#4]!", Ok([OP("R0"); LBRA; OP("R1"); HASH; OFFSET("4"); RBRA; EXCL; END])) //shuld be fine at this stage not later
                ("R0[R1]#4", Ok([OP("R0"); LBRA; OP("R1"); RBRA; HASH; OFFSET("4"); END]))
                ("R0[R1]#4error", Error("LexerMatchFailed at: 'error'"))
    ]

    [<Tests>]
    let t3 = makeExpectoTestList id id (makeLiteral) "Valid Literal Tests" [
                ("-244", Ok({K=243u ; R=0; I=true}))
                ("124", Ok({K=124u ; R=0; I=false}))
                ("0xFFFFFFF0", Ok({K=240u ; R=2; I=true}))
                ("0x0FFFFFF0", Ok({K=255u ; R=2; I=true}))
                ("121", Ok({K=121u ; R=0; I=false}) ) //this should pass at this stage, exec will check if its valid
                ("0xFFF", LitParErrRor8 )
                ("4294967252", LitParErrInvalid)
    ]

    [<Tests>]
    let t4 = makeExpectoTestList makeLiteral id (Result.map(litValue)) "Valid LiteralVal (Inversion) Tests" [
                ("-44", Ok(4294967252u))
                ("44", Ok(44u))
                ("0xFFFFFFF0", Ok(0xFFFFFFF0u))
                ("0x0FFFFFF0", Ok(0x0FFFFFF0u))
                ("0xFFF", LitParErrRor8 )
                ("4294967252", LitParErrInvalid)
    ]

    [<Tests>]
    let t5 = makeExpectoTestList tokenize id (makeOperands) "Tokens Validate Tests" [
                   ("R0[R1]", InvalidSynComma)
                   ("R0", Error("InvalidSyntaxError: Too few or too many arguments"))
                   ("R0[R1#4]", InvalidSynComma)
    ]

    [<Tests>]
    let t6 = makeExpectoTestList (splitIntoWords >> Array.toList) id parseFn "Parse Tests" [
                   ("LDR R0, [R1]", Ok(memQuickInstr(OpLDR, NoneSuff, Cal, R0, R1, NORM, None)))
                   ("LDR R22, [R1]", Error("DestRegParseError: 'R22' is not a valid register"))
                   ("LDR R0, [R1, #164]!", Ok(memQuickInstr(OpLDR, NoneSuff, Cal, R0, R1, PRE, Some(Lit({K=164u ; R=0; I=false})) ) ))
                   ("LDR R0, [R1, LR]!", Ok(memQuickInstr(OpLDR, NoneSuff, Cal, R0, R1, PRE, Some(Reg(R14)) ) ))
                   ("STRB R9, [R3], R7", Ok(memQuickInstr(OpSTR, BSuff, Cal, R9, R3, POST, Some(Reg(R7)) ) ))
                   ("LDR R0, [R1, #0x0FFFFFF0]", Ok(memQuickInstr(OpLDR, NoneSuff, Cal, R0, R1, NORM, Some(Lit({K=255u ; R=2; I=true})) ) ))
    ]

    [<Tests>]
    let t7 = makeExpectoTestList id id checkCond "Cond Flag Tests" [
                   (({N=false;C=false;V=false;Z=false}, Ceq) , false)
                   (({N=false;C=false;V=false;Z=true}, Ceq) , true)
                   (({N=true;C=false;V=false;Z=false}, Cle) , true)
    ]

    [<Tests>]
    let t8 = makeExpectoTestList id id parseAndExec "Mem Tests" [
                   (("LDR R0, [R1]", emptyMachMemMap.Add(WA(0x100u), DataLoc(0x123u)), {defDataPath with Regs=defDataPath.Regs.Add(R1, 0x100u)}) , Ok(emptyMachMemMap, defDataPath))
    ]

[<EntryPoint>]
let main _ =
    let test = parseLine None (WA(0u))
    //test "LDR R0, [R1], #4asd" |> ignore
    //printfn "result: %A" (test "LDR R0, [R1]")


    //printfn "maptryfind: %A" (Map.tryFind (uint32(120)) allowedLiterals)
    //printfn "literal3: %A" (makeLiteral("4294967252"))
    Tests.runTestsInAssembly Tests.defaultConfig [||] |> ignore
    0 // return an integer exit code