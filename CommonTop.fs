// Learn more about F# at http://fsharp.org
namespace CommonTop

////////////////////////////////////////////////////////////////////////////////////
//      Code defined at top level after the instruction processing modules
////////////////////////////////////////////////////////////////////////////////////


module CommonTop =

    open Common.CommonLex
    open Common.CommonData
    open Memory

    /// allows different modules to return different instruction types
    type Instr =
        | IMEM of Memory.Instr
    
    /// allows different modules to return different error info
    /// by default all return string so this is not needed
    type ErrInstr =
        | ERRIMEM of Memory.ErrInstr
        | ERRTOPLEVEL of string

    /// Note that Instr in Mem and DP modules is NOT same as Instr in this module
    /// Instr here is all possible instruction values combines with a D.U.
    /// that tags the Instruction class
    /// Similarly ErrInstr
    /// Similarly IMatch here is combination of module IMatches
    let IMatch (ld: LineData) : Result<Parse<Instr>,ErrInstr> option =
        //fr => functionResult
        //fe => functionError
        let pConv fr fe p = pResultInstrMap fr fe p |> Some
        match ld with
        | Memory.IMatch pa -> pConv IMEM ERRIMEM pa
        | _ -> None
    
    

    type CondInstr = Condition * Instr

    //loadAddr is where to load instruction from
    //asmLine is actual string to parse
    let parseLine (symtab: SymbolTable option) (loadAddr: WAddr) (asmLine:string) =
        /// put parameters into a LineData record
        let makeLineData opcode operands = {
            OpCode=opcode
            Operands=String.concat "" operands //separate using pipeline, experimental!! **edited** dont seperate
            Label=None
            LoadAddr = loadAddr
            SymTab = symtab
        }
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
        

        /// try to parse 1st word, or 2nd word, as opcode
        /// If 2nd word is opcode 1st word must be label
        let matchLine words =
            let pNoLabel =
                match words with
                | opc :: operands -> 
                    makeLineData opc operands 
                    |> IMatch
                | _ -> None

            
            match pNoLabel, words with
            | Some pa, _ -> pa  //successful parse first word as opcode
            | None, label :: opc :: operands -> 
                match { makeLineData opc operands 
                        with Label=Some label} 
                      |> IMatch with
                | None -> 
                    Error (ERRTOPLEVEL (sprintf "Unimplemented instruction %s" opc))
                | Some pa -> pa
            | _ -> Error (ERRTOPLEVEL (sprintf "Unimplemented instruction %A" words))
        

        //printfn "Flow: %A" (asmLine |> removeComment |> splitIntoWords |> Array.toList |> matchLine) 

        //main function flow
        asmLine
        |> removeComment
        |> splitIntoWords
        |> Array.toList
        |> matchLine

////////////////////////////////////////////////////////////////////////////////////
//      Code defined for top-level testing
////////////////////////////////////////////////////////////////////////////////////

module CommonTest =
    open Expecto
    //TODO:


////////////////////////////////////////////////////////////////////////////////////
//      Code defined to execute top-level tests
////////////////////////////////////////////////////////////////////////////////////

module CommonTestExecute =
    open Expecto

    [<EntryPoint>]
    Tests.runTestsInAssembly Tests.defaultConfig [||] |> ignore