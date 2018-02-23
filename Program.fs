// Learn more about F# at http://fsharp.org
namespace Program

module CommonData =
    //////////////////////////////////////////////////////////////////////////////////////
    //                   Common types and code used by all modules
    //////////////////////////////////////////////////////////////////////////////////////

    /// ARM Status bits
    type Flags = { N: bool; C:bool; Z: bool; V:bool}


    ////////////////////////ARM register names and operations/////////////////////////////
   

    /// ARM register names
    /// NB R15 is the program counter as read
    [<Struct>]
    type RName = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 
                 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

    /// ARM state as values of all registers and status bits
    /// NB PC can be found as R15 - 8. (Pipelining)
    type DataPath = {Fl: Flags; Regs:Map<RName,uint32>}

    /// Map used to convert strings into RName values, 
    /// includes register aliasses PC, LR, SP
    let regNames = 
        Map.ofList [ 
            "R0",R0 ; "R1",R1 ; "R2",R2 ; "R3",R3 ; "R4",R4 ; "R5",R5
            "R6",R6 ; "R7",R7 ; "R8",R8 ; "R9", R9 ; "R10",R10 ; "R11",R11 ; 
            "R12",R12 ; "R13",R13 ; "R14",R14 ; "R15",R15 ; 
            "PC",R15 ; "LR",R14 ; "SP",R13 
        ] 

    // various functions used to convert between string, RName, and register number

    /// Inverse of regNames, used to convert RName values to strings
    /// NB The string chosen will always be the register (not alias)
    let regStrings = 
        regNames
        |> Map.toList
        |> List.map (fun (s,rn)-> (rn,s)) 
        |> List.filter (fun (_,s:string) -> s.StartsWith "R")
        |> Map.ofList

    /// Map converts RName into register number (no aliasses)
    let regNums = Map.map (fun _ (s:string) -> int (s.[1..])) regStrings

    /// Map converts register number into RName (no aliasses)
    let inverseRegNums = 
        regNums |> Map.toList 
        |> List.map (fun (rn,n)->(n,rn)) |> Map.ofList

    /// Property on RName to return register number, for convenience
    /// Aliasses not included, since they are not RNames
    type RName with
        /// Return the number of a register as an integer
        member r.RegNum = regNums.[r]
    
    /// Return a register name from an integer
    let register n = 
        if 0 <= n && n < 16 
        then inverseRegNums.[n] 
        else (failwithf "Register %d does not exist!" n)

    /// Type to represent the contents of one memory location
    /// 'INS is a parameter set to the type of an instruction
    /// needed because instruction type is only defined
    /// at top level.
    type MemLoc<'INS> =
        | DataLoc of uint32 //we will usually use this one for now
        | Code of 'INS

    /// type to represent a (word) address
    /// there is some ambiguity. Does this contain the real address
    /// which is always divisible by 4
    /// or does it contain the word number (real address dvided by 4)
    /// either way multiply/divide by 4 will cause problems!
    /// document this well and be consistent.
    type WAddr = | WA of uint32

    /// type to represent memory
    type MachineMemory<'INS> = Map<WAddr,MemLoc<'INS>>


////////////////////////////////////////////////////////////////////////////////////////////////////
//                     Common code for Instruction Definition and Parsing
////////////////////////////////////////////////////////////////////////////////////////////////////

module CommonLex =

    open CommonData
    
    /// ARM execution conditions
    type Condition =

        | Ceq
        | Cne
        | Cmi
        | Cpl
        | Chi
        | Chs
        | Clo
        | Cls
        | Cge
        | Cgt
        | Cle
        | Clt
        | Cvs
        | Cvc
        | Cnv // the "never executed" condition NV - not often used!
        | Cal // the "always executed condition "AL". Used by default on no condition

    /// classes of instructions (example, add/change this is needed)
    type InstrClass = | MEM

    /// specification of set of instructions
    type OpSpec = {
        InstrC: InstrClass
        Roots: string list
        Suffixes: string list
    }

    type SymbolTable = Map<string,uint32> //value must be a valid WAddr

    /// result returned from instruction-specific module parsing
    /// an instruction class. If symbol definitions are found in a 
    /// symbol table then a complete parse will be output
    /// otherwise some fields will be None
    type Parse<'INS> = {
            /// value representing instruction. NB type varies with instruction class
            PInstr: 'INS 
            /// name and value of label defined on this line, if one is.
            PLabel: (string * uint32) option //this will be given to symbol table?
            /// number of bytes in memory taken up by this instruction
            PSize: uint32 
            /// execution condition for instruction
            PCond: Condition
        }

    /// data given to instruction-specific parse function
    type LineData = {
        /// memory address this instruction is loaded. Must be word address
        LoadAddr: WAddr 
        /// name of label defined on this line, if one exists
        Label: string option 
        /// table of symbols with defined values. 
        /// if this is given we are phase 2 and all symbols should be defined
        /// if this is not given we are phase 1 and no symbols are defined
        SymTab: SymbolTable option
        /// opcode string
        OpCode: string
        /// string of all the operands
        Operands: string
    }


    /// Strings with corresponding execution condition
    /// Note some conditions have multiple strings
    /// Note "" is a valid condition string (always execute condition)
    let condMap = [ "EQ",Ceq ; "NE",Cne ; "MI",Cmi ; "PL",Cpl ; "HI", Chi ; "HS",Chs ; "LO",Clo ; 
                    "LS",Cls ; "GE",Cge ; "GT", Cgt ; "LE", Cle ; "LT", Clt ; "VS",Cvs ; 
                    "VC",Cvc ; "NV",Cnv ; "AL",Cal ; "",Cal] |> Map.ofList

    /// list of all strings representing execution conditions
    /// includes ""
    let condStrings = 
        condMap
        |> Map.toList
        |> List.map fst
        |> List.distinct    

    /// generate all possible opcode strings for given specification
    /// each string is paired with info about instruction
    /// and the three parts of the opcode
    let opCodeExpand (spec: OpSpec) 
        //    opcode    class        root    suffix   instr cond
        : Map<string, InstrClass * (string * string * Condition)> =
        spec.Roots
        |> List.collect (fun r -> 
            spec.Suffixes
            |> List.collect (fun s -> 
                condStrings
                |> List.map (fun c -> r+s+c, (spec.InstrC,(r,s, condMap.[c])))))
                |> Map.ofList

    /// function used to change PInstr field of a Result<Parse<'INS>,'E>
    /// the output has this field mapped with fMap
    /// or if Error has this value chnaged by fMapE
    let pResultInstrMap fMap fMapE paRes =
        match paRes with
        | Ok ({PInstr=ins} as pr) -> 
            // Note subtle point. {pr with Pinst = ...} will not work here
            // That is because applying fmap changes the type of PInstr
            // and therefore the type of the record.


            //this is a result of type Parse
            Ok {
            PInstr = fMap ins  //parsedInstruction: LDR, STR, etc.
            PLabel = pr.PLabel
            PCond = pr.PCond
            PSize = pr.PSize
            }
        | Error e -> Error (fMapE e)

///***************MEMORY MODULE ***************///

module Memory =
    open System
    open CommonData
    open CommonLex
    open EEExtensions

    //valid aka avail opcodes
    type OpCode =
        | OpLDR
        | OpSTR
        // add more
    
    type Suffix =
        | BSuff
        | NoneSuff

    //map each string to a valid opcode
    let OpCodeMap = [ "LDR",OpLDR ; "STR",OpSTR ] |> Map.ofList ;

    let SuffixMap = [ "B", BSuff ; "", NoneSuff ] |> Map.ofList ;
    
    /// literal value = (K % 256) rotated right by (R % 16)*2
    type Literal = {K: uint32; R: int; I: bool} // best practice, see later
    //reverse of OpCodeMap and extract strings, used for namespec
    let TStrings map = 
        map
        |> Map.toList
        |> List.map fst
        |> List.distinct
    /// sample specification for set of instructions

    type AddrScheme =
         | NORM //standard offset addressing
         | PRE  //increase R1 by offset (R1'=R1+OFFSET, then look for word in mem[r1']
         | POST //Look for word in mem[r1] then increase r1 (r1'=r1+offset)
     
    type Offset = Literal of uint32 //temp for now

    // change these types as required

    /// instruction (dummy: must change) MUSTTTT
    type Operand = {Op1: RName; Op2: RName; OpAddr: AddrScheme; OpOff: Offset option;}
    type Instr =  {InsOpCodeRoot: OpCode; InsOpCodeSuffix: Suffix; InsOperand: Operand}

    /// parse error (dummy, but will do)
    type ErrInstr = string

    

    let memSpec = {
        InstrC = MEM
        Roots = TStrings (OpCodeMap)
        Suffixes = TStrings (SuffixMap)
    }

    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand memSpec

    /// main function to parse a line of assembler
    /// ld contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)

    type Token = | OP of string
                 | OFFSET of string //<--need to support literal data for this!!
                 | LBRA //[
                 | RBRA //]
                 | EXCL //!
                 | HASH //#
                 | COMMA //,
                 | END
                 | ERROR of string
    //type LexerState = OP1ST | OP2ST | OFFST | ENDST
    type LexData = { Txt: string; Numb: int}
    (*
        Accepted token list for opcodes
        OP1 ; LBRA ; OP2 ; RBRA                      => ldr op1, [op2]
        OP1 ; LBRA ; OP2; OFFSET ; RBRA        => ldr op1, [op2, #offset]
        OP1 ; LBRA ; OP2; OFFSET ; RBRA ; EXCL => ldr op1, [op2, #offset]!
        OP1 ; LBRA ; OP2; RBRA; OFFSET         => ldr op1, [op2] #offset
    *)

    let (|LexMatch|_|) regex state =
        let debug = false

        match String.regexMatch regex state.Txt with
        | None -> if debug 
                  then printfn "Match of '%s' with '%s' failed." state.Txt regex; 
                  None
        | Some (mStr, _) -> 
            let mChars = String.length mStr
            if mChars = 0 then 
                failwithf "What? Unexpected 0 character match in LexMatch '%s'" regex
            if debug then
                printfn "Match of '%s' with '%s' OK: match is '%s" state.Txt regex mStr; 
            let state' = {state with Txt = state.Txt.[mChars..]}
            Some (mStr,state')


    /// Returns next token Option, and new state, given state lData
    /// If it returns None for token, with a changed state
    /// It will be called again with new state
    /// It must never be called with Txt=""
    let nextToken lData =
        let incr st = {st with Numb = st.Numb+1}
        let retTag tag ld = Some(tag), incr(ld)
        match lData with 
        | LexMatch "^," (_,sta) -> retTag COMMA sta
        | LexMatch @"^[rR]\d{1,2}" (sym, sta) -> retTag (Token.OP (sym)) sta
        | LexMatch @"^\[" (_,sta) -> retTag LBRA sta
        | LexMatch @"^\]" (_, sta) -> retTag RBRA sta
        | LexMatch "^#" (_,sta) -> retTag HASH sta
        | LexMatch "^!" (_,sta) -> retTag EXCL sta
        | LexMatch @"^\d+" (sym, sta) -> retTag (Token.OFFSET sym) sta
        | _ -> (None, lData)

    /// Repeatedly calls nextToken
    /// to do lexical analysis
    let tokenize str : Result<Token list, string>  =
        let rec tokenize' st : Token list =
            match st.Txt with
            | "" -> [END]
            | _ -> let nt,st' = nextToken st
                   match nt with
                   | None -> [ERROR(sprintf "LexerMatchFailed at: '%s'" st.Txt)]
                   | Some tok -> tok :: tokenize' st'
        
        tokenize' {Txt=str;Numb=0}
        |> (fun lst -> (lst, List.head (List.rev lst))) //retrieve last token, should be an error or end
        |> function
           | (_, Token.ERROR(s)) -> Error(s)
           | (lst, Token.END) -> Ok(lst)
           | (_, x) -> Error(sprintf "LexerMatchFailed with final token: %A, expected: ERROR or END" x)

    //here we use uint32 when we parse we will parse int32
    let allowedLiterals = 
        [0..2..30] 
        |> List.allPairs [0u..255u] 
        |> List.map (fun (lit,n) -> (lit >>> n) + (lit <<< 32-n), {K=lit; R=n/2; I=false})
        |> List.collect (fun (allowedLitUint32, literal) -> [(allowedLitUint32, literal) ; (~~~(allowedLitUint32), {literal with I=true})]) //generate mvn or arm compatible valid literals
        |> Map.ofList
    
    let litValue {K=k ; R=r; I=i} =
        let rotVal2 u r =
            let n = (r &&& 0xF)*2 //(keep n >= 0, % would not do this)
            let res = (u % 256u >>> n) ||| (u % 256u <<< 32 - n)
            match i with
            | true -> ~~~res    //invert result because this was originally the mvn compatible value
            | false -> res
        rotVal2 k r

    let makeLiteral (lit: string) = 
        let tryFind(num) = match (Map.tryFind (uint32(num)) allowedLiterals) with //direct conversion
                           | Some(x) -> Ok(x)
                           | None -> Error("LiteralParseError: Literal must be producible by rotating right an 8-bit word within a 32-bit word")
        try
            Ok(uint32(int32(lit)))
        with
        | :? System.FormatException -> Error("LiteralParseError: Literal must be a valid signed int32 number")
        | :? OverflowException -> Error("LiteralParseError: Literal must be a valid signed int32 number")
        | _ -> Error("LiteralParseError: Unknown errors during parsing function")
        |> function
           | Ok(x) -> tryFind(x) //(printfn "unsigned value is %A %A" x (tryFind(x))) ; 
           | Error(x) -> Error(x)

        // match Int32.TryParse(lit) with
        // | (true, num) -> tryFind(num)
        // | _ -> match Int32.TryParse(lit,NumberStyles.HexNumber,CultureInfo.CreateSpecificCulture("en-US")) with
        //        | (true, num) -> tryFind(num)
        //        | _ -> Error("LiteralParseError: Literal must be a valid signed int32 number or 32-bit hex")
        


    //call this validate and make
    let makeOperands (tokListR : Result<Token list, string>) : Result<Operand, string> =
        //only call this if valid general format
        let makeOperands' op1Str op2Str (addrType) (offStr : string option) : Result<Operand, string> =
            match offStr with
            | None -> match ((Map.tryFind op1Str regNames), (Map.tryFind op2Str regNames), addrType) with
                      | (Some(op1Reg), Some(op2Reg), NORM) -> Ok({Op1=op1Reg; Op2=op2Reg; OpAddr=NORM; OpOff=None})
                      | (_, _, _)-> Error("Unknown error at making operand")
            | Some(x) -> Error("not implemented")

        match tokListR with
        | Ok(tokList) -> match tokList with
                         | OP(op1) :: COMMA :: LBRA :: OP(op2) :: RBRA :: tail
                            -> match tail with
                               | [END] -> makeOperands' op1 op2 NORM None //good no offset
                               | COMMA :: HASH :: OFFSET(off) :: [END] -> makeOperands' op1 op2 POST (Some(off)) //post-indexed
                               | HASH :: _ -> Error("InvalidSyntaxError: Operands must be comma seperated")
                               | _ -> Error("InvalidSyntaxError: Incorrect order or too many args")
                         | OP(op1) :: COMMA :: LBRA :: OP(op2) :: COMMA :: HASH :: OFFSET(off) :: RBRA :: tail
                            -> match tail with
                               | [END] -> makeOperands' op1 op2 NORM (Some(off)) //good normal-index
                               | EXCL :: [END] -> makeOperands' op1 op2 PRE (Some(off)) //good
                               | _ -> Error("InvalidSyntaxError: Incorrect order or too many args")
                         | OP(_) :: LBRA :: OP(_) :: _ -> Error("InvalidSyntaxError: Operands must be comma seperated")
                         | _ -> Error("InvalidSyntaxError: Too few or too many arguments")
        | Error(x) -> Error(x)

    let parse (ld: LineData) : Result<Parse<Instr>,string> option =
        let operandString = ld.Operands
        //printfn "op-string: %A" operandString
        


        let parse' (instrC, (root,suffix,pCond)) =
            //this will only be called if opcode was valid
            
            //convert operands to strings using active pattern
            let oprTokens =
                tokenize (ld.Operands)

            let operands = makeOperands(oprTokens) 

            
            printfn "op-tokens: %A" oprTokens
            printfn "made operands operands: %A" operands
            //convert these to valid types 
            //convert whole thing to an instruction record
            //see ok { ... } aslast line
            
            //also have to validate suffix and operand from string to type!!
            //that shuld always work really
            
            // this does the real work of parsing
            // dummy return for now
            //printfn "data avail: %A %A" (ld) (root)
            Ok { PInstr={InsOpCodeRoot=OpLDR;InsOpCodeSuffix=NoneSuff;InsOperand={Op1=R0;Op2=R1;OpAddr=NORM;OpOff=None}}; PLabel = None ; PSize = 4u; PCond = pCond }

        Map.tryFind ld.OpCode opCodes
        //if found then send the above for parsing
        |> Option.map parse'



    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|)  = parse

////////////////////////////////////////////////////////////////////////////////////
//      Code defined at top level after the instruction processing modules
////////////////////////////////////////////////////////////////////////////////////
module CommonTop =

    open CommonLex
    open CommonData

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
        printfn "heeeey: %A" 123
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
        //|> printfn "val: %A"
        |> matchLine

