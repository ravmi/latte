module ASM where

import Data.List (intercalate)

data Reg = Rax | Rbx | Rcx | Rdx | Rsi | Rdi | R8 | R9 | R10 | R11 | Rsp | Rbp
    deriving (Eq, Ord, Read)

data AmdArg = AAReg Reg | AAMem Int | AAConst Int
    deriving (Eq, Ord, Read)

data ASM = APush AmdArg
         | APop AmdArg
         | ANeg AmdArg
         | ASub AmdArg AmdArg
         | AAdd AmdArg AmdArg
         | AMul AmdArg AmdArg
         | ADiv AmdArg
         | AAnd AmdArg AmdArg
         | AOr AmdArg AmdArg
         | AMov AmdArg AmdArg
         | ACmp AmdArg AmdArg
         | ACdq
         | ARet
         | ALab String
         | AJmp String
         | AJmpNe String
         | ALeave
         | ASetLt
         | ASetLe
         | ASetGt
         | ASetGe
         | ASetEq
         | ASetNe
         | ACall String
          deriving (Eq, Ord, Read)

workingRegisters = [Rbx, Rcx, Rsi, Rdi, R8, R9, R10, R11]

instance Show Reg where
    show Rax = "%rax"
    show Rbx = "%rbx"
    show Rcx = "%rcx"
    show Rdx = "%rdx"
    show Rsi = "%rsi"
    show Rdi = "%rdi"
    show R8 = "%r8"
    show R9 = "%r9"
    show R10 = "%r10"
    show Rsp = "%rsp"
    show Rbp = "%rbp"

instance Show AmdArg where
    show (AAReg r) = show r
    show (AAMem i) = result where
        position = show $ -(8 * (i + 1))
        result = position ++ "(%rbp)"
    show (AAConst i) = "$" ++ (show i)


-- show comma separated
scs :: [AmdArg] -> String
scs = (intercalate ", ") . (map show)

instance Show ASM where
    show (APush arg) = "pushq " ++ (show arg)
    show (APop arg) = "popq " ++ (show arg)
    show (ANeg arg) = "negq " ++ (show arg)
    show (ASub arg1 arg2) = "subq " ++ (scs [arg1, arg2])
    show (AAdd arg1 arg2) = "addq " ++ (scs [arg1, arg2])
    show (AMul arg1 arg2) = "mulq " ++ (scs [arg1, arg2])
    show (ADiv arg) = "divq " ++ (show arg)
    show (AAnd arg1 arg2) = "andq " ++ (scs [arg1, arg2])
    show (AOr arg1 arg2) = "orq " ++ (scs [arg1, arg2])
    show (AMov arg1 arg2) = "movq " ++ (scs [arg1, arg2])
    show (ACmp arg1 arg2) = "cmpq " ++ (scs [arg1, arg2])
    show (ACdq) = "cdq"
    show (ARet) = "ret"
    show (ALab lname) = "." ++ lname ++ ":"
    show (AJmp lname) = "jmp ." ++ lname
    show (AJmpNe lname) = "jne ." ++ lname
    show (ALeave) = "leave"
    show (ASetLt) = "setl %al"
    show (ASetLe) = "setle %al"
    show (ASetGt) = "setg %al"
    show (ASetGe) = "setge %al"
    show (ASetEq) = "sete %al"
    show (ACall fname) = "call " ++ fname
