module ASM where

import Data.List (intercalate)

data StaticLocation = SL String
    deriving (Eq, Ord, Read)

instance Show StaticLocation where
    show (SL str) = "$" ++ str

data Reg = Rax | Rbx | Rcx | Rdx | Rsi | Rdi | R8 | R9 | R10 | R11 | Rsp | Rbp
    deriving (Eq, Ord, Read)

data AmdArg = AAReg Reg | AAMem Int | AAConst Int | AAConstStr StaticLocation
    deriving (Eq, Ord, Read)

data ASM = APush AmdArg
         | APop AmdArg
         | ANeg AmdArg
         | ASub AmdArg AmdArg
         | AAdd AmdArg AmdArg
         | AMul AmdArg AmdArg
         | ADiv AmdArg
         | AXor AmdArg AmdArg
         | AAnd AmdArg AmdArg
         | AOr AmdArg AmdArg
         | AMov AmdArg AmdArg
         | ACmp AmdArg AmdArg
         | ACdq
         | ARet
         | ALab String
         | AJmp String
         | AJmpNe String
         | AJmpE String
         | ALeave
         | ASetLt
         | ASetLe
         | ASetGt
         | ASetGe
         | ASetEq
         | ASetNe
         | ACall String
         | ALoad AmdArg AmdArg
         | ASave AmdArg AmdArg
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
    show R11 = "%r11"
    show Rsp = "%rsp"
    show Rbp = "%rbp"

instance Show AmdArg where
    show (AAReg r) = show r
    show (AAMem i) = let
        pos1 = show $ -(8 * (i + 1))
        pos2 = show $ (1-i) * 8 in
        case (i >= 0) of
            True -> pos1 ++ "(%rbp)"
            False -> pos2 ++ "(%rbp)"
    show (AAConst i) = "$" ++ (show i)
    show (AAConstStr str) = show str


-- show comma separated
scs :: [AmdArg] -> String
scs = (intercalate ", ") . (map show)

instance Show ASM where
    show (APush arg) = "pushq " ++ (show arg)
    show (APop arg) = "popq " ++ (show arg)
    show (ANeg arg) = "negq " ++ (show arg)
    show (ASub arg1 arg2) = "subq " ++ (scs [arg1, arg2])
    show (AAdd arg1 arg2) = "addq " ++ (scs [arg1, arg2])
    show (AMul arg1 arg2) = "imulq " ++ (scs [arg1, arg2])
    show (ADiv arg) = "idivq " ++ (show arg)
    show (AAnd arg1 arg2) = "andq " ++ (scs [arg1, arg2])
    show (AOr arg1 arg2) = "orq " ++ (scs [arg1, arg2])
    show (AMov arg1 arg2) = "movq " ++ (scs [arg1, arg2])
    show (ACmp arg1 arg2) = "cmpq " ++ (scs [arg1, arg2])
    show (AXor arg1 arg2) = "xorq " ++ (scs [arg1, arg2])
    show (ACdq) = "cdq"
    show (ARet) = "ret"
    show (ALab lname) = "." ++ lname ++ ":"
    show (AJmp lname) = "jmp ." ++ lname
    show (AJmpNe lname) = "jne ." ++ lname
    show (AJmpE lname) = "je ." ++ lname
    show (ALeave) = "leave"
    show (ASetLt) = "setl %al"
    show (ASetLe) = "setle %al"
    show (ASetGt) = "setg %al"
    show (ASetGe) = "setge %al"
    show (ASetEq) = "sete %al"
    show (ASetNe) = "setne %al"
    show (ACall fname) = "call " ++ fname
    show (ALoad arg1 arg2) = "movq " ++ "(" ++ (show arg1) ++ ")" ++  ", " ++ (show arg2)
    show (ASave arg1 arg2) = "movq " ++ (show arg1) ++ ", " ++ "(" ++ (show arg2) ++ ")"
