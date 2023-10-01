// Terminator Instructions - These instructions are used to terminate a basic
// block of the program.   Every basic block must end with one of these
// instructions for it to be a well formed basic block.
//
 FIRST_TERM_INST  ( 1)
HANDLE_TERM_INST  ( 1, Ret           , ReturnInst)
HANDLE_TERM_INST  ( 2, Br            , BranchInst)
HANDLE_TERM_INST  ( 3, Switch        , SwitchInst)
HANDLE_TERM_INST  ( 4, IndirectBr    , IndirectBrInst)
HANDLE_TERM_INST  ( 5, Invoke        , InvokeInst)
HANDLE_TERM_INST  ( 6, Resume        , ResumeInst)
HANDLE_TERM_INST  ( 7, Unreachable   , UnreachableInst)
HANDLE_TERM_INST  ( 8, CleanupRet    , CleanupReturnInst)
HANDLE_TERM_INST  ( 9, CatchRet      , CatchReturnInst)
HANDLE_TERM_INST  (10, CatchSwitch   , CatchSwitchInst)
HANDLE_TERM_INST  (11, CallBr        , CallBrInst) // A call-site terminator
  LAST_TERM_INST  (11)