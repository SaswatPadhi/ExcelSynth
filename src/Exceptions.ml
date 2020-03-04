(* raised by the frontend (SyGuS parser) *)
exception Parse_Exn of string

(* raised in case of logical errors that must be fixed *)
exception Internal_Exn of string

(* raised by the synthesizer *)
exception Enumeration_Exn of string

(* raised if there is no boolean function consistent with the given
   positive and negative examples. Possible in two situations:
     > a positive and negative example have the identical feature vector
     > there is no k-CNF formula (for some particular k being used)
       that distinguishes the positive and negative examples
*)
exception NoSuchFunction
exception NoMajorityType

(* raised by a postcondition to indicate
   that the given test input should be ignored *)
exception IgnoreTest