
(** 
 * 
 *  Parses command line arguments.  Allows both formats:
 *  
 *  command -arg x
 *  command -arg=x
 * 
 *  In the second example, the '=' must be contiguous with "-arg" and "x"
 * 
 *  It also allows aliases, so that, as in "grep", 
 *  -E and --extended-regexp are the same option.
 * 
 *  In this case, "extended-regexp" would be called the [name]
 *  and "E" the [alias].  All flag and param names must be distinct.
 *  A string specified as an alias can not also be a param or flag name.
 * 
 *  The package distinguishes between "flags" and "parameters",
 *  where flags take no arguments, while parameters take exactly
 *  1 argument.  Additionally, strings beginning with #"-" are not
 *  considered valid arguments to parameters.
 * 
 *  E.g., if the command line was command -a x -b y -c -d z v w
 * 
 *  parse_args {flags = ["c"],params = []} ~~> SOME [Param("a","x"),
 *                                       Param("b","y"),
 *                                       Flag "c",
 *                                       Param("d","z"),
 *                                       Undef "v",
 *                                       Undef "w"]
 * 
 * 
 *  parse_args {flags = ["c","d'],params = []} ~~> SOME [Param("a","x"),
 *                                           Param("b","y"),
 *                                           Flag "c",
 *                                           Flag "d",
 *                                           Undef "z",
 *                                           Undef "v",
 *                                           Undef "w"]
 * 
 * 
 *  whereas the erronous 
 * 
 *  parse_args {flags = []} ~~> NONE
 * 
 *  because in searching for the argument of "c", it found "-d".
 * 
 *  Note that if the flag list is not given, it is impossible to decide
 *  whether "-d z" is parsed as [Param("d","z")] or [Flag "d",Undef "z"].
 *  The default is to parse "-d z" as a param.  A param without an argument
 *  is an error.  [param_list] is given so that one may specify aliases. 
 *  You should only include the long names of the flags, not their aliases.
 * 
 *  Options with spaces should be enclosed in quotes: 
 * 
 *  command -line "To be or not to be,"
 * 
 *  The function [parse_check_args] checks the commandline for well-formedness.
 *  Each argument is given a [kind], indicating whether it is optional, and
 *  the number of such allowable arguments.  For instance, in some cases it makes sense
 *  to have one -file argument, say, for writing the output to a file, but not
 *  to multiple files.  Conversely, it might make sense to allow multiple input
 *  files (though of course this is application specific). 
 * 
 *  We raise an exception ([BadArgs]) rather than returning an option in this
 *  case, as we want the user to see the error where parsing or checking failed.    
 * 
 *)
signature CHECK_COMMANDLINE =
sig

exception BadArgs of string

(* -------------------------------  Aliases  -------------------------------- *)

type aliases 

val mkAliases: {fullname: string, alias: string} list -> aliases
val deAlias: string * aliases -> string option

(* --------------------------------  Flags  --------------------------------- *)

type flags

val mkFlags: string list -> flags

(* ---------------------------  Simple interface  --------------------------- *)

type args 

(**
 * parseArgs (flags,aliases)
 *
 * @exception BadArgs if a non-flag is followed by an option (e.g., -a -b)
 *            or if a non-flag has no arg
 *)
val parseArgs: flags * aliases -> string list -> args

val params: args -> (string * string) list
val flags : args -> string list
val undefs: args -> string list

(* --------------------------  Checked Interface  --------------------------- *)
                    
datatype kind = One
              | OneOrMore
              | ZeroOrOne
              | ZeroOrMore

type param = {name: string,
              kind: kind}

type checked_input = {flags: flags,
                      aliases: aliases,
                      params: param list,
                      undefs: int option}

type checked_args 

(* 
 * @exception BadArgs if a required argument is not received, 
 *            a singular argument occurs multiply, or the number
 *            of undefined arguments does not match the given 
 *            number. 
 *)
val parseCheckArgs: checked_input -> string list -> checked_args

val requiredParam: checked_input * checked_args * string -> string
val optionalParam: checked_input * checked_args * string -> string option
val requiredParams: checked_input * checked_args * string -> string list
val optionalParams: checked_input * checked_args * string -> string list 
val checkedFlags: checked_args -> string list 
val checkedUndefs: checked_args -> string list 

end
