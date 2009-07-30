
signature OS_FILE_SYS_EXT =
sig

include OS_FILE_SYS

val fileExists : string -> bool

(*Get all the files in a directory *)
val ls : string -> string list

(*
 Get the immediate subdirectories of a directory.   
 Same as 
 find . -maxdepth 1 -mindepth 1 -type d 
 *)
val dirs : string -> string list

end
