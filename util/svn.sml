
structure Svn:> SVN =
struct 

structure T = TextIOExt
structure S = StringExt

fun revision () = 
    let
       val tempFile = "/tmp/___SMLSVN___"
       val syscall = "svn info | grep Revision"
       val _ = OS.Process.system (concat [syscall," > ",tempFile])
       val svn = T.readFile tempFile
       (* svn is of the form 'Revison: XXXX', e.g. 'Revison: 389' *)
       val ssize = S.size svn
       val rev = "Revision: "
       val rsize = S.size rev
       val svn' = S.substring(svn,0,rsize) (* may raise Subscript *)
    in
       if svn' <> rev then NONE else
       Int.fromString(S.substring(svn,rsize,ssize-rsize))
    end handle Subscript => NONE

fun date () = 
    let
       val tempFile = "/tmp/___SMLSVN___"
       val syscall = "svn info | grep Date"
       val _ = OS.Process.system (concat [syscall," > ",tempFile])
       val svn = T.readFile tempFile

       (* svn gives us *)
       (* Last Changed Date: 2007-09-12 19:48:30 -0400 (Wed, 12 Sep 2007) *)

       (* Date.fromString expects *)
       (* "Wed Sep 12 20:57:15 2007" *)

       val lcd = "Last Changed Date: "
       val lcdSize = S.size lcd
       val svn' = S.substring(svn,0,lcdSize) (* may raise Subscript *)
       val _ = if svn' <> lcd then raise Subscript else ()
       val time = 
           S.substring(svn,
                       S.size "Last Changed Date: 2007-09-12 ",
                       S.size "19:48:30")
       val year = 
           S.substring(svn,
                       S.size "Last Changed Date: 2007-09-12 19:48:30 -0400 (Wed, 12 Sep ",
                       S.size "2007")
       val date = 
           S.substring(svn,
                       S.size "Last Changed Date: 2007-09-12 19:48:30 -0400 (Wed, ",
                       S.size "12")
       val day = 
           S.substring(svn,
                       S.size "Last Changed Date: 2007-09-12 19:48:30 -0400 (",
                       S.size "Wed")
       val month = 
           S.substring(svn,
                       S.size "Last Changed Date: 2007-09-12 19:48:30 -0400 (Wed, 12 ",
                       S.size "Sep")
    in
       Date.fromString (S.concatWith " " [day,month,date,time,year])
    end handle Subscript => NONE

end
