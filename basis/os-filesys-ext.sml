
structure OSFileSysExt: OS_FILE_SYS_EXT =
struct 

structure F = OS.FileSys

fun ls dir = 
    let
       val stm = F.openDir dir
       val fs = ref []
       val f = ref (F.readDir stm)
    in
       let in
          while !f <> NONE do
             let in 
                fs := (valOf (!f)) :: (!fs)
              ; f := F.readDir stm
             end
       end
     ; F.closeDir stm
     ; !fs
    end

fun dirs dir = 
    let
       val fs = ls dir
    in
       List.filter (fn x => F.isDir(dir ^ "/" ^ x)) fs
    end

fun fileExists s = F.access(s, [])

open F

end
