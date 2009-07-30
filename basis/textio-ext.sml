
structure TextIOExt: TEXT_IO_EXT =
struct 

  structure T = TextIO

  fun printl x = print (x ^ "\n")

  fun outputl stm s = T.output(stm,s ^ "\n")

  fun readFile s = 
      let
        val str = T.openIn s
        val ret = T.inputAll str
      in
        T.closeIn str;
        ret
      end 

  fun writeFile {file,s} = 
      let
        val str = T.openOut file
      in
        T.output(str,s);
        T.closeOut str
      end

  fun appendFile {file,s} = 
      let
        val str = T.openAppend file
      in
        T.output(str,s);
        T.closeOut str
      end

  open T

end
