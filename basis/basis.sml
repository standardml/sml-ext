
signature BASIS =
   sig
      structure GeneralExt: GENERAL_EXT
      structure StringExt: STRING_EXT
      structure OptionExt: OPTION_EXT
      structure BoolExt: BOOL_EXT
      structure ListExt: LIST_EXT
      structure ArrayExt: ARRAY_EXT
      structure VectorExt: VECTOR_EXT
      structure Fun: FUN
      structure Pair: PAIR
      structure Ref: REF
      structure TextIOExt: TEXT_IO_EXT
      structure OSFileSysExt: OS_FILE_SYS_EXT
      structure IntExt: INT_EXT
      structure Order: ORDER
      structure RealExt: REAL_EXT
      structure IEEERealExt: IEEE_REAL_EXT
      structure TimerExt: TIMER_EXT
      structure MathExt: MATH_EXT
   end

structure SmlExt'Basis : BASIS =
   struct 
      structure GeneralExt = GeneralExt
      structure StringExt = StringExt
      structure OptionExt = OptionExt
      structure BoolExt = BoolExt
      structure ListExt = ListExt
      structure ArrayExt = ArrayExt
      structure VectorExt = VectorExt
      structure Fun = Fun
      structure Pair = Pair
      structure Ref = Ref
      structure TextIOExt = TextIOExt
      structure OSFileSysExt = OSFileSysExt
      structure IntExt = IntExt
      structure Order = Order
      structure RealExt = RealExt
      structure IEEERealExt = IEEERealExt
      structure TimerExt = TimerExt
      structure MathExt = MathExt
   end 

