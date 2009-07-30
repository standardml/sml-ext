
structure SmlExt'SmlNjLib =
   struct 
      functor OrdSetExtFn = OrdSetExtFn
      functor OrdMapExtFn = OrdMapExtFn
      functor BitSetExtFn = BitSetExtFn
      structure FifoExt = FifoExt
      structure AtomSplaySetExt = AtomSplaySetExt
      structure AtomSplayMapExt = AtomSplayMapExt 
      structure RandomExt = RandomExt
      structure HashTableExt = HashTableExt
   end

signature SmlExt'SmlNjLib'ORD_SET_EXT = ORD_SET_EXT
signature SmlExt'SmlNjLib'ORD_MAP_EXT = ORD_MAP_EXT
