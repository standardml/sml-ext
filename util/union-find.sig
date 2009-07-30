
signature IMPERATIVE_UNION_FIND =
   sig
      structure K: HASH_KEY
      type union
      val create: int -> union
      val find: union -> K.hash_key -> K.hash_key
      val union: union -> K.hash_key * K.hash_key -> unit
      val equal: union -> K.hash_key * K.hash_key -> bool
      val toList: union -> K.hash_key list list
   end

signature PERSISTENT_UNION_FIND =
   sig 
      structure K: ORD_KEY
      type union
      val empty: union
      val inDomain: union * K.ord_key -> bool
      val find: union -> K.ord_key -> K.ord_key
      val union: union -> K.ord_key * K.ord_key -> union
      val equal: union -> K.ord_key * K.ord_key -> bool
      val merge: union * union -> union
      val toList: union -> K.ord_key list list
   end

