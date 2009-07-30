
structure AtomSplaySetExt = OrdSetExtFn(struct 
                                           type ord_key = Atom.atom
                                           val compare = Atom.compare
                                           val ppItem = PP.string o Atom.toString
                                        end)

structure AtomSplayMapExt = OrdMapExtFn(struct 
                                           type ord_key = Atom.atom
                                           val compare = Atom.compare
                                        end)
