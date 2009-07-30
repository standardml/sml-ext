
(* This is a combinator-style parser for the ML5 grammar.
   The combinator library used is in (SML-LIB)/parse/.

   This structure contains parsers from token streams into expressions (etc.)
   See tokenize.sml for the tokenizer.

   The parsers here are straightforward except for a few things:
      Some parsers have contexts. I use the context to track, for instance,
      the fixity of tokens. Thus, "infix x" inserts something into the context,
      and the pattern and app phrases make use of the context. The Initfix
      structure gives the default parsing context.

      The "import" keyword causes the target file to be read in,
      tokenized, and then prepended to the current input. 
*)

(* PERF: several phrases will parse, looking for a postfix operator
   (handle), and then parse again if that fails. Instead we should
   do an optional parse of the postfix phrase to avoid doubling of
   work. *)

structure Parse :> PARSE =
struct

  val root = (FSUtil.chdir_excursion 
              (CommandLine.name())
              (fn _ =>
               OS.FileSys.getDir ()))
(*               Posix.FileSys.getcwd ())) *)
    
  (* if running under NJ, default back to current dir, since
     we don't want to root ourselves at the sml/nj binary! *)
  val root = if Option.isSome (StringUtil.find "smlnj" root)
             then OS.FileSys.getDir () (* Posix.FileSys.getcwd () *)
             else root

  (* val () = print ("The root is: " ^ root ^ "\n") *)

  val ROOTMARKER = "#ROOT#"
               
  val rootp = Params.param root 
    (SOME ("-root",
           "path to humlock installation (if different " ^
           "from location of humlock binary)")) "rootp"

  val include_dirs = 
      Params.paramacc ["#ROOT#stdlib", "."]
                     (SOME ("-I",
                            "additional include directories", #",")) "include"

  fun incdirs () =
    map (fn s =>
         if StringUtil.matchhead ROOTMARKER s
         then 
           FSUtil.dirplus (!rootp) (String.substring(s, 
                                                     size ROOTMARKER,
                                                     size s - size ROOTMARKER))
         else s) (!include_dirs)

  open Tokens

  open Parsing

  open EL

  structure LU = ListUtil

  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard when
  infixr 1 ||

  exception Parse of string

  (* XXX idea *)
  (*
  infixr 0 `` 
  fun a `` b = a b
  fun ? s p x = p wth (fn y => (y, x)) || $(fn () => raise Parse s)
  *)

  exception Impossible 

  fun **(s, p) = p ## (fn pos => raise Parse ("@" ^ Pos.toString pos ^ ": " ^ s))
  infixr 4 **

  (* as `KEYWORD -- punt "expected KEYWORD KEYWORD2" *)
  fun punt msg _ = msg ** fail

  val namedstring = ML5pghUtil.newstr
  val itos = Int.toString

  (* look in every include path for this file *)
  fun tryopenwith func f =
      let
          fun one s =
              let val t = FSUtil.dirplus s f
              in
                  (* print ("Try '" ^ t ^ "'...\n"); *)
                  (SOME (func t))
              end
              handle _ => NONE
      in
          case List.mapPartial one (incdirs()) of
              nil => raise Parse (f ^ " not found in any include dir (" ^ 
                                  StringUtil.delimit "," (!include_dirs) ^ 
                                  ")")
            | [h] => h
            | h::_ =>
                  let in
                      (* XXX move to a 'warn' function *)
                      print ("WARNING: include file " ^ f ^
                             " found in multiple directories; " ^
                             "choosing arbitrarily.\n");
                      h
                  end
      end

  fun tryopen f = tryopenwith StreamUtil.ftostream f

  fun `w = satisfy (fn x => eq (x, w))

  fun ifmany f [x] = x
    | ifmany f l = f l

  val id = satisfy (fn ID s => true | _ => false) 
               wth (fn ID s => s | _ => raise Impossible)
      
  val world = id

  fun ptuple pl =
      PRecord(ListUtil.mapi 
              (fn (p, i) => 
               (Int.toString (i+1), p)) pl)

  val number = any when (fn INT i => SOME i | _ => NONE)

  (* labels are identifiers but also numbers *)
  val label = id || number wth (Int.toString o Word32.toInt)

  local 
      (* look for prodtypes separated by arrows.
         prodtypes are apptypes separated by *.
         apptypes are parenthesized, atomic, or applications. *)
      fun arrowtype () = separate ($prodtype) (`ARROW) wth LU.combiner TArrow

      and arrowtypes () = separate ($arrowtype) (`COMMA)

      and prodtype () = separate ($apptype) (`TIMES) wth ifmany
          (* create record 1:, 2:, ... *)
             (fn l => TRec (ListUtil.mapi 
                            (fn (t, n) => (itos (n+1), t)) l))

      and mostatomic () =
          (* PERF backtracking since world is id 
             (but lookahead is at most 1) *)
          alt [world << `ADDR wth TAddr,
               `UNIT return TRec nil,
               number wth (TNum o Word32.toInt),
               id wth TVar,
               (* XXX should allow specifying world var *)
               (`LBRACE && `RBRACE) >> $mostatomic wth (fn t => TSham(NONE, t)),
               (* don't allow empty record, because {} means shamrock. *)
               `LBRACE >> separate (label && (`COLON >> $arrowtype)) (`COMMA) << `RBRACE wth TRec,
               `LPAREN >> $arrowtype << `RPAREN]

      and postfixapps t =
          repeat id
          wth (foldl (fn (s, b) =>
                      TApp ([b], s)) t)

      and apptype () = 
          alt [(`LPAREN >> $arrowtype && 
                (`COMMA >> $arrowtypes << `RPAREN)) && id
               (* then perhaps some more postfix applications *)
               -- (fn ((t, tt), s) => postfixapps (TApp(t :: tt, s))),

               (* also includes mostatomic, not applied to anything *)
               $mostatomic -- postfixapps]
  in
      val attype = $mostatomic
      val typ = $arrowtype
  end

  fun ttoc (INT i) = SOME (CInt i)
    | ttoc (CHAR c) = SOME (CChar c)
    | ttoc (TEXT [STR s]) = SOME (CString s)
    | ttoc _ = NONE

  val constant = maybe ttoc

  fun isnonfix G s = not (LU.Alist.haskey (op= : string * string -> bool) G s)

  (* in expressions, we can use some tokens that have special
     meaning in types *)
  val expid = id || `TIMES return "*" || `ARROW return "->"

  (* nonfix identifiers, or any identifier prefixed with op *)
  fun fid G = expid suchthat (isnonfix G) || 
              `OP >> `EQUALS return "=" ||
              `OP >> expid

  fun exactid s = expid suchthat (fn ss => s = ss)

  (* should restrict to alphanum ids, add more *)
  val dbid = id || `OF return "of"

  (* crappy. *)
  fun call G parser = $(fn () => parser G)

  (* a pattern is irrefutable if it contains no constants or applications *)
  (* XXX actually, we should expand this notion to include applications
     of one-constructor datatypes. Then this needs to be moved into the
     elaborator, unless we go crazy enough to track bindings of datatype
     constructors, etc. (That might not be so bad. For one thing we need
     to rewrite non-carrying ctors to carry unit.) *)

  fun irrefutable PWild = true
    | irrefutable (PVar _) = true
    | irrefutable (PAs(_, p)) = irrefutable p
    | irrefutable (PRecord pl) = List.all (fn (_, p) => irrefutable p) pl
    | irrefutable (PConstrain (p, _, _)) = irrefutable p
    | irrefutable _ = false

  val tyvars = alt [id wth LU.list,
                    `LPAREN >> separate id (`COMMA) << `RPAREN]

  val datatypes = separate 
      (id && `EQUALS &&
       (separate0 (expid && opt (`OF >> typ)) (`BAR)) wth
       (fn (b, (_, c)) => (b, c))) (`AND)

  local
      (* used in parsing expressions, below *)

      fun fixcomp ((_,(p,_)), (_,(q,_))) = Int.compare (p, q)

      fun ifmanymark f [(x,_)] = x
        | ifmanymark f l = f l

      fun combinermark f [x] = x
        | combinermark f ((h,l)::t) = (f ((h,l), combinermark f t), l)
        | combinermark f nil = raise Impossible

      fun foldrmark f un nil = un
        | foldrmark f un ((h,l)::t) = (f ((h,l), foldrmark f un t), l)


      val stringlit = any when (fn TEXT [STR s] => SOME s | _ => NONE)


      val wk = `JAVASCRIPT return KJavascript ||
               `BYTECODE   return KBytecode

      (* pattern parsing is with respect to a fixity context. (string *
         (prec * status)) the fixity context is in sorted order by
         precedence (lower precedences near the head of the list).
         *)

      fun recordbind G =
        (label && opt (`EQUALS >> call G pat))
        wth (fn (l, SOME p) => (l, p)
              (* XXX only if l is also an identifier *)
              | (l, NONE) => (l, PVar l))

      and mapat G =
          alt [`UNDERSCORE return PWild,
               constant wth PConstant,
               `LBRACE >> separate0 (call G recordbind) (`COMMA) << `RBRACE wth PRecord,
               fid G wth PVar,
               `LPAREN >> separate0 (call G pat) (`COMMA) << `RPAREN wth ifmany
                  (fn pl => PRecord(ListUtil.mapi 
                                    (fn (p, i) => 
                                     (Int.toString (i+1), p)) pl))]

      and appat G =
          alt [fid G && call G appat wth (fn (a, b) => PApp(a, SOME b)),
               (`LPAREN >> call G exp << `RPAREN) && call G appat wth PWhen,
               call G mapat]

      and infixpat G =
          let
              val par =
                  alt [expid when (LU.Alist.get op= G) 
                         wth (fn (s,(prec, ass)) => 
                              Opr(Infix(ass, prec, 
                                        (fn (x,y) => 
                                         PApp(s, SOME (PRecord[("1", x),
                                                               ("2", y)])))))),
                       call G appat wth Atm]
          in
              parsefixity par
          end

      and aspat G =
          alt [fid G && `AS && call G aspat wth (fn (i,(_,p)) => PAs(i, p)),
               call G infixpat]

      and pat G = 
          call G aspat && opt (`COLON >> typ && opt (`AT >> world))
               wth (fn (p, SOME (t, wo)) => PConstrain(p, t, wo)
                     | (p, NONE) => p)


      (* ------------- expressions ------------- *)

      and atomexp G =
          alt [fid G wth Var,
               constant wth Constant,

               (`LETCC >> fid G) && (`IN >> separate (call G exp) (`SEMICOLON) << `END)
               wth (fn (v, es) =>
                    Letcc(v, combinermark Seq es)),

               (* Intended only for stdlib use *)
               `PRIMAPP >> fid G && 
                 (opt (`LBRACE >> separate0 typ (`COMMA) << `RBRACE)) &&
                 `LPAREN >> separate0 (call G exp) (`COMMA) << `RPAREN
                     wth (fn (f, (ts, args)) => Primapp (f, 
                                                         (case ts of NONE => nil | SOME l => l), 
                                                         args)),

               `PRIMAPP -- punt "expected ID LBRACE TYPES RBRACE LPAREN VALS RPAREN after PRIMAPP",

               `LET >> "expected DECS after LET" **
                   (call G decs -- 
                    (fn (G,ds) => 
                     `IN >> "expected EXP after IN" **
                     (separate (call G exp) (`SEMICOLON) << `END
                      wth (fn es => #1 (foldrmark Let 
                                        (combinermark Seq es) ds))))),

               (* text is a little tricky because it contains
                  nested streams of tokens *)
               (!! any) when (fn (TEXT tl, pos) =>
                         SOME 
                         let
                             val l = map (fn STR s => (Constant(CString s))
                                          |  EXP s =>
                                          case Stream.tolist
                                                (Parsing.transform
                                                 (call G exp) s) of
                                             [(e,_)] => e
                                           | _ => raise Parse ("bad expression " ^
                                                               "inside text"))
                                          tl
                         in
                             Jointext (map (fn x => (x, pos)) l)
                         end
                          | _ => NONE),

               (* PERF should roll the following two into one *)
               (`LBRACE && `BAR) >> separate0 (call G exp) (`COMMA) << (`BAR && `RBRACE) wth Vector,

               `LBRACE >> "expected (LABEL = EXP,)s after LBRACE" **
                    (separate0 (label && (`EQUALS >> call G exp)) (`COMMA) 
                       << `RBRACE wth Record),

               (* datafile expects a string literal next *)
               `DATAFILE >> "expected STRING LITERAL after DATAFILE" **
                            (stringlit
                                    wth (fn s =>
                                         let
                                             val dat = 
                                               tryopenwith StringUtil.readfile s
                                         in
                                             Constant(CString dat)
                                         end)),


               (* parens can be a parenthesized expression,
                  a tuple expression (= record),
                  or a sequenced expression. Putting them
                  all together makes for an efficient parser! *)
               (`LPAREN && ` RPAREN) return (Record nil),


               `LPAREN >> (call G exp) && 
                           opt (alt [`SEMICOLON >> 
                                        separate (call G exp) (`SEMICOLON)
                                        wth Util.A,
                                     `COMMA >> separate (call G exp) (`COMMA)
                                        wth Util.B]) << `RPAREN
                    wth (fn (e, NONE) => #1 e
                          | (e, SOME(Util.A el)) => 
                                   #1 (combinermark Seq (e::el))
                          | (e, SOME(Util.B el)) => 
                                Record(ListUtil.mapi 
                                        (fn (e, i) => 
                                         (Int.toString (i + 1), e)) (e::el)))]

      and appexp G =
          let
              fun mkinfix (s, x as (_,l), y) = 
                  (App((Var s,l), (Record[("1",x),("2",y)],l)),l)
              fun mark ass prec f = 
                  Opr(Infix(ass, prec, (fn (x as (_,l), y) => (f(x,y),l))))

              val par =
                  alt [expid when (LU.Alist.get op= G)
                         wth (fn (s,(prec, ass)) =>
                              Opr(Infix(ass, prec, (fn (x as (_,l),y) => 
                                                    mkinfix (s, x, y))))),
                       `EQUALS return mark Non 1 (#1 o (fn (e1, e2) =>
                                                  mkinfix ("=", e1, e2))),
                       `ANDALSO return mark Right ~100 Andalso,
                       `ORELSE return mark Right ~200 Orelse,
                       `ANDTHEN return mark Right ~300 Andthen,
                       `OTHERWISE return mark Right ~300 Otherwise,
                       !!(call G atomexp) wth Atm]
          in
              parsefixityadj par Left (fn (a,b as (_,l)) =>
                                       (App (a, b),l)) wth #1
          end

      and handlexp G =
          !! (call G appexp) && opt (`HANDLE && call G matching)
                     wth (fn (a,SOME (_,m)) => Handle (a, m)
                           | ((a,_), NONE) => a)

      and matching G = separate0 (call G pat && `DARROW && call G exp wth 
                                  (fn (a,(_,c)) => (a,c))) (`BAR) 

      (* XXX use repeat to allow e : t : t : t *)
      and constrainexp G =
          !! (call G handlexp) && opt (`COLON >> typ && opt (`AT >> world))
                     wth (fn (a,SOME(c, wo)) => Constrain (a, c, wo)
                           | ((a,_),NONE) => a)

      and exp G = 
          (* can only write cases with one object, though the
             ast allows multiple *)
          !!( alt [`CASE >> "expected EXP OF MATCHING after CASE" **
                   (call G exp && `OF && call G matching
                      wth (fn (obj,(_,pel)) => Case([obj], 
                                                    map (fn (p,e) =>
                                                         ([p], e))
                                                    pel,
                                                    NONE))),
                      (* generalize these *)
                   `RAISE >> call G exp wth Raise,
                   `SAY >> 
                      (opt (`LBRACE >> 
                           separate (label && opt (`EQUALS >> id) 
                                     wth (fn (l, NONE) => (l, l)
                                           | (l, SOME v) => (l, v))) (`COMMA)
                           << `RBRACE) 
                        wth (fn NONE => nil
                              | SOME l => l))
                      && call G exp wth Say,
                   `SAY -- punt "expected EXP after SAY",

                   `HOLD >> call G exp wth Hold,
                   `HOLD -- punt "expected EXP after HOLD",

                   (* XXX should allow specifying the world var *)
                   `SHAM >> call G exp wth (fn e => Sham(NONE, e)),
                   `SHAM -- punt "expected EXP after SHAM",

                   `FROM >> (* "expected EXP GET EXP after FROM" ** *)
                   (call G exp &&
                    `GET && (* "expected EXP after GET" **  *) call G exp)
                   wth (fn (addr, (_, bod)) => Get(addr, bod)),

                   (* XXX should instead rewrite this to a function in place
                          so that we don't HAVE to write (#l/t e) *)

                   `HASH >> label && `DIVIDE && attype && call G exp 
                      wth (fn (i, (_, (t, e))) => Proj(i, t, e)),

                   (`THROW >> call G exp) && (`TO >> call G exp) wth Throw,

                   `IF >> "expected EXP THEN EXP ELSE EXP after IF" **
                   (call G exp && 
                    `THEN && "expected EXP after THEN" ** call G exp && 
                    `ELSE && "expected EXP after ELSE" ** call G exp 
                      wth (fn (e as (_,l),(_,(t,(_,f)))) => If (e,t,f))),

                   !!(`FN) && (separate0 (repeat1 (call G mapat) && 
                                          (`DARROW return NONE) && 
                                          call G exp) (`BAR))
                      wth (fn ((_, l), s) => 
                           let val v = namedstring "anonfn"
                           in Let ((Fun { inline = false, funs = [(nil, v, map flat3 s)] }, l), 
                                   (Var v, l))
                           end),
                   call G constrainexp])

      and funclause G =
          repeat1 (call G mapat) && opt (`COLON >> typ) && `EQUALS && 
           call G exp
           wth (fn (pats, (to, (_, e))) => (pats, to, e))

      and onefun' G =
          expid -- (fn f => separate0 (call G funclause) 
                    (`BAR >> expid suchthat (fn x => x = f)) wth 
                    (fn a => (f, a)))

      and onefun G =
          alt [(`LPAREN >> separate id (`COMMA) << `RPAREN) && call G onefun' 
                   wth (fn (tv, (f, clauses)) => (tv, f, clauses)),
               call G onefun' wth (fn (f, clauses) => (nil, f, clauses))]

      and funs G =
          separate (call G onefun) (`AND)

      and dec G = call G regulardec wth Util.A
               || call G infixdec wth Util.B

      (* after processing a dec, either use the new fixity context or
         place the dec on the accumulator *)
      and postdecs G (Util.A d) = call G decs -- 
                                    (fn (G,ds) => succeed (G, d :: ds))
        | postdecs _ (Util.B G) = call G decs

      and decs G = alt [`IMPORT >> (stringlit
                                    -- (fn s =>
                                         let
                                             (* XXX error messages *)
                                             fun tokenize f = 
                                                 Parsing.transform 
                                                   Tokenize.token 
                                                    (Pos.markstreamex f 
                                                      (tryopen f))
                                         in
                                             push (tokenize s)
                                              (call G decs)
                                              (* (call G dec -- postdecs G) *)
                                         end)),
                        call G dec -- postdecs G,
                        succeed (G,nil)]

      and infixdec G =
          alt [(`INFIX || `INFIXR) && opt 
               (any when (fn INT i => 
                          if i >= 0w0 andalso i <= 0w9
                          then SOME i 
                          else NONE | _ => NONE)) && expid 
                 wth (fn (fx, (preco, i)) => 
                      LU.Sorted.insert fixcomp G (i, (Word32.toIntX
                                                      (Option.getOpt 
                                                      (preco, 0w4)), 
                                                      case fx of 
                                                          INFIX => Left 
                                                        | _ => Right))),
               `NONFIX >> expid wth (fn i => List.filter 
                                     (fn (j,_) => i <> j) G)
                 ]

      and bindword () = (`VAL return Val ||
                         `PUT return Put
                         (* `LETA return Leta || 
                         `LETSHAM return Letsham *))

      and regulardec G =
         !!(alt [$bindword && (call G pat suchthat irrefutable) && `EQUALS && 
                   call G exp
                   wth (fn (b, (pat, (_, e))) => Bind(b, nil, pat, e)),
                 $bindword && tyvars && (call G pat suchthat irrefutable) && 
                   `EQUALS && call G exp
                   wth (fn (b, (tv, (pat, (_, e)))) => Bind(b, tv, pat, e)),

                 $bindword -- punt "expected bind declaration after VAL/PUT",

                 (`LETSHAM >> id) && (`EQUALS >> call G exp)
                    wth (fn (id, e) => Letsham (nil, id, e)),
                 (`LETSHAM >> tyvars && id) && (`EQUALS >> call G exp)
                    wth (fn ((tv, id), e) => Letsham (tv, id, e)),
                 `LETSHAM -- punt "expected letsham declaration after LETSHAM",

                 (`LETA >> id) && (`EQUALS >> call G exp)
                    wth (fn (id, e) => Leta (nil, id, e)),
                 (`LETA >> tyvars && id) && (`EQUALS >> call G exp)
                    wth (fn ((tv, id), e) => Leta (tv, id, e)),
                 `LETA -- punt "expected leta declaration after LETA",

                 `EXTERN >> `VAL >> alt[tyvars && id,
                                        succeed nil && id]
                   && alt [`COLON >> typ && `AT && world wth (fn (t, (_, w)) => (t, Modal w)),
                           `COLON -- punt "expected TYP AT WORLD after COLON",
                           `TILDE >> (id && (`DARROW >> typ) wth (fn (wv, t) => (t, Valid wv))
                                      || typ wth (fn t => (t, Valid (namedstring "ev_wv_unused"))))
                           ]
                   && opt (`EQUALS >> id)
                   
                   wth (fn ((tv, i), ((t, m), ol)) => ExternVal(tv, i, t, m, ol)),

                 (`EXTERN >> wk) && (`WORLD >> id) wth ExternWorld,

                 `EXTERN >> `TYPE >> alt[tyvars && id,
                                         succeed nil && id] 
                                  && opt (`EQUALS >> id)
                 wth (fn ((sl, s), so) => ExternType (sl, s, so)),

                 `EXTERN -- punt "expected VAL, TYPE, or <kind> WORLD after EXTERN",

                 `DO >> "expected EXP after DO" ** 
                   (call G exp wth Do),

                 `TYPE >> id && `EQUALS && typ wth (fn (i,(_,t)) => 
                                                    Type (nil,i,t)),
                 `TYPE >> tyvars && id && `EQUALS && typ 
                   wth (fn (tv,(i,(_,t))) => Type(tv,i,t)),

                 `TYPE -- punt "expected type declaration after TYPE",

                 `TAGTYPE >> id wth Tagtype,
                 `TAGTYPE -- punt "expected ID after TAGTYPE",

                 `NEWTAG >> expid && opt (`OF >> typ) && `IN && id
                   wth (fn (i,(to,(_,ty))) => Newtag (i, false, to, ty)),
                 `NEWVTAG >> expid && opt (`OF >> typ) && `IN && id
                   wth (fn (i,(to,(_,ty))) => Newtag (i, true, to, ty)),

                 `NEWTAG -- punt "expected ID (OF TYP) IN ID after NEWTAG",
                 `NEWVTAG -- punt "expected ID (OF TYP) IN ID after NEWVTAG",

                 `EXCEPTION >> expid && opt (`OF >> typ) wth (fn (i, to) => Exception(i, false, to)),
                 `VEXCEPTION >> expid && opt (`OF >> typ) wth (fn (i, to) => Exception(i, true, to)),

                 `EXCEPTION -- punt "expected ID (OF TYP) after EXCEPTION",
                 `VEXCEPTION -- punt "expected ID (OF TYP) after VEXCEPTION",

                 `DATATYPE >> "expected DATATYPES after DATATYPE" **
                   alt [tyvars && datatypes wth Datatype,
                        datatypes wth (fn d => Datatype(nil, d))],
                 `FUN >> opt (`INLINE) && 
                   call G funs wth (fn (inl, fs) => Fun { inline = Option.isSome inl,
                                                          funs = fs }),
                 `FUN -- punt "expected (INLINE) FUNS after FUN"
                   ])

      fun export G =
        alt [`EXPORT >> `TYPE >> alt[tyvars && id, succeed nil && id]
             && opt(`EQUALS >> typ) wth (fn ((atv,i),to) => ExportType (atv, i, to)),
             (* XXX should support type annotation? *)
             `EXPORT >> `VAL >> alt[tyvars && id, succeed nil && id]
             && opt(`EQUALS >> call G exp) wth (fn ((atv,i),eo) => ExportVal (atv, i, eo)),
             
             `EXPORT -- punt "expected WORLD, TYPE, or VAL after EXPORT"]
             

      fun unit G =
        alt [`UNIT >> "expected DECS after UNIT" **
             (call G decs -- 
              (fn (G,ds) => 
               alt
               [`IN >> "expected EXPORTS after IN" **
               (repeat (call G export) << `END
                wth (fn es => Unit(ds, es))),
                (* don't require IN..END if there
                   are no exports (since we don't
                   have separate compilation yet anyway,
                   there is no reason to have them) *)
                `END return Unit(ds, nil),
                any -- punt "expected IN or END after UNIT DECS"]
               )),

             any -- punt "expected UNIT decs IN exports END"]

  in
      val pat = fn G => call G pat
      val atpat = fn G => call G mapat

      val exp = fn G => call G exp
      val dec = fn G => call G dec
      val unit = fn G => call G unit
  end

end
