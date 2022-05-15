
(*
ARRAY is structure for simplifying calculations 
related to array.
It had 5 functions:
*)

structure ARRAY : sig
   exception EmptyArray
   val makemem:int->int list
   val update:int*int*int list ->int list
   val get_code:int*(int*int*int*int) list ->(int*int*int*int)
   val getmem:int*int list->int
   val len: (int*int*int*int) list ->int
  
end = struct
    exception EmptyArray
    (*
    Function len(L) :
        return size of list L
    *)
    fun len([])=0
       | len(x::xs)=1+len(xs);
  (*
    Function makemem(n):
        returns list of zeroes of length n.
  *)
   fun makemem(n:int)=
   let
     fun helper (L:int list,i:int)=
        if i<1 then L
        else helper(L@[0],i-1);
   in
     helper([],n)
   end
    (*
    Function update(n,a,L):
        return L after assigning L[n]=a
    *)
   fun update(n,a,L)=
     let fun helper(n,a,[],i)=[]
      | helper(n,a,x::xs,i)= if i=n then a::xs else x::helper(n,a,xs,i+1);
  in
     helper(n,a,L,0)
  end

  (*
    This Function return L[n] if exists;
  *)
  fun getmem(n,L)=
    let
       fun helper(n,[],i)=raise EmptyArray
    |  helper(n,x::xs,i)=if i=n then x else helper(n,xs,i+1);
    in 
       helper(n,L,0)
    end

    (*
      Returns L[n] from input list L
    *)
    fun get_code(n,L)=
    let
       fun helper(n,[],i)=raise EmptyArray
    |  helper(n,x::xs,i)=if i=n then x else helper(n,xs,i+1);
    in 
       helper(n,L,0)
    end

end ;



(*
_______________________________
-------------------------------
Data Structure for code reading
_______________________________
-------------------------------
*)
structure CODE : sig
  exception DividedByZero
  exception SyntaxError
  exception InvalidOperation
  exception InvalidInput
  val interpret_code:string list*int -> unit
end = struct
   exception DividedByZero
   exception SyntaxError
   exception InvalidOperation
   exception InvalidInput
  (*
  ................
  HELPER FUNCTIONS:
  ................
  *)
  fun getint(s:string)=     
    case Int.fromString(s) of
       SOME n=> (true,n)|
       NONE=>(false,0);

    (*
       detectextracomma(s,i) checks for double commas
    *)
    fun detectextracomma(s:string,i:int):bool=
    let
        val n=size(s)
    in
    if i<1 then true
    else if i=1 andalso substring(s,i,1)="," then false
    else if i=n-2 andalso substring(s,i,1)="," then false
    else if substring(s,i,1)="," andalso substring(s,i+1,1)="," then false
    else detectextracomma(s,i-1)
    end



    (*
      checkpattern returns true if
      st is of correct format.
    *)
    fun checkpattern(st:string):bool=
        let
            val length=size(st)
            fun check(s:string,t:int):bool=
                let
                    fun helper (s:string, i:int):bool =
                        let
                            val n=size(s)
                        in
                            if i>n orelse i<1 then
                                false
                            else if i=n andalso substring(s,i-1,1)=")" then 
                                true
                            else if i=1 andalso substring(s,i-1,1)="(" then 
                                true 
                            else if i=1 orelse i=n then 
                                false
                            else if substring(s,i-1,1)="," then
                                true
                            else if Int.fromString(substring(s,i-1,1))=NONE then 
                                false
                            else 
                                true
                        end
                in
                    if t=0 then true else if t>0 then helper(s,t) andalso check(s,t-1) else false
                end
        in 
            check(st,length)
        end


    (*
      Returns list of int if string s is of desired pattern;
    *)
    fun listFromString(s:string):int list=
        let
            val n=size(s)
            fun foo(a:int,st:string,i:int,count:int,L:int list,k:int): int list=
                let
                val (y,x)=getint(substring(st,i,count))
                in
                if a=k andalso y then L@[x]
                else if a=k then L
                else if substring(st,a,1)="," andalso y then foo(a+1,st,a+1,0,L@[x],k)
                else if substring(st,a,1)="," then foo(a+1,st,a+1,0,L,k)
                else foo(a+1,st,i,count+1,L,k)
                end
        in
        if checkpattern(s) andalso detectextracomma(s,n-1) then foo(1,s,1,0,[],n-1) else []
        end
    
    fun len([])=0
    |len(x::xs)=1+len(xs);

    fun isquadtuple(s:string)=if len(listFromString(s))=4 then true else false
    
    (*
    Returns tuple of four from input string 
    only if input string is also tuple of four 
    then output is string tuple 
    otherwise
    (0,0,0,0) is returned

    *)
    fun maketuple(s:string)=
    let
        fun helper([])=(0,0,0,0)
        | helper([x])=(0,0,0,0)
        | helper([x,y])=(0,0,0,0)
        | helper([x,y,z])=(0,0,0,0)
        | helper([a,b,c,d])=(a,b,c,d)
        | helper(a::b::c::d::e::xs)=(0,0,0,0)
    in
    helper(listFromString(s))
    end

    (* fun operater((a,b,c,d)) *)

    (*
    -----------------
    -----------------
    MAIN FUNCTIONS
    -----------------
    -----------------
    *)
  
  fun tuplelist([])=[]
    | tuplelist(x::xs)= if isquadtuple(substring(x,0,size(x)-1)) then maketuple(substring(x,0,size(x)-1))::tuplelist(xs)                            
                        else raise SyntaxError;

    (*
    ________
    --------
    OPERATER
    ________
    --------
    *)
  fun operate((a,b,c,k),mem)=
    let
       fun andop(m,n)=if m=1 andalso n=1 then 1 else 0;
       fun orop(m,n)=if m=0 andalso n=0 then 0 else 1;
       fun notop(m)=if m=1 then 0 else 1;
       fun eqlop(m:int,n:int)=if m<>n then 0 else 1;
       fun gtop(m,n)=if m>n then 1 else 0;
    in
      if a=2 then ARRAY.update(k,ARRAY.getmem(b,mem),mem)
      else if a=3 then ARRAY.update(k,notop(ARRAY.getmem(b,mem)),mem)
      else if a=4 then ARRAY.update(k,orop(ARRAY.getmem(b,mem),ARRAY.getmem(c,mem)),mem)
      else if a=5 then ARRAY.update(k,andop(ARRAY.getmem(b,mem),ARRAY.getmem(c,mem)),mem)
      else if a=6 then ARRAY.update(k,ARRAY.getmem(b,mem)+ARRAY.getmem(c,mem),mem)
      else if a=7 then ARRAY.update(k,ARRAY.getmem(b,mem)-ARRAY.getmem(c,mem),mem)
      else if a=8 then ARRAY.update(k,ARRAY.getmem(b,mem)*ARRAY.getmem(c,mem),mem)
      else if a=9 then ARRAY.update(k,ARRAY.getmem(b,mem) div ARRAY.getmem(c,mem),mem)
      else if a=10 then ARRAY.update(k,ARRAY.getmem(b,mem) mod ARRAY.getmem(c,mem),mem)
      else if a=11 then ARRAY.update(k,eqlop(ARRAY.getmem(b,mem),ARRAY.getmem(c,mem)),mem)
      else if a=12 then ARRAY.update(k,gtop(ARRAY.getmem(b,mem),ARRAY.getmem(c,mem)),mem)
      else if a=16 then ARRAY.update(k,b,mem)
      else mem
    end

    fun getNumber() = 
let
    val str = valOf (TextIO.inputLine TextIO.stdIn)
    val i : int = valOf (Int.fromString(str))
    in 
     i
end

fun printout(L:string list)=
   let
     fun helper(s,[])=s
       | helper(s,x::xs)=helper(s^x^"\n",xs)
   in
     helper("",L)
   end


   (*
     Input: L --> list of string --> each string represent one line of code.
            n --> int ---> size of memory.
   *)
   fun interpret_code(L,n)=
       let 
          val codes=tuplelist(L)
          val memory=ARRAY.makemem(n)
          fun helper(code_s,mem,index,L)=
             let
               val n=ARRAY.len(code_s)
               val (a,b,c,k)= ARRAY.get_code(index,code_s)
             in
               if index>n then L
               else if a=0 then L  
               else if a=1 then 
                    let 
                       val z=print("input: ")
                       val x= valOf(Int.fromString(valOf (TextIO.inputLine TextIO.stdIn)))
                       val y=operate((16,x,c,k),mem)
                    in
                      helper(code_s,y,index+1,L)
                    end
                else if a=13 andalso ARRAY.getmem(b,mem)=1 then helper(code_s,mem,k,L)
                else if a=13 then helper(code_s,mem,index+1,L)
                else if a=14 then helper(code_s,mem,k,L)
                else if a=15 then 
                  let
                        val x=Int.toString(ARRAY.getmem(b,mem))
                        val y=L@[x]
                  in
                    helper(code_s,mem,index+1,y)
                  end
                else if (a=9 orelse a=10 ) andalso ARRAY.getmem(c,mem)=0 then raise DividedByZero
                else if a=2 orelse a=3 orelse a=4 orelse a=5 orelse a=6 orelse a=7 orelse a=8 orelse a=9 orelse a=10 orelse a=11 orelse a=12 orelse a=16 then
                    helper(code_s,operate((a,b,c,k),mem),index+1,L)
                else raise InvalidOperation
             end
           val outstr= printout(helper(codes,memory,0,[]))
        in 
            print(outstr)
        end
end;

  

(* READS FILE AND RETURNS LIST OF LINE*)
(*<----------------------------------->*)
fun lick (filename:string) =
let val f = TextIO.getInstream(TextIO.openIn filename)
     fun loop (accum: string list, f) =
    case (TextIO.StreamIO.inputLine f) of 
    SOME(chunk, f') => loop (chunk::accum, f')
    | NONE => (TextIO.StreamIO.closeIn f; accum)
    (* esac *)
 in rev(loop ([], f))
 end
(*
_____________
------------->
INTERPRET 
------------->
______________
*)

fun interpret(filename:string)=
    let 
       val L=lick(filename)
    in
       CODE.interpret_code(L,16)
    end
