object SimplifyList {

    def simplify(l: L.List) : L.List = {
        
        l match {
            case L.Cons(h1,t1) => 
                t1 match {
                    case L.Cons(h2,t2) =>
                        if(h1 == h2) {
                            simplify(L.Cons(h1, t2))
                        } else {
                            L.Cons(h1, simplify(t1))
                        }
                    case L.Nil() =>
                        l
                }
            case L.Nil() =>
                l
        } 
        
    }

    def beforeAfter(b: L.List, a: L.List): Unit = {
        Std.printString("The list before and after it was compressed : " ++ L.toString(b) ++ " / " ++ L.toString(a))
    }

    val l1: L.List = L.Cons(1, L.Cons(1, L.Cons(1, L.Cons(1, L.Cons(2, L.Cons(3, L.Cons(3, L.Cons(1, L.Cons(1, L.Cons(4, L.Cons(5, L.Cons(5, L.Cons(5, L.Cons(5, L.Nil()))))))))))))));
    val l2: L.List = L.Cons(1, L.Cons(1, L.Cons(2, L.Cons(3, L.Cons(3, L.Cons(3, L.Cons(3, L.Cons(1, L.Cons(4, L.Cons(5, L.Cons(2, L.Cons(2, L.Cons(3, L.Nil())))))))))))));
    val l3: L.List = L.Cons(5, L.Cons(2, L.Cons(2, L.Cons(4, L.Cons(3, L.Cons(3, L.Cons(6, L.Cons(6, L.Cons(6, L.Cons(6, L.Cons(6, L.Nil())))))))))));
    val l4: L.List = L.Cons(1, L.Cons(1, L.Cons(1, L.Cons(1, L.Cons(1, L.Nil())))));

   beforeAfter(l1, simplify(l1));
   beforeAfter(l2, simplify(l2));
   beforeAfter(l3, simplify(l3));
   beforeAfter(l4, simplify(l4))
}