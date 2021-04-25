object change {

    def countChange(money: Int, coins: L.List): Int = {
        if (money == 0) { 1 }
        else {
            coins match {
                case L.Cons(h, t) =>
                    if (0 < money) {
                        countChange(money - h, coins) + countChange(money, t)
                    } else {
                        0
                    }
                case L.Nil() => 0
            }       
        }
    }

    val l: L.List = L.Cons(2, L.Cons(1, L.Nil()));
    Std.printInt(countChange(4, l))

}