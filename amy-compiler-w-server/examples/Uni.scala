object Uni {

    abstract class Student
    case class Bachelor(name: String, sciper: Int, grades: L.List) extends Student
    case class Master(name: String, sciper: Int, onExchange: Boolean) extends Student

    def check(std: Student): Unit = { 
            std match {
            case Bachelor(n, 111111, _) => Std.printString(n ++ " is 1st year student.")
            case Bachelor(n, _, L.Nil()) => Std.printString(n ++ " has no grades yet.")
            case Bachelor(n, _, l) => Std.printString(n ++ " has grades: " ++ L.toString(l))
            case Master(n, _, e) => Std.printString(n ++ " is on exchange: " ++ Std.booleanToString(e))
            case _ => Std.printString("No match.")
        }
    }

    val first: Student = Bachelor("Pascal", 111111, L.Nil());
    val second: Student = Bachelor("Paul", 222222, L.Nil());
    val third: Student = Bachelor("Anna", 333333, L.Cons(5, L.Cons(-5, L.Cons(-1, L.Cons(0, L.Cons(10, L.Nil()))))));
    val fourth: Student = Master("Hanna", 444444, false);

    check(first);
    check(second);
    check(third);
    check(fourth)
}





