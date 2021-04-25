object Neg {
  def negme(x: Int) : Int = {
    x match {
      case 0 => 
        Std.printString("0 has no sign statement");
        0
      case _ => 
        Std.printString("my return value is the negation of your input: " ++ Std.intToString(-x));
        -x
    }

  }

  

  def divideme(x: Int, y: Int) : Int = {
    val ret: Int = x/y;
    Std.printString("The division of " ++ Std.intToString(x) ++ " by " ++ Std.intToString(y) ++ " gives " ++ Std.intToString(ret));
    ret
  }

  negme(5);
  negme(999);
  negme(303);
  negme(-303);
  negme(1000000);
  negme(negme(1));
  negme(0);

  divideme(6,5);
  divideme(78,4);
  divideme(64,2);
  divideme(56,8);
  //should fail and print "Trying to divide by 0"
  divideme(5,0)

}