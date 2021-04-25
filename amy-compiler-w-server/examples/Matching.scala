object Matching {
  def matchingInt(i : Int) : Unit = {
      i % 7 match {
        case 0 => 
          Std.printString(Std.intToString(i) ++ " is a multiple of 7")
        case 2 => 
          Std.printString("When " ++ Std.intToString(i) ++ " is divided by 7, the rest is 2")
        case _ =>
          Std.printString("The given number is " ++ Std.intToString(i))
      }
    }

  matchingInt(9);
  matchingInt(13);
  matchingInt(21);
  matchingInt(28)

}