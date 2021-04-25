object Trick71113 {
  def trick(hun: Int, dec: Int, unit: Int) : Unit = {
    if (hun < 0 || dec < 0 || unit < 0) {
      Std.printString("each value must be non-negative")
    } else {
      if (!(hun <= 9 && dec <= 9 && unit <= 9)) {
      Std.printString("each value must be the single digit of its according place")
      } else {
        val x: Int = hun * 100 + dec * 10 + unit;
        Std.printString("the number is " ++ Std.intToString(x) ++ " and written twice is " ++ Std.intToString(x) ++ Std.intToString(x));
        val x7: Int = x / 7;
        Std.printString("divide by 7 = " ++ Std.intToString(x7));
        val x117: Int = x7 / 11;
        Std.printString("divide further by 11 = " ++ Std.intToString(x117));
        val x13117: Int = x117 / 13;
        Std.printString("divide one last time by 13 = " ++ Std.intToString(x13117))
      }
    }
  }

  trick(0,0,0);
  trick(3,1,7);
  trick(9,0,9);
  trick(4,6,8);
  trick(1,2,3);
  trick(8,0,8);
  trick(3,0,3);
  trick(1,0,1);
  trick(6,0,6)
}
