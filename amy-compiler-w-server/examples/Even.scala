object Even{

  def isEven(n: Int): Boolean = {
    if(n < 0){
      go(-n, -n)
    } else {
      go(n, n)
    }
  }

  def go(n: Int, count: Int): Boolean = {
    if(count == 0){
      if(0 <= n){
        true
      } else {
        false
      }
    } else {
      go(-n, count-1)
    }
  } 

  Std.printBoolean(isEven(2));
  Std.printBoolean(isEven(3));
  Std.printBoolean(isEven(-1));
  Std.printBoolean(isEven(0));
  Std.printBoolean(isEven(-4))

}