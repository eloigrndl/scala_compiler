object pascal {
    
    def pascal(c: Int, r: Int): Int = {
        if (c <= r) {
            if (c == 0 || c == r) {1}
            else {
                pascal(c - 1, r - 1) + pascal(c, r - 1)
            }
        } else {
            Std.printString("c (# of columns) must be smaller or equal than r (# of rows)");
            0
        }
    }

    Std.printInt(pascal(10, 10));
    Std.printInt(pascal(4, 9));
    Std.printInt(pascal(9, 4))
    
}