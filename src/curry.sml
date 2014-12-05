fun exponent x 0 = 1
  | exponent x y = x * exponent x (y - 1);

print(Int.toString(exponent 2 1));
