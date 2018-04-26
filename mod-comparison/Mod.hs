-- mod（剰余）の再定義
mymod :: (Integer, Integer) -> Integer
mymod(a, b)
  | a < b = a
  | otherwise = mymod(a - b, b)

main = do
  let a = 25
      b = 7
  print (mymod(a, b))
  print (mymod(100, b))
