clearEmpty :: [[a]] -> [[a]]
clearEmpty list = filter (\sublist -> not (null sublist)) list

merge :: [[a]] -> [a]
merge [] = []
merge list = pullHeadAndRemerge (clearEmpty list)
  where pullHeadAndRemerge list = map head list ++ merge (map tail list)

main = do
  print(merge [ [1, 2, 3], [], [4, 5, 6], [7, 8, 9, 10], [11], [12, 13] ])
