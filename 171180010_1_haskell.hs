import Data.List
trueKontrol :: [Int] -> Int -> [Int] -> Int -> Int -> Int -> Int ->Int->Int-> [Int]
trueKontrol burstTime q copyBt t wt1 wt2 wt3 j n = 
 if j< n
       then for1 burstTime q copyBt t wt1 wt2 wt3 j (0) n
     else yazdir  t wt1 wt2 wt3

for1 :: [Int] -> Int -> [Int] -> Int -> Int -> Int -> Int -> Int -> Int->Int->[Int]
for1 burstTime q copyBt t wt1 wt2 wt3 j i n = do
 if i< length burstTime
      then if burstTime!!i > q
              then findTime1 burstTime q copyBt t wt1 wt2 wt3 j i n
            else findTime2 burstTime q copyBt t wt1 wt2 wt3 j i n
   else trueKontrol burstTime q copyBt t wt1 wt2 wt3 (j+1) n  
findTime1 :: [Int] -> Int -> [Int] -> Int -> Int -> Int -> Int -> Int -> Int ->Int-> [Int]
findTime1 burstTime q copyBt t wt1 wt2 wt3 j i n= for1 (islem1 burstTime q i) q copyBt (t+q) wt1 wt2 wt3 j (i+1) n


islem1 :: [Int] -> Int -> Int -> [Int]
islem1 burstTime q i = case i of
                            0 -> [((burstTime!!0)-q),(burstTime!!1),(burstTime!!2)]
                            1 -> [(burstTime!!0),((burstTime!!1)-q),(burstTime!!2)]
                            2 -> [(burstTime!!0),(burstTime!!1),((burstTime!!2)-q)]
                            _  -> [(burstTime!!0),(burstTime!!1),(burstTime!!2)]
findTime2 :: [Int] -> Int -> [Int] -> Int -> Int -> Int -> Int -> Int -> Int ->Int-> [Int]
findTime2 burstTime q copyBt t wt1 wt2 wt3 j i n= 
 if i==0 && wt1/=0
     then for1 (islem2 burstTime 0) q copyBt (t+burstTime!!0) wt1 wt2 wt3 j (i+1) n
     else if (i==0) && (wt1==0)
       then for1 (islem2 burstTime 0) q copyBt (t+burstTime!!0) (t+burstTime!!0 - copyBt!!0) wt2 wt3 j (i+1) n
       else if (i==1) && (wt2/=0)
           then for1 (islem2 burstTime 1) q copyBt (t+burstTime!!1) wt1 wt2 wt3 j (i+1) n
           else if (i==1) && (wt2==0)
             then for1 (islem2 burstTime 1) q copyBt (t+burstTime!!1) wt1 (t+burstTime!!1 - copyBt!!1) wt3 j (i+1) n
             else if (i==2) && (wt3==0)
               then for1 (islem2 burstTime 2) q copyBt (t+burstTime!!2) wt1 wt2 (t+burstTime!!2 - copyBt!!2) j (i+1) n
               else for1 (islem2 burstTime 2) q copyBt (t+burstTime!!2) wt1 wt2 wt3 j (i+1) n
               
islem2 :: [Int] -> Int -> [Int]
islem2 burstTime i = case i of
                          0 -> [(0),(burstTime!!1),(burstTime!!2)]
                          1 -> [(burstTime!!0), (0), (burstTime!!2)]
                          2 -> [(burstTime!!0),(burstTime!!1),(0)]
                          _ -> [0,0,0]
                          
yazdir :: Int -> Int -> Int -> Int -> [Int]
yazdir t wt1 wt2 wt3 = t:wt1:wt2:wt3:[]  
main :: IO()
main =do
  let burstTime=[12,7,10]
  let copyBt=[12,7,10]
  let wt1=0
  let wt2=0
  let wt3=0
  let t=0
  let q=2
  let larger = maximum copyBt
  let n= (larger `div` q) + (larger `mod` q)
  let waitingTime=trueKontrol burstTime q copyBt t wt1 wt2 wt3 0 n
  putStr "Burst Time 1:  "
  print $ burstTime!!0
  putStr "Burst Time 2:  "
  print $ burstTime!!1
  putStr "Burst Time 3:  "
  print $ burstTime!!2
  putStrLn "   "
  putStr "Waiting Time 1:  "
  print $ waitingTime!!1
  putStr "Waiting Time 2:  "
  print $ waitingTime!!2
  putStr "Waiting Time 3:  "
  print $ waitingTime!!3
  putStrLn "   "
  let w1=waitingTime!!1
  let w2=waitingTime!!2
  let w3=waitingTime!!3
  let sumWt =(w1+w2+w3)
  putStrLn "Arrival Time 1: 0 "
  putStrLn "Arrival Time 2: 0 "
  putStrLn "Arrival Time 3: 0 "
  putStrLn "   "
  putStr "Turn Around Time 1:  "
  print $ (copyBt!!0 + w1)
  putStr "Turn Around Time 2:  "
  print $ (copyBt!!1 + w2)
  putStr "Turn Around Time 3:  "
  print $ (copyBt!!2 + w3) 
  putStrLn "   "
  putStr "Average Waiting Time:  "
  print $ sumWt `div` 3
  putStr "Average Turn Around Time :  "
  print $ (copyBt!!0 + w1+copyBt!!1 + w2+copyBt!!2 + w3) `div` 3
  --print $ trueKontrol burstTime q copyBt t wt1 wt2 wt3 0