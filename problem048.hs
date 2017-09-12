
selfPowers :: String
selfPowers = reverse $ take 10 $ reverse $ show $ sum [ n^n | n <- [1..1000]]

-- selfPowers == "9110846700"

