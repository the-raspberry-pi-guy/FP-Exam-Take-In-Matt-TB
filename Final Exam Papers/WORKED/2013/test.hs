f :: [Bool] -> Bool
f [a,b,r] = (not a||r||b) && (not a||not r|| not b) && (a || r || not b) && (a || not r || b) == (not b) || ((not a) && b && r) || (a && b && (not r))
