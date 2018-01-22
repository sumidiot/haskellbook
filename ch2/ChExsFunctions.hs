-- module ChExsFunctions

module ChExsFunctions where

-- waxOn = x * 5
--   where x = y ^ 2
--     where y = z + 8
--       where z = 7

waxOn = x * 5
  where z = 7
        y = z + 8
        x = y ^2

triple x = x * 3

waxOff x = triple x

