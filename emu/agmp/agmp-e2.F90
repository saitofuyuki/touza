      !! ORDER: 2 (10 terms)

      !! [0] sin[1 lat]
      Ce(0) = 6.0_KTGT   !! denom
      Ce(1) = 12.0_KTGT   !! (dlon/2)**1
      Ce(2) = 1.0_KTGT-3.0_KTGT*tdhsq   !! (dlon/2)**3

      !! [1] cos[1 lat] tan(dlat/2)
      Ce(3) = 6.0_KTGT   !! denom
      Ce(4) = 12.0_KTGT   !! (dlon/2)**1
      Ce(5) = 1.0_KTGT-3.0_KTGT*tdhsq   !! (dlon/2)**3

      !! [2] sin[3 lat]
      Ce(6) = 6.0_KTGT   !! denom
      Ce(7) = 1.0_KTGT-3.0_KTGT*tdhsq   !! (dlon/2)**3

      !! [3] cos[3 lat] tan(dlat/2)
      Ce(8) = 6.0_KTGT   !! denom
      Ce(9) = 3.0_KTGT-tdhsq   !! (dlon/2)**3

      !! subtract pmem area
      if (rel) Ce(1) = 0.0_KTGT

      dlhf = dlh
      C(0) = (Ce(2)*dlhsq+Ce(1)) * dlhf / Ce(0)
      C(1) = (Ce(5)*dlhsq+Ce(4)) * dlhf / Ce(3)
      dlhf = dlhf * dlhsq
      C(2) = Ce(7) * dlhf / Ce(6)
      C(3) = Ce(9) * dlhf / Ce(8)
