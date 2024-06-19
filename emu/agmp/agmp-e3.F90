      !! ORDER: 3 (18 terms)

      !! [0] sin[1 lat]
      Ce(0) = 60.0_KTGT   !! denom
      Ce(1) = 120.0_KTGT   !! (dlon/2)**1
      Ce(2) = 10.0_KTGT-30.0_KTGT*tdhsq   !! (dlon/2)**3
      Ce(3) = 15.0_KTGT*tdhsq*tdhsq+1.0_KTGT   !! (dlon/2)**5

      !! [1] cos[1 lat] tan(dlat/2)
      Ce(4) = 60.0_KTGT   !! denom
      Ce(5) = 120.0_KTGT   !! (dlon/2)**1
      Ce(6) = 10.0_KTGT-30.0_KTGT*tdhsq   !! (dlon/2)**3
      Ce(7) = 15.0_KTGT*tdhsq*tdhsq+1.0_KTGT   !! (dlon/2)**5

      !! [2] sin[3 lat]
      Ce(8) = 24.0_KTGT   !! denom
      Ce(9) = 4.0_KTGT-12.0_KTGT*tdhsq   !! (dlon/2)**3
      Ce(10) = tdhsq*(9.0_KTGT*tdhsq-6.0_KTGT)+1.0_KTGT   !! (dlon/2)**5

      !! [3] cos[3 lat] tan(dlat/2)
      Ce(11) = 24.0_KTGT   !! denom
      Ce(12) = 12.0_KTGT-4.0_KTGT*tdhsq   !! (dlon/2)**3
      Ce(13) = tdhsq*(3.0_KTGT*tdhsq-10.0_KTGT)+3.0_KTGT   !! (dlon/2)**5

      !! [4] sin[5 lat]
      Ce(14) = 40.0_KTGT   !! denom
      Ce(15) = tdhsq*(5.0_KTGT*tdhsq-10.0_KTGT)+1.0_KTGT   !! (dlon/2)**5

      !! [5] cos[5 lat] tan(dlat/2)
      Ce(16) = 40.0_KTGT   !! denom
      Ce(17) = (tdhsq-10.0_KTGT)*tdhsq+5.0_KTGT   !! (dlon/2)**5

      !! subtract pmem area
      if (rel) Ce(1) = 0.0_KTGT

      dlhf = dlh
      C(0) = ((Ce(3)*dlhsq+Ce(2))*dlhsq+Ce(1)) * dlhf / Ce(0)
      C(1) = ((Ce(7)*dlhsq+Ce(6))*dlhsq+Ce(5)) * dlhf / Ce(4)
      dlhf = dlhf * dlhsq
      C(2) = (Ce(10)*dlhsq+Ce(9)) * dlhf / Ce(8)
      C(3) = (Ce(13)*dlhsq+Ce(12)) * dlhf / Ce(11)
      dlhf = dlhf * dlhsq
      C(4) = Ce(15) * dlhf / Ce(14)
      C(5) = Ce(17) * dlhf / Ce(16)
