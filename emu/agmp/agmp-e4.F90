      !! ORDER: 4 (28 terms)

      !! [0] sin[1 lat]
      Ce(0) = 10080.0_KTGT   !! denom
      Ce(1) = 20160.0_KTGT   !! (dlon/2)**1
      Ce(2) = 1680.0_KTGT-5040.0_KTGT*tdhsq   !! (dlon/2)**3
      Ce(3) = 2520.0_KTGT*tdhsq*tdhsq+168.0_KTGT   !! (dlon/2)**5
      Ce(4) = tdhsq*(((-1575.0_KTGT*tdhsq)-525.0_KTGT)*tdhsq-21.0_KTGT)+17.0_KTGT   !! (dlon/2)**7

      !! [1] cos[1 lat] tan(dlat/2)
      Ce(5) = 10080.0_KTGT   !! denom
      Ce(6) = 20160.0_KTGT   !! (dlon/2)**1
      Ce(7) = 1680.0_KTGT-5040.0_KTGT*tdhsq   !! (dlon/2)**3
      Ce(8) = 2520.0_KTGT*tdhsq*tdhsq+168.0_KTGT   !! (dlon/2)**5
      Ce(9) = tdhsq*(((-1575.0_KTGT*tdhsq)-525.0_KTGT)*tdhsq-21.0_KTGT)+17.0_KTGT   !! (dlon/2)**7

      !! [2] sin[3 lat]
      Ce(10) = 1440.0_KTGT   !! denom
      Ce(11) = 240.0_KTGT-720.0_KTGT*tdhsq   !! (dlon/2)**3
      Ce(12) = tdhsq*(540.0_KTGT*tdhsq-360.0_KTGT)+60.0_KTGT   !! (dlon/2)**5
      Ce(13) = tdhsq*((225.0_KTGT-405.0_KTGT*tdhsq)*tdhsq-63.0_KTGT)+11.0_KTGT   !! (dlon/2)**7

      !! [3] cos[3 lat] tan(dlat/2)
      Ce(14) = 1440.0_KTGT   !! denom
      Ce(15) = 720.0_KTGT-240.0_KTGT*tdhsq   !! (dlon/2)**3
      Ce(16) = tdhsq*(180.0_KTGT*tdhsq-600.0_KTGT)+180.0_KTGT   !! (dlon/2)**5
      Ce(17) = tdhsq*((435.0_KTGT-135.0_KTGT*tdhsq)*tdhsq-101.0_KTGT)+33.0_KTGT   !! (dlon/2)**7

      !! [4] sin[5 lat]
      Ce(18) = 480.0_KTGT   !! denom
      Ce(19) = tdhsq*(60.0_KTGT*tdhsq-120.0_KTGT)+12.0_KTGT   !! (dlon/2)**5
      Ce(20) = tdhsq*((175.0_KTGT-75.0_KTGT*tdhsq)*tdhsq-65.0_KTGT)+5.0_KTGT   !! (dlon/2)**7

      !! [5] cos[5 lat] tan(dlat/2)
      Ce(21) = 480.0_KTGT   !! denom
      Ce(22) = tdhsq*(12.0_KTGT*tdhsq-120.0_KTGT)+60.0_KTGT   !! (dlon/2)**5
      Ce(23) = tdhsq*((155.0_KTGT-15.0_KTGT*tdhsq)*tdhsq-125.0_KTGT)+25.0_KTGT   !! (dlon/2)**7

      !! [6] sin[7 lat]
      Ce(24) = 224.0_KTGT   !! denom
      Ce(25) = tdhsq*((35.0_KTGT-7.0_KTGT*tdhsq)*tdhsq-21.0_KTGT)+1.0_KTGT   !! (dlon/2)**7

      !! [7] cos[7 lat] tan(dlat/2)
      Ce(26) = 224.0_KTGT   !! denom
      Ce(27) = tdhsq*((21.0_KTGT-tdhsq)*tdhsq-35.0_KTGT)+7.0_KTGT   !! (dlon/2)**7

      !! subtract pmem area
      if (rel) Ce(1) = 0.0_KTGT

      dlhf = dlh
      C(0) = (((Ce(4)*dlhsq+Ce(3))*dlhsq+Ce(2))*dlhsq+Ce(1)) * dlhf / Ce(0)
      C(1) = (((Ce(9)*dlhsq+Ce(8))*dlhsq+Ce(7))*dlhsq+Ce(6)) * dlhf / Ce(5)
      dlhf = dlhf * dlhsq
      C(2) = ((Ce(13)*dlhsq+Ce(12))*dlhsq+Ce(11)) * dlhf / Ce(10)
      C(3) = ((Ce(17)*dlhsq+Ce(16))*dlhsq+Ce(15)) * dlhf / Ce(14)
      dlhf = dlhf * dlhsq
      C(4) = (Ce(20)*dlhsq+Ce(19)) * dlhf / Ce(18)
      C(5) = (Ce(23)*dlhsq+Ce(22)) * dlhf / Ce(21)
      dlhf = dlhf * dlhsq
      C(6) = Ce(25) * dlhf / Ce(24)
      C(7) = Ce(27) * dlhf / Ce(26)
