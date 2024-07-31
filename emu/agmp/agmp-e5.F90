      !! ORDER: 5 (40 terms)

      !! [0] sin[1 lat]
      Ce(0) = 181440.0_KTGT   !! denom
      Ce(1) = 362880.0_KTGT   !! (dlon/2)**1
      Ce(2) = 30240.0_KTGT-90720.0_KTGT*tdhsq   !! (dlon/2)**3
      Ce(3) = 45360.0_KTGT*tdhsq*tdhsq+3024.0_KTGT   !! (dlon/2)**5
      Ce(4) = tdhsq*(((-28350.0_KTGT*tdhsq)-9450.0_KTGT)*tdhsq-378.0_KTGT)+306.0_KTGT   !! (dlon/2)**7
      Ce(5) = tdhsq*(tdhsq*(tdhsq*(19845.0_KTGT*tdhsq+13230.0_KTGT)+1260.0_KTGT)-30.0_KTGT)+31.0_KTGT   !! (dlon/2)**9

      !! [1] cos[1 lat] tan(dlat/2)
      Ce(6) = 181440.0_KTGT   !! denom
      Ce(7) = 362880.0_KTGT   !! (dlon/2)**1
      Ce(8) = 30240.0_KTGT-90720.0_KTGT*tdhsq   !! (dlon/2)**3
      Ce(9) = 45360.0_KTGT*tdhsq*tdhsq+3024.0_KTGT   !! (dlon/2)**5
      Ce(10) = tdhsq*(((-28350.0_KTGT*tdhsq)-9450.0_KTGT)*tdhsq-378.0_KTGT)+306.0_KTGT   !! (dlon/2)**7
      Ce(11) = tdhsq*(tdhsq*(tdhsq*(19845.0_KTGT*tdhsq+13230.0_KTGT)+1260.0_KTGT)-30.0_KTGT)+31.0_KTGT   !! (dlon/2)**9

      !! [2] sin[3 lat]
      Ce(12) = 90720.0_KTGT   !! denom
      Ce(13) = 15120.0_KTGT-45360.0_KTGT*tdhsq   !! (dlon/2)**3
      Ce(14) = tdhsq*(34020.0_KTGT*tdhsq-22680.0_KTGT)+3780.0_KTGT   !! (dlon/2)**5
      Ce(15) = tdhsq*((14175.0_KTGT-25515.0_KTGT*tdhsq)*tdhsq-3969.0_KTGT)+693.0_KTGT   !! (dlon/2)**7
      Ce(16) = tdhsq*(tdhsq*(tdhsq*(19845.0_KTGT*tdhsq-6615.0_KTGT)+945.0_KTGT)-645.0_KTGT)+110.0_KTGT   !! (dlon/2)**9

      !! [3] cos[3 lat] tan(dlat/2)
      Ce(17) = 90720.0_KTGT   !! denom
      Ce(18) = 45360.0_KTGT-15120.0_KTGT*tdhsq   !! (dlon/2)**3
      Ce(19) = tdhsq*(11340.0_KTGT*tdhsq-37800.0_KTGT)+11340.0_KTGT   !! (dlon/2)**5
      Ce(20) = tdhsq*((27405.0_KTGT-8505.0_KTGT*tdhsq)*tdhsq-6363.0_KTGT)+2079.0_KTGT   !! (dlon/2)**7
      Ce(21) = tdhsq*(tdhsq*(tdhsq*(6615.0_KTGT*tdhsq-19845.0_KTGT)+315.0_KTGT)-1055.0_KTGT)+330.0_KTGT   !! (dlon/2)**9

      !! [4] sin[5 lat]
      Ce(22) = 1440.0_KTGT   !! denom
      Ce(23) = tdhsq*(180.0_KTGT*tdhsq-360.0_KTGT)+36.0_KTGT   !! (dlon/2)**5
      Ce(24) = tdhsq*((525.0_KTGT-225.0_KTGT*tdhsq)*tdhsq-195.0_KTGT)+15.0_KTGT   !! (dlon/2)**7
      Ce(25) = tdhsq*(tdhsq*(tdhsq*(225.0_KTGT*tdhsq-525.0_KTGT)+215.0_KTGT)-55.0_KTGT)+4.0_KTGT   !! (dlon/2)**9

      !! [5] cos[5 lat] tan(dlat/2)
      Ce(26) = 1440.0_KTGT   !! denom
      Ce(27) = tdhsq*(36.0_KTGT*tdhsq-360.0_KTGT)+180.0_KTGT   !! (dlon/2)**5
      Ce(28) = tdhsq*((465.0_KTGT-45.0_KTGT*tdhsq)*tdhsq-375.0_KTGT)+75.0_KTGT   !! (dlon/2)**7
      Ce(29) = tdhsq*(tdhsq*(tdhsq*(45.0_KTGT*tdhsq-465.0_KTGT)+379.0_KTGT)-115.0_KTGT)+20.0_KTGT   !! (dlon/2)**9

      !! [6] sin[7 lat]
      Ce(30) = 2688.0_KTGT   !! denom
      Ce(31) = tdhsq*((420.0_KTGT-84.0_KTGT*tdhsq)*tdhsq-252.0_KTGT)+12.0_KTGT   !! (dlon/2)**7
      Ce(32) = tdhsq*(tdhsq*(tdhsq*(147.0_KTGT*tdhsq-784.0_KTGT)+686.0_KTGT)-168.0_KTGT)+7.0_KTGT   !! (dlon/2)**9

      !! [7] cos[7 lat] tan(dlat/2)
      Ce(33) = 2688.0_KTGT   !! denom
      Ce(34) = tdhsq*((252.0_KTGT-12.0_KTGT*tdhsq)*tdhsq-420.0_KTGT)+84.0_KTGT   !! (dlon/2)**7
      Ce(35) = tdhsq*(tdhsq*(tdhsq*(21.0_KTGT*tdhsq-448.0_KTGT)+882.0_KTGT)-392.0_KTGT)+49.0_KTGT   !! (dlon/2)**9

      !! [8] sin[9 lat]
      Ce(36) = 1152.0_KTGT   !! denom
      Ce(37) = tdhsq*(tdhsq*(tdhsq*(9.0_KTGT*tdhsq-84.0_KTGT)+126.0_KTGT)-36.0_KTGT)+1.0_KTGT   !! (dlon/2)**9

      !! [9] cos[9 lat] tan(dlat/2)
      Ce(38) = 1152.0_KTGT   !! denom
      Ce(39) = tdhsq*(tdhsq*((tdhsq-36.0_KTGT)*tdhsq+126.0_KTGT)-84.0_KTGT)+9.0_KTGT   !! (dlon/2)**9

      !! subtract pmem area
      if (rel) Ce(1) = 0.0_KTGT

      dlhf = dlh
      C(0) = ((((Ce(5)*dlhsq+Ce(4))*dlhsq+Ce(3))*dlhsq+Ce(2))*dlhsq+Ce(1)) * dlhf / Ce(0)
      C(1) = ((((Ce(11)*dlhsq+Ce(10))*dlhsq+Ce(9))*dlhsq+Ce(8))*dlhsq+Ce(7)) * dlhf / Ce(6)
      dlhf = dlhf * dlhsq
      C(2) = (((Ce(16)*dlhsq+Ce(15))*dlhsq+Ce(14))*dlhsq+Ce(13)) * dlhf / Ce(12)
      C(3) = (((Ce(21)*dlhsq+Ce(20))*dlhsq+Ce(19))*dlhsq+Ce(18)) * dlhf / Ce(17)
      dlhf = dlhf * dlhsq
      C(4) = ((Ce(25)*dlhsq+Ce(24))*dlhsq+Ce(23)) * dlhf / Ce(22)
      C(5) = ((Ce(29)*dlhsq+Ce(28))*dlhsq+Ce(27)) * dlhf / Ce(26)
      dlhf = dlhf * dlhsq
      C(6) = (Ce(32)*dlhsq+Ce(31)) * dlhf / Ce(30)
      C(7) = (Ce(35)*dlhsq+Ce(34)) * dlhf / Ce(33)
      dlhf = dlhf * dlhsq
      C(8) = Ce(37) * dlhf / Ce(36)
      C(9) = Ce(39) * dlhf / Ce(38)
