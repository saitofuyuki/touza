      !! ORDER: 6 (54 terms)

      !! [0] sin[1 lat]
      Ce(0) = 39916800.0_KTGT   !! denom
      Ce(1) = 79833600.0_KTGT   !! (dlon/2)**1
      Ce(2) = 6652800.0_KTGT-19958400.0_KTGT*tdhsq   !! (dlon/2)**3
      Ce(3) = 9979200.0_KTGT*tdhsq*tdhsq+665280.0_KTGT   !! (dlon/2)**5
      Ce(4) = tdhsq*(((-6237000.0_KTGT*tdhsq)-2079000.0_KTGT)*tdhsq-83160.0_KTGT)+67320.0_KTGT   !! (dlon/2)**7
      Ce(5) = tdhsq*(tdhsq*(tdhsq*(4365900.0_KTGT &
       & *tdhsq+2910600.0_KTGT)+277200.0_KTGT)-6600.0_KTGT)+6820.0_KTGT   !! (dlon/2)**9
      Ce(6) = tdhsq*(tdhsq*(tdhsq*(((-3274425.0_KTGT*tdhsq)-3274425.0_KTGT) &
       & *tdhsq-727650.0_KTGT)-21450.0_KTGT)-693.0_KTGT)+691.0_KTGT   !! (dlon/2)**11

      !! [1] cos[1 lat] tan(dlat/2)
      Ce(7) = 39916800.0_KTGT   !! denom
      Ce(8) = 79833600.0_KTGT   !! (dlon/2)**1
      Ce(9) = 6652800.0_KTGT-19958400.0_KTGT*tdhsq   !! (dlon/2)**3
      Ce(10) = 9979200.0_KTGT*tdhsq*tdhsq+665280.0_KTGT   !! (dlon/2)**5
      Ce(11) = tdhsq*(((-6237000.0_KTGT*tdhsq)-2079000.0_KTGT)*tdhsq-83160.0_KTGT)+67320.0_KTGT   !! (dlon/2)**7
      Ce(12) = tdhsq*(tdhsq*(tdhsq*(4365900.0_KTGT &
       & *tdhsq+2910600.0_KTGT)+277200.0_KTGT)-6600.0_KTGT)+6820.0_KTGT   !! (dlon/2)**9
      Ce(13) = tdhsq*(tdhsq*(tdhsq*(((-3274425.0_KTGT*tdhsq)-3274425.0_KTGT) &
       & *tdhsq-727650.0_KTGT)-21450.0_KTGT)-693.0_KTGT)+691.0_KTGT   !! (dlon/2)**11

      !! [2] sin[3 lat]
      Ce(14) = 3628800.0_KTGT   !! denom
      Ce(15) = 604800.0_KTGT-1814400.0_KTGT*tdhsq   !! (dlon/2)**3
      Ce(16) = tdhsq*(1360800.0_KTGT*tdhsq-907200.0_KTGT)+151200.0_KTGT   !! (dlon/2)**5
      Ce(17) = tdhsq*((567000.0_KTGT-1020600.0_KTGT*tdhsq)*tdhsq-158760.0_KTGT)+27720.0_KTGT   !! (dlon/2)**7
      Ce(18) = tdhsq*(tdhsq*(tdhsq*(793800.0_KTGT &
       & *tdhsq-264600.0_KTGT)+37800.0_KTGT)-25800.0_KTGT)+4400.0_KTGT   !! (dlon/2)**9
      Ce(19) = tdhsq*(tdhsq*(tdhsq*((42525.0_KTGT-637875.0_KTGT*tdhsq) &
       & *tdhsq+39690.0_KTGT)+11250.0_KTGT)-3783.0_KTGT)+641.0_KTGT   !! (dlon/2)**11

      !! [3] cos[3 lat] tan(dlat/2)
      Ce(20) = 3628800.0_KTGT   !! denom
      Ce(21) = 1814400.0_KTGT-604800.0_KTGT*tdhsq   !! (dlon/2)**3
      Ce(22) = tdhsq*(453600.0_KTGT*tdhsq-1512000.0_KTGT)+453600.0_KTGT   !! (dlon/2)**5
      Ce(23) = tdhsq*((1096200.0_KTGT-340200.0_KTGT*tdhsq)*tdhsq-254520.0_KTGT)+83160.0_KTGT   !! (dlon/2)**7
      Ce(24) = tdhsq*(tdhsq*(tdhsq*(264600.0_KTGT &
       & *tdhsq-793800.0_KTGT)+12600.0_KTGT)-42200.0_KTGT)+13200.0_KTGT   !! (dlon/2)**9
      Ce(25) = tdhsq*(tdhsq*(tdhsq*((581175.0_KTGT-212625.0_KTGT*tdhsq) &
       & *tdhsq+164430.0_KTGT)+18870.0_KTGT)-6221.0_KTGT)+1923.0_KTGT   !! (dlon/2)**11

      !! [4] sin[5 lat]
      Ce(26) = 483840.0_KTGT   !! denom
      Ce(27) = tdhsq*(60480.0_KTGT*tdhsq-120960.0_KTGT)+12096.0_KTGT   !! (dlon/2)**5
      Ce(28) = tdhsq*((176400.0_KTGT-75600.0_KTGT*tdhsq)*tdhsq-65520.0_KTGT)+5040.0_KTGT   !! (dlon/2)**7
      Ce(29) = tdhsq*(tdhsq*(tdhsq*(75600.0_KTGT &
       & *tdhsq-176400.0_KTGT)+72240.0_KTGT)-18480.0_KTGT)+1344.0_KTGT   !! (dlon/2)**9
      Ce(30) = tdhsq*(tdhsq*(tdhsq*((155925.0_KTGT-70875.0_KTGT*tdhsq) &
       & *tdhsq-48510.0_KTGT)+16250.0_KTGT)-4087.0_KTGT)+289.0_KTGT   !! (dlon/2)**11

      !! [5] cos[5 lat] tan(dlat/2)
      Ce(31) = 483840.0_KTGT   !! denom
      Ce(32) = tdhsq*(12096.0_KTGT*tdhsq-120960.0_KTGT)+60480.0_KTGT   !! (dlon/2)**5
      Ce(33) = tdhsq*((156240.0_KTGT-15120.0_KTGT*tdhsq)*tdhsq-126000.0_KTGT)+25200.0_KTGT   !! (dlon/2)**7
      Ce(34) = tdhsq*(tdhsq*(tdhsq*(15120.0_KTGT &
       & *tdhsq-156240.0_KTGT)+127344.0_KTGT)-38640.0_KTGT)+6720.0_KTGT   !! (dlon/2)**9
      Ce(35) = tdhsq*(tdhsq*(tdhsq*((144585.0_KTGT-14175.0_KTGT*tdhsq) &
       & *tdhsq-100422.0_KTGT)+26434.0_KTGT)-8875.0_KTGT)+1445.0_KTGT   !! (dlon/2)**11

      !! [6] sin[7 lat]
      Ce(36) = 53760.0_KTGT   !! denom
      Ce(37) = tdhsq*((8400.0_KTGT-1680.0_KTGT*tdhsq)*tdhsq-5040.0_KTGT)+240.0_KTGT   !! (dlon/2)**7
      Ce(38) = tdhsq*(tdhsq*(tdhsq*(2940.0_KTGT*tdhsq-15680.0_KTGT)+13720.0_KTGT)-3360.0_KTGT)+140.0_KTGT   !! (dlon/2) &
       & **9
      Ce(39) = tdhsq*(tdhsq*(tdhsq*((19845.0_KTGT-3675.0_KTGT*tdhsq) &
       & *tdhsq-18718.0_KTGT)+6650.0_KTGT)-1239.0_KTGT)+49.0_KTGT   !! (dlon/2)**11

      !! [7] cos[7 lat] tan(dlat/2)
      Ce(40) = 53760.0_KTGT   !! denom
      Ce(41) = tdhsq*((5040.0_KTGT-240.0_KTGT*tdhsq)*tdhsq-8400.0_KTGT)+1680.0_KTGT   !! (dlon/2)**7
      Ce(42) = tdhsq*(tdhsq*(tdhsq*(420.0_KTGT*tdhsq-8960.0_KTGT)+17640.0_KTGT)-7840.0_KTGT)+980.0_KTGT   !! (dlon/2)**9
      Ce(43) = tdhsq*(tdhsq*(tdhsq*((11235.0_KTGT-525.0_KTGT*tdhsq) &
       & *tdhsq-22834.0_KTGT)+12054.0_KTGT)-3185.0_KTGT)+343.0_KTGT   !! (dlon/2)**11

      !! [8] sin[9 lat]
      Ce(44) = 4608.0_KTGT   !! denom
      Ce(45) = tdhsq*(tdhsq*(tdhsq*(36.0_KTGT*tdhsq-336.0_KTGT)+504.0_KTGT)-144.0_KTGT)+4.0_KTGT   !! (dlon/2)**9
      Ce(46) = tdhsq*(tdhsq*(tdhsq*((783.0_KTGT-81.0_KTGT*tdhsq) &
       & *tdhsq-1386.0_KTGT)+702.0_KTGT)-117.0_KTGT)+3.0_KTGT   !! (dlon/2)**11

      !! [9] cos[9 lat] tan(dlat/2)
      Ce(47) = 4608.0_KTGT   !! denom
      Ce(48) = tdhsq*(tdhsq*(tdhsq*(4.0_KTGT*tdhsq-144.0_KTGT)+504.0_KTGT)-336.0_KTGT)+36.0_KTGT   !! (dlon/2)**9
      Ce(49) = tdhsq*(tdhsq*(tdhsq*((327.0_KTGT-9.0_KTGT*tdhsq) &
       & *tdhsq-1242.0_KTGT)+1134.0_KTGT)-333.0_KTGT)+27.0_KTGT   !! (dlon/2)**11

      !! [10] sin[11 lat]
      Ce(50) = 5632.0_KTGT   !! denom
      Ce(51) = tdhsq*(tdhsq*(tdhsq*((165.0_KTGT-11.0_KTGT*tdhsq) &
       & *tdhsq-462.0_KTGT)+330.0_KTGT)-55.0_KTGT)+1.0_KTGT   !! (dlon/2)**11

      !! [11] cos[11 lat] tan(dlat/2)
      Ce(52) = 5632.0_KTGT   !! denom
      Ce(53) = tdhsq*(tdhsq*(tdhsq*((55.0_KTGT-tdhsq)*tdhsq-330.0_KTGT)+462.0_KTGT)-165.0_KTGT)+11.0_KTGT   !! (dlon/2) &
       & **11

      !! subtract pmem area
      if (rel) Ce(1) = 0.0_KTGT

      dlhf = dlh
      C(0) = (((((Ce(6)*dlhsq+Ce(5))*dlhsq+Ce(4))*dlhsq+Ce(3))*dlhsq+Ce(2))*dlhsq+Ce(1)) * dlhf / Ce(0)
      C(1) = (((((Ce(13)*dlhsq+Ce(12))*dlhsq+Ce(11))*dlhsq+Ce(10))*dlhsq+Ce(9))*dlhsq+Ce(8)) * dlhf / Ce(7)
      dlhf = dlhf * dlhsq
      C(2) = ((((Ce(19)*dlhsq+Ce(18))*dlhsq+Ce(17))*dlhsq+Ce(16))*dlhsq+Ce(15)) * dlhf / Ce(14)
      C(3) = ((((Ce(25)*dlhsq+Ce(24))*dlhsq+Ce(23))*dlhsq+Ce(22))*dlhsq+Ce(21)) * dlhf / Ce(20)
      dlhf = dlhf * dlhsq
      C(4) = (((Ce(30)*dlhsq+Ce(29))*dlhsq+Ce(28))*dlhsq+Ce(27)) * dlhf / Ce(26)
      C(5) = (((Ce(35)*dlhsq+Ce(34))*dlhsq+Ce(33))*dlhsq+Ce(32)) * dlhf / Ce(31)
      dlhf = dlhf * dlhsq
      C(6) = ((Ce(39)*dlhsq+Ce(38))*dlhsq+Ce(37)) * dlhf / Ce(36)
      C(7) = ((Ce(43)*dlhsq+Ce(42))*dlhsq+Ce(41)) * dlhf / Ce(40)
      dlhf = dlhf * dlhsq
      C(8) = (Ce(46)*dlhsq+Ce(45)) * dlhf / Ce(44)
      C(9) = (Ce(49)*dlhsq+Ce(48)) * dlhf / Ce(47)
      dlhf = dlhf * dlhsq
      C(10) = Ce(51) * dlhf / Ce(50)
      C(11) = Ce(53) * dlhf / Ce(52)
