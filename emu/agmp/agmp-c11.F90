      !! ORDER: 11 (49 terms)

      !! [c tan(dlat/2)]**0
      !!    (-28350*dlonh^11*s^11)+103950*dlonh^11*s^9+34650*dlonh^9*s^9-145530*dlonh^11*s^7-103950*dlonh^9*s^7-44550
      !!   *dlonh^7*s^7+95370*dlonh^11*s^5+110880*dlonh^9*s^5+103950*dlonh^7*... 
      Cc(0) = 155925.0_KTGT   !! denom
      Cc(1) = s*(s2*(s2*(s2*(s2*(103950.0_KTGT-28350.0_KTGT &
       & *s2)-145530.0_KTGT)+95370.0_KTGT)-28204.0_KTGT)+2764.0_KTGT)    !! (dlon/2)**11
      Cc(2) = s*(s2*(s2*(s2*(34650.0_KTGT*s2-103950.0_KTGT)+110880.0_KTGT)-48400.0_KTGT)+6820.0_KTGT)    !! (dlon/2)**9
      Cc(3) = s*(s2*(s2*(103950.0_KTGT-44550.0_KTGT*s2)-76230.0_KTGT)+16830.0_KTGT)    !! (dlon/2)**7
      Cc(4) = s*(s2*(62370.0_KTGT*s2-103950.0_KTGT)+41580.0_KTGT)    !! (dlon/2)**5
      Cc(5) = s*(103950.0_KTGT-103950.0_KTGT*s2)    !! (dlon/2)**3
      Cc(6) = 311850.0_KTGT*s    !! (dlon/2)**1

      !! [c tan(dlat/2)]**1
      !!    5670*c*dlonh^9*s^8-13230*c*dlonh^9*s^6-5670*c*dlonh^7*s^6+10080*c*dlonh^9*s^4+9450*c*dlonh^7*s^4+5670*c
      !!   *dlonh^5*s^4-2640*c*dlonh^9*s^2-4158*c*dlonh^7*s^2-5670*c*dlonh^5*s^... 
      Cc(7) = 2835.0_KTGT   !! denom
      Cc(8) = s2*(s2*(s2*(5670.0_KTGT*s2-13230.0_KTGT)+10080.0_KTGT)-2640.0_KTGT)+124.0_KTGT    !! (dlon/2)**9
      Cc(9) = s2*(s2*(9450.0_KTGT-5670.0_KTGT*s2)-4158.0_KTGT)+306.0_KTGT    !! (dlon/2)**7
      Cc(10) = s2*(5670.0_KTGT*s2-5670.0_KTGT)+756.0_KTGT    !! (dlon/2)**5
      Cc(11) = 1890.0_KTGT-5670.0_KTGT*s2    !! (dlon/2)**3
      Cc(12) = 5670.0_KTGT    !! (dlon/2)**1

      !! [c tan(dlat/2)]**2
      !!    7560*c^2*dlonh^9*s^7-13230*c^2*dlonh^9*s^5-5670*c^2*dlonh^7*s^5+6720*c^2*dlonh^9*s^3+6300*c^2*dlonh^7*s^3+3780
      !!   *c^2*dlonh^5*s^3-880*c^2*dlonh^9*s-1386*c^2*dlonh^7*s-1890*c^... 
      Cc(13) = 945.0_KTGT   !! denom
      Cc(14) = s*(s2*(s2*(7560.0_KTGT*s2-13230.0_KTGT)+6720.0_KTGT)-880.0_KTGT)    !! (dlon/2)**9
      Cc(15) = s*(s2*(6300.0_KTGT-5670.0_KTGT*s2)-1386.0_KTGT)    !! (dlon/2)**7
      Cc(16) = s*(3780.0_KTGT*s2-1890.0_KTGT)    !! (dlon/2)**5
      Cc(17) = -1890.0_KTGT*s    !! (dlon/2)**3

      !! [c tan(dlat/2)]**3
      !!    (-450*c^3*dlonh^7*s^4)+300*c^3*dlonh^7*s^2+180*c^3*dlonh^5*s^2-22*c^3*dlonh^7-30*c^3*dlonh^5-30*c^3*dlonh^3 
      Cc(18) = 45.0_KTGT   !! denom
      Cc(19) = s2*(300.0_KTGT-450.0_KTGT*s2)-22.0_KTGT    !! (dlon/2)**7
      Cc(20) = 180.0_KTGT*s2-30.0_KTGT    !! (dlon/2)**5
      Cc(21) = -30.0_KTGT    !! (dlon/2)**3

      !! [c tan(dlat/2)]**4
      !!    (-30*c^4*dlonh^7*s^3)+10*c^4*dlonh^7*s+6*c^4*dlonh^5*s 
      Cc(22) = 3.0_KTGT   !! denom
      Cc(23) = s*(10.0_KTGT-30.0_KTGT*s2)    !! (dlon/2)**7
      Cc(24) = 6.0_KTGT*s    !! (dlon/2)**5

      !! [c tan(dlat/2)]**5
      !!    2*c^5*dlonh^5 
      Cc(25) = 5.0_KTGT   !! denom
      Cc(26) = 2.0_KTGT    !! (dlon/2)**5

      !! subtract pmem area
      if (rel) Cc(6) = 0.0_KTGT
