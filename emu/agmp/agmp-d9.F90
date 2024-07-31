      !! ORDER: 9 (40 terms)

      !! [c tan(dlat/2)]**0
      !!    630*dlonh^9*s^9-1890*dlonh^9*s^7-810*dlonh^7*s^7+2016*dlonh^9*s^5+1890*dlonh^7*s^5+1134*dlonh^5*s^5-880
      !!   *dlonh^9*s^3-1386*dlonh^7*s^3-1890*dlonh^5*s^3-1890*dlonh^3*s^3+124*... 
      Cd(0) = 2835.0_KTGT   !! denom
      Cd(1) = s*(s2*(s2*(s2*(630.0_KTGT*s2-1890.0_KTGT)+2016.0_KTGT)-880.0_KTGT)+124.0_KTGT)    !! (dlon/2)**9
      Cd(2) = s*(s2*(s2*(1890.0_KTGT-810.0_KTGT*s2)-1386.0_KTGT)+306.0_KTGT)    !! (dlon/2)**7
      Cd(3) = s*(s2*(1134.0_KTGT*s2-1890.0_KTGT)+756.0_KTGT)    !! (dlon/2)**5
      Cd(4) = s*(1890.0_KTGT-1890.0_KTGT*s2)    !! (dlon/2)**3
      Cd(5) = 5670.0_KTGT*s    !! (dlon/2)**1

      !! [c tan(dlat/2)]**1
      !!    5670*c*dlonh^9*s^8-13230*c*dlonh^9*s^6-5670*c*dlonh^7*s^6+10080*c*dlonh^9*s^4+9450*c*dlonh^7*s^4+5670*c
      !!   *dlonh^5*s^4-2640*c*dlonh^9*s^2-4158*c*dlonh^7*s^2-5670*c*dlonh^5*s^... 
      Cd(6) = 2835.0_KTGT   !! denom
      Cd(7) = s2*(s2*(s2*(5670.0_KTGT*s2-13230.0_KTGT)+10080.0_KTGT)-2640.0_KTGT)+124.0_KTGT    !! (dlon/2)**9
      Cd(8) = s2*(s2*(9450.0_KTGT-5670.0_KTGT*s2)-4158.0_KTGT)+306.0_KTGT    !! (dlon/2)**7
      Cd(9) = s2*(5670.0_KTGT*s2-5670.0_KTGT)+756.0_KTGT    !! (dlon/2)**5
      Cd(10) = 1890.0_KTGT-5670.0_KTGT*s2    !! (dlon/2)**3
      Cd(11) = 5670.0_KTGT    !! (dlon/2)**1

      !! [c tan(dlat/2)]**2
      !!    7560*c^2*dlonh^9*s^7-13230*c^2*dlonh^9*s^5-5670*c^2*dlonh^7*s^5+6720*c^2*dlonh^9*s^3+6300*c^2*dlonh^7*s^3+3780
      !!   *c^2*dlonh^5*s^3-880*c^2*dlonh^9*s-1386*c^2*dlonh^7*s-1890*c^... 
      Cd(12) = 945.0_KTGT   !! denom
      Cd(13) = s*(s2*(s2*(7560.0_KTGT*s2-13230.0_KTGT)+6720.0_KTGT)-880.0_KTGT)    !! (dlon/2)**9
      Cd(14) = s*(s2*(6300.0_KTGT-5670.0_KTGT*s2)-1386.0_KTGT)    !! (dlon/2)**7
      Cd(15) = s*(3780.0_KTGT*s2-1890.0_KTGT)    !! (dlon/2)**5
      Cd(16) = -1890.0_KTGT*s    !! (dlon/2)**3

      !! [c tan(dlat/2)]**3
      !!    52920*c^3*dlonh^9*s^6-66150*c^3*dlonh^9*s^4-28350*c^3*dlonh^7*s^4+20160*c^3*dlonh^9*s^2+18900*c^3*dlonh^7
      !!   *s^2+11340*c^3*dlonh^5*s^2-880*c^3*dlonh^9-1386*c^3*dlonh^7-1890*c... 
      Cd(17) = 2835.0_KTGT   !! denom
      Cd(18) = s2*(s2*(52920.0_KTGT*s2-66150.0_KTGT)+20160.0_KTGT)-880.0_KTGT    !! (dlon/2)**9
      Cd(19) = s2*(18900.0_KTGT-28350.0_KTGT*s2)-1386.0_KTGT    !! (dlon/2)**7
      Cd(20) = 11340.0_KTGT*s2-1890.0_KTGT    !! (dlon/2)**5
      Cd(21) = -1890.0_KTGT    !! (dlon/2)**3

      !! [c tan(dlat/2)]**4
      !!    252*c^4*dlonh^9*s^5-210*c^4*dlonh^9*s^3-90*c^4*dlonh^7*s^3+32*c^4*dlonh^9*s+30*c^4*dlonh^7*s+18*c^4*dlonh^5*s 
      Cd(22) = 9.0_KTGT   !! denom
      Cd(23) = s*(s2*(252.0_KTGT*s2-210.0_KTGT)+32.0_KTGT)    !! (dlon/2)**9
      Cd(24) = s*(30.0_KTGT-90.0_KTGT*s2)    !! (dlon/2)**7
      Cd(25) = 18.0_KTGT*s    !! (dlon/2)**5

      !! [c tan(dlat/2)]**5
      !!    1260*c^5*dlonh^9*s^4-630*c^5*dlonh^9*s^2-270*c^5*dlonh^7*s^2+32*c^5*dlonh^9+30*c^5*dlonh^7+18*c^5*dlonh^5 
      Cd(26) = 45.0_KTGT   !! denom
      Cd(27) = s2*(1260.0_KTGT*s2-630.0_KTGT)+32.0_KTGT    !! (dlon/2)**9
      Cd(28) = 30.0_KTGT-270.0_KTGT*s2    !! (dlon/2)**7
      Cd(29) = 18.0_KTGT    !! (dlon/2)**5

      !! [c tan(dlat/2)]**6
      !!    56*c^6*dlonh^9*s^3-14*c^6*dlonh^9*s-6*c^6*dlonh^7*s 
      Cd(30) = 3.0_KTGT   !! denom
      Cd(31) = s*(56.0_KTGT*s2-14.0_KTGT)    !! (dlon/2)**9
      Cd(32) = -6.0_KTGT*s    !! (dlon/2)**7

      !! [c tan(dlat/2)]**7
      !!    168*c^7*dlonh^9*s^2-14*c^7*dlonh^9-6*c^7*dlonh^7 
      Cd(33) = 21.0_KTGT   !! denom
      Cd(34) = 168.0_KTGT*s2-14.0_KTGT    !! (dlon/2)**9
      Cd(35) = -6.0_KTGT    !! (dlon/2)**7

      !! [c tan(dlat/2)]**8
      !!    2*c^8*dlonh^9*s 
      Cd(36) = 1.0_KTGT   !! denom
      Cd(37) = 2.0_KTGT*s    !! (dlon/2)**9

      !! [c tan(dlat/2)]**9
      !!    2*c^9*dlonh^9 
      Cd(38) = 9.0_KTGT   !! denom
      Cd(39) = 2.0_KTGT    !! (dlon/2)**9

      !! subtract pmem area
      if (rel) Cd(5) = 0.0_KTGT
