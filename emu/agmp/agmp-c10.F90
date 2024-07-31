      !! ORDER: 10 (24 terms)

      !! [c tan(dlat/2)]**0
      !!    630*dlonh^9*s^9-1890*dlonh^9*s^7-810*dlonh^7*s^7+2016*dlonh^9*s^5+1890*dlonh^7*s^5+1134*dlonh^5*s^5-880
      !!   *dlonh^9*s^3-1386*dlonh^7*s^3-1890*dlonh^5*s^3-1890*dlonh^3*s^3+124*... 
      Cc(0) = 2835.0_KTGT   !! denom
      Cc(1) = s*(s2*(s2*(s2*(630.0_KTGT*s2-1890.0_KTGT)+2016.0_KTGT)-880.0_KTGT)+124.0_KTGT)    !! (dlon/2)**9
      Cc(2) = s*(s2*(s2*(1890.0_KTGT-810.0_KTGT*s2)-1386.0_KTGT)+306.0_KTGT)    !! (dlon/2)**7
      Cc(3) = s*(s2*(1134.0_KTGT*s2-1890.0_KTGT)+756.0_KTGT)    !! (dlon/2)**5
      Cc(4) = s*(1890.0_KTGT-1890.0_KTGT*s2)    !! (dlon/2)**3
      Cc(5) = 5670.0_KTGT*s    !! (dlon/2)**1

      !! [c tan(dlat/2)]**1
      !!    5670*c*dlonh^9*s^8-13230*c*dlonh^9*s^6-5670*c*dlonh^7*s^6+10080*c*dlonh^9*s^4+9450*c*dlonh^7*s^4+5670*c
      !!   *dlonh^5*s^4-2640*c*dlonh^9*s^2-4158*c*dlonh^7*s^2-5670*c*dlonh^5*s^... 
      Cc(6) = 2835.0_KTGT   !! denom
      Cc(7) = s2*(s2*(s2*(5670.0_KTGT*s2-13230.0_KTGT)+10080.0_KTGT)-2640.0_KTGT)+124.0_KTGT    !! (dlon/2)**9
      Cc(8) = s2*(s2*(9450.0_KTGT-5670.0_KTGT*s2)-4158.0_KTGT)+306.0_KTGT    !! (dlon/2)**7
      Cc(9) = s2*(5670.0_KTGT*s2-5670.0_KTGT)+756.0_KTGT    !! (dlon/2)**5
      Cc(10) = 1890.0_KTGT-5670.0_KTGT*s2    !! (dlon/2)**3
      Cc(11) = 5670.0_KTGT    !! (dlon/2)**1

      !! [c tan(dlat/2)]**2
      !!    (-90*c^2*dlonh^7*s^5)+100*c^2*dlonh^7*s^3+60*c^2*dlonh^5*s^3-22*c^2*dlonh^7*s-30*c^2*dlonh^5*s-30*c^2*dlonh^3
      !!   *s 
      Cc(12) = 15.0_KTGT   !! denom
      Cc(13) = s*(s2*(100.0_KTGT-90.0_KTGT*s2)-22.0_KTGT)    !! (dlon/2)**7
      Cc(14) = s*(60.0_KTGT*s2-30.0_KTGT)    !! (dlon/2)**5
      Cc(15) = -30.0_KTGT*s    !! (dlon/2)**3

      !! [c tan(dlat/2)]**3
      !!    (-450*c^3*dlonh^7*s^4)+300*c^3*dlonh^7*s^2+180*c^3*dlonh^5*s^2-22*c^3*dlonh^7-30*c^3*dlonh^5-30*c^3*dlonh^3 
      Cc(16) = 45.0_KTGT   !! denom
      Cc(17) = s2*(300.0_KTGT-450.0_KTGT*s2)-22.0_KTGT    !! (dlon/2)**7
      Cc(18) = 180.0_KTGT*s2-30.0_KTGT    !! (dlon/2)**5
      Cc(19) = -30.0_KTGT    !! (dlon/2)**3

      !! [c tan(dlat/2)]**4
      !!    2*c^4*dlonh^5*s 
      Cc(20) = 1.0_KTGT   !! denom
      Cc(21) = 2.0_KTGT*s    !! (dlon/2)**5

      !! [c tan(dlat/2)]**5
      !!    2*c^5*dlonh^5 
      Cc(22) = 5.0_KTGT   !! denom
      Cc(23) = 2.0_KTGT    !! (dlon/2)**5

      !! subtract pmem area
      if (rel) Cc(5) = 0.0_KTGT
