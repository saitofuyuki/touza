      !! ORDER: 9 (36 terms)

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
      !!    (-630*c*dlonh^7*s^6)+1050*c*dlonh^7*s^4+630*c*dlonh^5*s^4-462*c*dlonh^7*s^2-630*c*dlonh^5*s^2-630*c*dlonh^3
      !!   *s^2+34*c*dlonh^7+84*c*dlonh^5+210*c*dlonh^3+630*c*dlonh 
      Cc(6) = 315.0_KTGT   !! denom
      Cc(7) = s2*(s2*(1050.0_KTGT-630.0_KTGT*s2)-462.0_KTGT)+34.0_KTGT    !! (dlon/2)**7
      Cc(8) = s2*(630.0_KTGT*s2-630.0_KTGT)+84.0_KTGT    !! (dlon/2)**5
      Cc(9) = 210.0_KTGT-630.0_KTGT*s2    !! (dlon/2)**3
      Cc(10) = 630.0_KTGT    !! (dlon/2)**1

      !! [c tan(dlat/2)]**2
      !!    (-90*c^2*dlonh^7*s^5)+100*c^2*dlonh^7*s^3+60*c^2*dlonh^5*s^3-22*c^2*dlonh^7*s-30*c^2*dlonh^5*s-30*c^2*dlonh^3
      !!   *s 
      Cc(11) = 15.0_KTGT   !! denom
      Cc(12) = s*(s2*(100.0_KTGT-90.0_KTGT*s2)-22.0_KTGT)    !! (dlon/2)**7
      Cc(13) = s*(60.0_KTGT*s2-30.0_KTGT)    !! (dlon/2)**5
      Cc(14) = -30.0_KTGT*s    !! (dlon/2)**3

      !! [c tan(dlat/2)]**3
      !!    12*c^3*dlonh^5*s^2-2*c^3*dlonh^5-2*c^3*dlonh^3 
      Cc(15) = 3.0_KTGT   !! denom
      Cc(16) = 12.0_KTGT*s2-2.0_KTGT    !! (dlon/2)**5
      Cc(17) = -2.0_KTGT    !! (dlon/2)**3

      !! [c tan(dlat/2)]**4
      !!    2*c^4*dlonh^5*s 
      Cc(18) = 1.0_KTGT   !! denom
      Cc(19) = 2.0_KTGT*s    !! (dlon/2)**5

      !! subtract pmem area
      if (rel) Cc(5) = 0.0_KTGT
