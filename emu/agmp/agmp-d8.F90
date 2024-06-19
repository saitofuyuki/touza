      !! ORDER: 8 (28 terms)

      !! [c tan(dlat/2)]**0
      !!    (-90*dlonh^7*s^7)+210*dlonh^7*s^5+126*dlonh^5*s^5-154*dlonh^7*s^3-210*dlonh^5*s^3-210*dlonh^3*s^3+34*dlonh^7
      !!   *s+84*dlonh^5*s+210*dlonh^3*s+630*dlonh*s 
      Cd(0) = 315.0_KTGT   !! denom
      Cd(1) = s*(s2*(s2*(210.0_KTGT-90.0_KTGT*s2)-154.0_KTGT)+34.0_KTGT)    !! (dlon/2)**7
      Cd(2) = s*(s2*(126.0_KTGT*s2-210.0_KTGT)+84.0_KTGT)    !! (dlon/2)**5
      Cd(3) = s*(210.0_KTGT-210.0_KTGT*s2)    !! (dlon/2)**3
      Cd(4) = 630.0_KTGT*s    !! (dlon/2)**1

      !! [c tan(dlat/2)]**1
      !!    (-630*c*dlonh^7*s^6)+1050*c*dlonh^7*s^4+630*c*dlonh^5*s^4-462*c*dlonh^7*s^2-630*c*dlonh^5*s^2-630*c*dlonh^3
      !!   *s^2+34*c*dlonh^7+84*c*dlonh^5+210*c*dlonh^3+630*c*dlonh 
      Cd(5) = 315.0_KTGT   !! denom
      Cd(6) = s2*(s2*(1050.0_KTGT-630.0_KTGT*s2)-462.0_KTGT)+34.0_KTGT    !! (dlon/2)**7
      Cd(7) = s2*(630.0_KTGT*s2-630.0_KTGT)+84.0_KTGT    !! (dlon/2)**5
      Cd(8) = 210.0_KTGT-630.0_KTGT*s2    !! (dlon/2)**3
      Cd(9) = 630.0_KTGT    !! (dlon/2)**1

      !! [c tan(dlat/2)]**2
      !!    (-90*c^2*dlonh^7*s^5)+100*c^2*dlonh^7*s^3+60*c^2*dlonh^5*s^3-22*c^2*dlonh^7*s-30*c^2*dlonh^5*s-30*c^2*dlonh^3
      !!   *s 
      Cd(10) = 15.0_KTGT   !! denom
      Cd(11) = s*(s2*(100.0_KTGT-90.0_KTGT*s2)-22.0_KTGT)    !! (dlon/2)**7
      Cd(12) = s*(60.0_KTGT*s2-30.0_KTGT)    !! (dlon/2)**5
      Cd(13) = -30.0_KTGT*s    !! (dlon/2)**3

      !! [c tan(dlat/2)]**3
      !!    (-450*c^3*dlonh^7*s^4)+300*c^3*dlonh^7*s^2+180*c^3*dlonh^5*s^2-22*c^3*dlonh^7-30*c^3*dlonh^5-30*c^3*dlonh^3 
      Cd(14) = 45.0_KTGT   !! denom
      Cd(15) = s2*(300.0_KTGT-450.0_KTGT*s2)-22.0_KTGT    !! (dlon/2)**7
      Cd(16) = 180.0_KTGT*s2-30.0_KTGT    !! (dlon/2)**5
      Cd(17) = -30.0_KTGT    !! (dlon/2)**3

      !! [c tan(dlat/2)]**4
      !!    (-30*c^4*dlonh^7*s^3)+10*c^4*dlonh^7*s+6*c^4*dlonh^5*s 
      Cd(18) = 3.0_KTGT   !! denom
      Cd(19) = s*(10.0_KTGT-30.0_KTGT*s2)    !! (dlon/2)**7
      Cd(20) = 6.0_KTGT*s    !! (dlon/2)**5

      !! [c tan(dlat/2)]**5
      !!    (-90*c^5*dlonh^7*s^2)+10*c^5*dlonh^7+6*c^5*dlonh^5 
      Cd(21) = 15.0_KTGT   !! denom
      Cd(22) = 10.0_KTGT-90.0_KTGT*s2    !! (dlon/2)**7
      Cd(23) = 6.0_KTGT    !! (dlon/2)**5

      !! [c tan(dlat/2)]**6
      !!    -2*c^6*dlonh^7*s 
      Cd(24) = 1.0_KTGT   !! denom
      Cd(25) = -2.0_KTGT*s    !! (dlon/2)**7

      !! [c tan(dlat/2)]**7
      !!    -2*c^7*dlonh^7 
      Cd(26) = 7.0_KTGT   !! denom
      Cd(27) = -2.0_KTGT    !! (dlon/2)**7

      !! subtract pmem area
      if (rel) Cd(4) = 0.0_KTGT
