      !! ORDER: 7 (24 terms)

      !! [c tan(dlat/2)]**0
      !!    (-90*dlonh^7*s^7)+210*dlonh^7*s^5+126*dlonh^5*s^5-154*dlonh^7*s^3-210*dlonh^5*s^3-210*dlonh^3*s^3+34*dlonh^7
      !!   *s+84*dlonh^5*s+210*dlonh^3*s+630*dlonh*s 
      Cc(0) = 315.0_KTGT   !! denom
      Cc(1) = s*(s2*(s2*(210.0_KTGT-90.0_KTGT*s2)-154.0_KTGT)+34.0_KTGT)    !! (dlon/2)**7
      Cc(2) = s*(s2*(126.0_KTGT*s2-210.0_KTGT)+84.0_KTGT)    !! (dlon/2)**5
      Cc(3) = s*(210.0_KTGT-210.0_KTGT*s2)    !! (dlon/2)**3
      Cc(4) = 630.0_KTGT*s    !! (dlon/2)**1

      !! [c tan(dlat/2)]**1
      !!    30*c*dlonh^5*s^4-30*c*dlonh^5*s^2-30*c*dlonh^3*s^2+4*c*dlonh^5+10*c*dlonh^3+30*c*dlonh 
      Cc(5) = 15.0_KTGT   !! denom
      Cc(6) = s2*(30.0_KTGT*s2-30.0_KTGT)+4.0_KTGT    !! (dlon/2)**5
      Cc(7) = 10.0_KTGT-30.0_KTGT*s2    !! (dlon/2)**3
      Cc(8) = 30.0_KTGT    !! (dlon/2)**1

      !! [c tan(dlat/2)]**2
      !!    4*c^2*dlonh^5*s^3-2*c^2*dlonh^5*s-2*c^2*dlonh^3*s 
      Cc(9) = 1.0_KTGT   !! denom
      Cc(10) = s*(4.0_KTGT*s2-2.0_KTGT)    !! (dlon/2)**5
      Cc(11) = -2.0_KTGT*s    !! (dlon/2)**3

      !! [c tan(dlat/2)]**3
      !!    -2*c^3*dlonh^3 
      Cc(12) = 3.0_KTGT   !! denom
      Cc(13) = -2.0_KTGT    !! (dlon/2)**3

      !! subtract pmem area
      if (rel) Cc(4) = 0.0_KTGT
