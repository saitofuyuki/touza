      !! ORDER: 6 (12 terms)

      !! [c tan(dlat/2)]**0
      !!    6*dlonh^5*s^5-10*dlonh^5*s^3-10*dlonh^3*s^3+4*dlonh^5*s+10*dlonh^3*s+30*dlonh*s 
      Cc(0) = 15.0_KTGT   !! denom
      Cc(1) = s*(s2*(6.0_KTGT*s2-10.0_KTGT)+4.0_KTGT)    !! (dlon/2)**5
      Cc(2) = s*(10.0_KTGT-10.0_KTGT*s2)    !! (dlon/2)**3
      Cc(3) = 30.0_KTGT*s    !! (dlon/2)**1

      !! [c tan(dlat/2)]**1
      !!    30*c*dlonh^5*s^4-30*c*dlonh^5*s^2-30*c*dlonh^3*s^2+4*c*dlonh^5+10*c*dlonh^3+30*c*dlonh 
      Cc(4) = 15.0_KTGT   !! denom
      Cc(5) = s2*(30.0_KTGT*s2-30.0_KTGT)+4.0_KTGT    !! (dlon/2)**5
      Cc(6) = 10.0_KTGT-30.0_KTGT*s2    !! (dlon/2)**3
      Cc(7) = 30.0_KTGT    !! (dlon/2)**1

      !! [c tan(dlat/2)]**2
      !!    -2*c^2*dlonh^3*s 
      Cc(8) = 1.0_KTGT   !! denom
      Cc(9) = -2.0_KTGT*s    !! (dlon/2)**3

      !! [c tan(dlat/2)]**3
      !!    -2*c^3*dlonh^3 
      Cc(10) = 3.0_KTGT   !! denom
      Cc(11) = -2.0_KTGT    !! (dlon/2)**3

      !! subtract pmem area
      if (rel) Cc(3) = 0.0_KTGT
