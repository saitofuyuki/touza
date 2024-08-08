      !! ORDER: 5 (15 terms)

      !! [c tan(dlat/2)]**0
      !!    6*dlonh^5*s^5-10*dlonh^5*s^3-10*dlonh^3*s^3+4*dlonh^5*s+10*dlonh^3*s+30*dlonh*s 
      Cc(0) = 15.0_KTGT   !! denom
      Cc(1) = s*(s2*(6.0_KTGT*s2-10.0_KTGT)+4.0_KTGT)    !! (dlon/2)**5
      Cc(2) = s*(10.0_KTGT-10.0_KTGT*s2)    !! (dlon/2)**3
      Cc(3) = 30.0_KTGT*s    !! (dlon/2)**1

      !! [c tan(dlat/2)]**1
      !!    (-6*c*dlonh^3*s^2)+2*c*dlonh^3+6*c*dlonh 
      Cc(4) = 3.0_KTGT   !! denom
      Cc(5) = 2.0_KTGT-6.0_KTGT*s2    !! (dlon/2)**3
      Cc(6) = 6.0_KTGT    !! (dlon/2)**1

      !! [c tan(dlat/2)]**2
      !!    -2*c^2*dlonh^3*s 
      Cc(7) = 1.0_KTGT   !! denom
      Cc(8) = -2.0_KTGT*s    !! (dlon/2)**3

      !! subtract pmem area
      if (rel) Cc(3) = 0.0_KTGT
