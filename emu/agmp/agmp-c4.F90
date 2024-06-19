      !! ORDER: 4 (6 terms)

      !! [c tan(dlat/2)]**0
      !!    (-2*dlonh^3*s^3)+2*dlonh^3*s+6*dlonh*s 
      Cc(0) = 3.0_KTGT   !! denom
      Cc(1) = s*(2.0_KTGT-2.0_KTGT*s2)    !! (dlon/2)**3
      Cc(2) = 6.0_KTGT*s    !! (dlon/2)**1

      !! [c tan(dlat/2)]**1
      !!    (-6*c*dlonh^3*s^2)+2*c*dlonh^3+6*c*dlonh 
      Cc(3) = 3.0_KTGT   !! denom
      Cc(4) = 2.0_KTGT-6.0_KTGT*s2    !! (dlon/2)**3
      Cc(5) = 6.0_KTGT    !! (dlon/2)**1

      !! subtract pmem area
      if (rel) Cc(2) = 0.0_KTGT
