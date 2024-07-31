      !! ORDER: 3 (7 terms)

      !! [c tan(dlat/2)]**0
      !!    (-2*dlonh^3*s^3)+2*dlonh^3*s+6*dlonh*s 
      Cd(0) = 3.0_KTGT   !! denom
      Cd(1) = s*(2.0_KTGT-2.0_KTGT*s2)    !! (dlon/2)**3
      Cd(2) = 6.0_KTGT*s    !! (dlon/2)**1

      !! [c tan(dlat/2)]**1
      !!    (-6*c*dlonh^3*s^2)+2*c*dlonh^3+6*c*dlonh 
      Cd(3) = 3.0_KTGT   !! denom
      Cd(4) = 2.0_KTGT-6.0_KTGT*s2    !! (dlon/2)**3
      Cd(5) = 6.0_KTGT    !! (dlon/2)**1

      !! [c tan(dlat/2)]**2
      !!    -2*c^2*dlonh^3*s 
      Cd(6) = 1.0_KTGT   !! denom
      Cd(7) = -2.0_KTGT*s    !! (dlon/2)**3

      !! [c tan(dlat/2)]**3
      !!    -2*c^3*dlonh^3 
      Cd(8) = 3.0_KTGT   !! denom
      Cd(9) = -2.0_KTGT    !! (dlon/2)**3

      !! subtract pmem area
      if (rel) Cd(2) = 0.0_KTGT
