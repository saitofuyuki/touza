      !! ORDER: 6 (18 terms)

      !! [c tan(dlat/2)]**0
      !!    6*dlonh^5*s^5-10*dlonh^5*s^3-10*dlonh^3*s^3+4*dlonh^5*s+10*dlonh^3*s+30*dlonh*s 
      Cd(0) = 15.0_KTGT   !! denom
      Cd(1) = s*(s2*(6.0_KTGT*s2-10.0_KTGT)+4.0_KTGT)    !! (dlon/2)**5
      Cd(2) = s*(10.0_KTGT-10.0_KTGT*s2)    !! (dlon/2)**3
      Cd(3) = 30.0_KTGT*s    !! (dlon/2)**1

      !! [c tan(dlat/2)]**1
      !!    30*c*dlonh^5*s^4-30*c*dlonh^5*s^2-30*c*dlonh^3*s^2+4*c*dlonh^5+10*c*dlonh^3+30*c*dlonh 
      Cd(4) = 15.0_KTGT   !! denom
      Cd(5) = s2*(30.0_KTGT*s2-30.0_KTGT)+4.0_KTGT    !! (dlon/2)**5
      Cd(6) = 10.0_KTGT-30.0_KTGT*s2    !! (dlon/2)**3
      Cd(7) = 30.0_KTGT    !! (dlon/2)**1

      !! [c tan(dlat/2)]**2
      !!    4*c^2*dlonh^5*s^3-2*c^2*dlonh^5*s-2*c^2*dlonh^3*s 
      Cd(8) = 1.0_KTGT   !! denom
      Cd(9) = s*(4.0_KTGT*s2-2.0_KTGT)    !! (dlon/2)**5
      Cd(10) = -2.0_KTGT*s    !! (dlon/2)**3

      !! [c tan(dlat/2)]**3
      !!    12*c^3*dlonh^5*s^2-2*c^3*dlonh^5-2*c^3*dlonh^3 
      Cd(11) = 3.0_KTGT   !! denom
      Cd(12) = 12.0_KTGT*s2-2.0_KTGT    !! (dlon/2)**5
      Cd(13) = -2.0_KTGT    !! (dlon/2)**3

      !! [c tan(dlat/2)]**4
      !!    2*c^4*dlonh^5*s 
      Cd(14) = 1.0_KTGT   !! denom
      Cd(15) = 2.0_KTGT*s    !! (dlon/2)**5

      !! [c tan(dlat/2)]**5
      !!    2*c^5*dlonh^5 
      Cd(16) = 5.0_KTGT   !! denom
      Cd(17) = 2.0_KTGT    !! (dlon/2)**5

      !! subtract pmem area
      if (rel) Cd(3) = 0.0_KTGT
