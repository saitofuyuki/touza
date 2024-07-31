      !! ORDER: 12 (30 terms)

      !! [c tan(dlat/2)]**0
      !!    (-28350*dlonh^11*s^11)+103950*dlonh^11*s^9+34650*dlonh^9*s^9-145530*dlonh^11*s^7-103950*dlonh^9*s^7-44550
      !!   *dlonh^7*s^7+95370*dlonh^11*s^5+110880*dlonh^9*s^5+103950*dlonh^7*... 
      Cc(0) = 155925.0_KTGT   !! denom
      Cc(1) = s*(s2*(s2*(s2*(s2*(103950.0_KTGT-28350.0_KTGT &
       & *s2)-145530.0_KTGT)+95370.0_KTGT)-28204.0_KTGT)+2764.0_KTGT)    !! (dlon/2)**11
      Cc(2) = s*(s2*(s2*(s2*(34650.0_KTGT*s2-103950.0_KTGT)+110880.0_KTGT)-48400.0_KTGT)+6820.0_KTGT)    !! (dlon/2)**9
      Cc(3) = s*(s2*(s2*(103950.0_KTGT-44550.0_KTGT*s2)-76230.0_KTGT)+16830.0_KTGT)    !! (dlon/2)**7
      Cc(4) = s*(s2*(62370.0_KTGT*s2-103950.0_KTGT)+41580.0_KTGT)    !! (dlon/2)**5
      Cc(5) = s*(103950.0_KTGT-103950.0_KTGT*s2)    !! (dlon/2)**3
      Cc(6) = 311850.0_KTGT*s    !! (dlon/2)**1

      !! [c tan(dlat/2)]**1
      !!    (-311850*c*dlonh^11*s^10)+935550*c*dlonh^11*s^8+311850*c*dlonh^9*s^8-1018710*c*dlonh^11*s^6-727650*c*dlonh^9
      !!   *s^6-311850*c*dlonh^7*s^6+476850*c*dlonh^11*s^4+554400*c*dlonh^... 
      Cc(7) = 155925.0_KTGT   !! denom
      Cc(8) = s2*(s2*(s2*(s2*(935550.0_KTGT-311850.0_KTGT &
       & *s2)-1018710.0_KTGT)+476850.0_KTGT)-84612.0_KTGT)+2764.0_KTGT    !! (dlon/2)**11
      Cc(9) = s2*(s2*(s2*(311850.0_KTGT*s2-727650.0_KTGT)+554400.0_KTGT)-145200.0_KTGT)+6820.0_KTGT    !! (dlon/2)**9
      Cc(10) = s2*(s2*(519750.0_KTGT-311850.0_KTGT*s2)-228690.0_KTGT)+16830.0_KTGT    !! (dlon/2)**7
      Cc(11) = s2*(311850.0_KTGT*s2-311850.0_KTGT)+41580.0_KTGT    !! (dlon/2)**5
      Cc(12) = 103950.0_KTGT-311850.0_KTGT*s2    !! (dlon/2)**3
      Cc(13) = 311850.0_KTGT    !! (dlon/2)**1

      !! [c tan(dlat/2)]**2
      !!    7560*c^2*dlonh^9*s^7-13230*c^2*dlonh^9*s^5-5670*c^2*dlonh^7*s^5+6720*c^2*dlonh^9*s^3+6300*c^2*dlonh^7*s^3+3780
      !!   *c^2*dlonh^5*s^3-880*c^2*dlonh^9*s-1386*c^2*dlonh^7*s-1890*c^... 
      Cc(14) = 945.0_KTGT   !! denom
      Cc(15) = s*(s2*(s2*(7560.0_KTGT*s2-13230.0_KTGT)+6720.0_KTGT)-880.0_KTGT)    !! (dlon/2)**9
      Cc(16) = s*(s2*(6300.0_KTGT-5670.0_KTGT*s2)-1386.0_KTGT)    !! (dlon/2)**7
      Cc(17) = s*(3780.0_KTGT*s2-1890.0_KTGT)    !! (dlon/2)**5
      Cc(18) = -1890.0_KTGT*s    !! (dlon/2)**3

      !! [c tan(dlat/2)]**3
      !!    52920*c^3*dlonh^9*s^6-66150*c^3*dlonh^9*s^4-28350*c^3*dlonh^7*s^4+20160*c^3*dlonh^9*s^2+18900*c^3*dlonh^7
      !!   *s^2+11340*c^3*dlonh^5*s^2-880*c^3*dlonh^9-1386*c^3*dlonh^7-1890*c... 
      Cc(19) = 2835.0_KTGT   !! denom
      Cc(20) = s2*(s2*(52920.0_KTGT*s2-66150.0_KTGT)+20160.0_KTGT)-880.0_KTGT    !! (dlon/2)**9
      Cc(21) = s2*(18900.0_KTGT-28350.0_KTGT*s2)-1386.0_KTGT    !! (dlon/2)**7
      Cc(22) = 11340.0_KTGT*s2-1890.0_KTGT    !! (dlon/2)**5
      Cc(23) = -1890.0_KTGT    !! (dlon/2)**3

      !! [c tan(dlat/2)]**4
      !!    (-30*c^4*dlonh^7*s^3)+10*c^4*dlonh^7*s+6*c^4*dlonh^5*s 
      Cc(24) = 3.0_KTGT   !! denom
      Cc(25) = s*(10.0_KTGT-30.0_KTGT*s2)    !! (dlon/2)**7
      Cc(26) = 6.0_KTGT*s    !! (dlon/2)**5

      !! [c tan(dlat/2)]**5
      !!    (-90*c^5*dlonh^7*s^2)+10*c^5*dlonh^7+6*c^5*dlonh^5 
      Cc(27) = 15.0_KTGT   !! denom
      Cc(28) = 10.0_KTGT-90.0_KTGT*s2    !! (dlon/2)**7
      Cc(29) = 6.0_KTGT    !! (dlon/2)**5

      !! subtract pmem area
      if (rel) Cc(6) = 0.0_KTGT
