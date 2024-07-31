      !! ORDER: 2 (4 terms)

      !! [c tan(dlat/2)]**0
      !!    2*dlonh*s 
      Cc(0) = 1.0_KTGT   !! denom

      !! [c tan(dlat/2)]**1
      !!    2*c*dlonh 
      Cc(1) = 1.0_KTGT   !! denom
      Cc(2) = 2.0_KTGT    !! (dlon/2)**1
      Cc(3) = 2.0_KTGT*s    !! (dlon/2)**0

      !! subtract pmem area
      if (rel) Cc() = 0.0_KTGT
