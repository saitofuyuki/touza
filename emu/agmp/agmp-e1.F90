      !! ORDER: 1 (4 terms)

      !! [0] sin[1 lat]
      Ce(0) = 1.0_KTGT   !! denom
      Ce(1) = 2.0_KTGT   !! (dlon/2)**1

      !! [1] cos[1 lat] tan(dlat/2)
      Ce(2) = 1.0_KTGT   !! denom
      Ce(3) = 2.0_KTGT   !! (dlon/2)**1

      !! subtract pmem area
      if (rel) Ce(1) = 0.0_KTGT

      dlhf = dlh
      C(0) = Ce(1) * dlhf / Ce(0)
      C(1) = Ce(3) * dlhf / Ce(2)
