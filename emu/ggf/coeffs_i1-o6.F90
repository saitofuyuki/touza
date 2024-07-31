!!! 1-x
      epsc = 1.0_KTGT
      C1(0) = ((((1.0_KTGT * eps2 + 4.0_KTGT) * eps2 + 64.0_KTGT) * eps2 - 0.0_KTGT) * epsc)  &
       & / 256.0_KTGT
      epsc = epsc * eps
      C1(1) = ((((19.0_KTGT * eps2 - 64.0_KTGT) * eps2 + 384.0_KTGT) * eps2 - 1024.0_KTGT) * epsc)  &
       & / 2048.0_KTGT
      epsc = epsc * eps
      C1(2) = (((-9.0_KTGT * eps2 + 64.0_KTGT) * eps2 - 128.0_KTGT) * epsc) / 2048.0_KTGT
      epsc = epsc * eps
      C1(3) = (((-9.0_KTGT * eps2 + 72.0_KTGT) * eps2 - 128.0_KTGT) * epsc) / 6144.0_KTGT
      epsc = epsc * eps
      C1(4) = ((3.0_KTGT * eps2 - 5.0_KTGT) * epsc) / 512.0_KTGT
      epsc = epsc * eps
      C1(5) = ((35.0_KTGT * eps2 - 56.0_KTGT) * epsc) / 10240.0_KTGT
      epsc = epsc * eps
      C1(6) = (-7.0_KTGT * epsc) / 2048.0_KTGT
      epsc = epsc * eps
      C1(7) = (-33.0_KTGT * epsc) / 14336.0_KTGT
