!!! 1-x
      epsc = 1.0_KTGT
      C1(0) = ((((1.0_KTGT * eps2 + 4.0_KTGT) * eps2 + 64.0_KTGT) * eps2 - 0.0_KTGT) * epsc)  &
       & / 256.0_KTGT
      epsc = epsc * eps
      C1(1) = (((-1.0_KTGT * eps2 + 6.0_KTGT) * eps2 - 16.0_KTGT) * epsc) / 32.0_KTGT
      epsc = epsc * eps
      C1(2) = (((-9.0_KTGT * eps2 + 64.0_KTGT) * eps2 - 128.0_KTGT) * epsc) / 2048.0_KTGT
      epsc = epsc * eps
      C1(3) = ((9.0_KTGT * eps2 - 16.0_KTGT) * epsc) / 768.0_KTGT
      epsc = epsc * eps
      C1(4) = ((3.0_KTGT * eps2 - 5.0_KTGT) * epsc) / 512.0_KTGT
      epsc = epsc * eps
      C1(5) = (-7.0_KTGT * epsc) / 1280.0_KTGT
      epsc = epsc * eps
      C1(6) = (-7.0_KTGT * epsc) / 2048.0_KTGT
