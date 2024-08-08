!!! 1/(1-x)
      epsc = 1.0_KTGT
      C2a(0) = ((((25.0_KTGT * eps2 + 36.0_KTGT) * eps2 + 64.0_KTGT) * eps2 - 0.0_KTGT) * epsc)  &
       & / 256.0_KTGT
      epsc = epsc * eps
      C2a(1) = (((1.0_KTGT * eps2 + 2.0_KTGT) * eps2 + 16.0_KTGT) * epsc) / 32.0_KTGT
      epsc = epsc * eps
      C2a(2) = (((35.0_KTGT * eps2 + 64.0_KTGT) * eps2 + 384.0_KTGT) * epsc) / 2048.0_KTGT
      epsc = epsc * eps
      C2a(3) = ((15.0_KTGT * eps2 + 80.0_KTGT) * epsc) / 768.0_KTGT
      epsc = epsc * eps
      C2a(4) = ((7.0_KTGT * eps2 + 35.0_KTGT) * epsc) / 512.0_KTGT
      epsc = epsc * eps
      C2a(5) = (63.0_KTGT * epsc) / 1280.0_KTGT
      epsc = epsc * eps
      C2a(6) = (77.0_KTGT * epsc) / 2048.0_KTGT
