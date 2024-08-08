      epsc = 1.0_KTGT
      C2(0) = ((((-11.0_KTGT * eps2 - 28.0_KTGT) * eps2 - 192.0_KTGT) * eps2 + 256.0_KTGT) * epsc)  &
       & / 256.0_KTGT
      epsc = epsc * eps
      C2(1) = (((1.0_KTGT * eps2 + 2.0_KTGT) * eps2 + 16.0_KTGT) * epsc) / 32.0_KTGT
      epsc = epsc * eps
      C2(2) = (((35.0_KTGT * eps2 + 64.0_KTGT) * eps2 + 384.0_KTGT) * epsc) / 2048.0_KTGT
      epsc = epsc * eps
      C2(3) = ((15.0_KTGT * eps2 + 80.0_KTGT) * epsc) / 768.0_KTGT
      epsc = epsc * eps
      C2(4) = ((7.0_KTGT * eps2 + 35.0_KTGT) * epsc) / 512.0_KTGT
      epsc = epsc * eps
      C2(5) = (63.0_KTGT * epsc) / 1280.0_KTGT
      epsc = epsc * eps
      C2(6) = (77.0_KTGT * epsc) / 2048.0_KTGT
