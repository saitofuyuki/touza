!!! 1/(1-x)
      epsc = 1.0_KTGT
      C2a(0) = ((((25.0_KTGT * eps2 + 36.0_KTGT) * eps2 + 64.0_KTGT) * eps2 - 0.0_KTGT) * epsc)  &
       & / 256.0_KTGT
      epsc = epsc * eps
      C2a(1) = ((((41.0_KTGT * eps2 + 64.0_KTGT) * eps2 + 128.0_KTGT) * eps2 + 1024.0_KTGT)  &
       & * epsc) / 2048.0_KTGT
      epsc = epsc * eps
      C2a(2) = (((35.0_KTGT * eps2 + 64.0_KTGT) * eps2 + 384.0_KTGT) * epsc) / 2048.0_KTGT
      epsc = epsc * eps
      C2a(3) = (((69.0_KTGT * eps2 + 120.0_KTGT) * eps2 + 640.0_KTGT) * epsc) / 6144.0_KTGT
      epsc = epsc * eps
      C2a(4) = ((7.0_KTGT * eps2 + 35.0_KTGT) * epsc) / 512.0_KTGT
      epsc = epsc * eps
      C2a(5) = ((105.0_KTGT * eps2 + 504.0_KTGT) * epsc) / 10240.0_KTGT
      epsc = epsc * eps
      C2a(6) = (77.0_KTGT * epsc) / 2048.0_KTGT
      epsc = epsc * eps
      C2a(7) = (429.0_KTGT * epsc) / 14336.0_KTGT
