!!! 1-x
      epsc = 1.0_KTGT
      C1(0) = ((((((49.0_KTGT * eps2 + 100.0_KTGT) * eps2 + 256.0_KTGT) * eps2 + 1024.0_KTGT)  &
       & * eps2 + 16384.0_KTGT) * eps2 - 0.0_KTGT) * epsc) / 65536.0_KTGT
      epsc = epsc * eps
      C1(1) = (((((-3.0_KTGT * eps2 + 38.0_KTGT) * eps2 - 128.0_KTGT) * eps2 + 768.0_KTGT)  &
       & * eps2 - 2048.0_KTGT) * epsc) / 4096.0_KTGT
      epsc = epsc * eps
      C1(2) = (((((1.0_KTGT * eps2 + 112.0_KTGT) * eps2 - 288.0_KTGT) * eps2 + 2048.0_KTGT)  &
       & * eps2 - 4096.0_KTGT) * epsc) / 65536.0_KTGT
      epsc = epsc * eps
      C1(3) = ((((17.0_KTGT * eps2 - 36.0_KTGT) * eps2 + 288.0_KTGT) * eps2 - 512.0_KTGT) * epsc)  &
       & / 24576.0_KTGT
      epsc = epsc * eps
      C1(4) = ((((6.0_KTGT * eps2 - 11.0_KTGT) * eps2 + 96.0_KTGT) * eps2 - 160.0_KTGT) * epsc)  &
       & / 16384.0_KTGT
      epsc = epsc * eps
      C1(5) = (((-15.0_KTGT * eps2 + 140.0_KTGT) * eps2 - 224.0_KTGT) * epsc) / 40960.0_KTGT
      epsc = epsc * eps
      C1(6) = (((-117.0_KTGT * eps2 + 1152.0_KTGT) * eps2 - 1792.0_KTGT) * epsc) / 524288.0_KTGT
      epsc = epsc * eps
      C1(7) = ((693.0_KTGT * eps2 - 1056.0_KTGT) * epsc) / 458752.0_KTGT
      epsc = epsc * eps
      C1(8) = ((286.0_KTGT * eps2 - 429.0_KTGT) * epsc) / 262144.0_KTGT
      epsc = epsc * eps
      C1(9) = (-715.0_KTGT * epsc) / 589824.0_KTGT
      epsc = epsc * eps
      C1(10) = (-2431.0_KTGT * epsc) / 2621440.0_KTGT
