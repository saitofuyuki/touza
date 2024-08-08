!!! 1-x
      epsc = 1.0_KTGT
      C1(0) = (((((25.0_KTGT * eps2 + 64.0_KTGT) * eps2 + 256.0_KTGT) * eps2 + 4096.0_KTGT)  &
       & * eps2 - 0.0_KTGT) * epsc) / 16384.0_KTGT
      epsc = epsc * eps
      C1(1) = (((((-3.0_KTGT * eps2 + 38.0_KTGT) * eps2 - 128.0_KTGT) * eps2 + 768.0_KTGT)  &
       & * eps2 - 2048.0_KTGT) * epsc) / 4096.0_KTGT
      epsc = epsc * eps
      C1(2) = ((((7.0_KTGT * eps2 - 18.0_KTGT) * eps2 + 128.0_KTGT) * eps2 - 256.0_KTGT) * epsc)  &
       & / 4096.0_KTGT
      epsc = epsc * eps
      C1(3) = ((((17.0_KTGT * eps2 - 36.0_KTGT) * eps2 + 288.0_KTGT) * eps2 - 512.0_KTGT) * epsc)  &
       & / 24576.0_KTGT
      epsc = epsc * eps
      C1(4) = (((-11.0_KTGT * eps2 + 96.0_KTGT) * eps2 - 160.0_KTGT) * epsc) / 16384.0_KTGT
      epsc = epsc * eps
      C1(5) = (((-15.0_KTGT * eps2 + 140.0_KTGT) * eps2 - 224.0_KTGT) * epsc) / 40960.0_KTGT
      epsc = epsc * eps
      C1(6) = ((9.0_KTGT * eps2 - 14.0_KTGT) * epsc) / 4096.0_KTGT
      epsc = epsc * eps
      C1(7) = ((693.0_KTGT * eps2 - 1056.0_KTGT) * epsc) / 458752.0_KTGT
      epsc = epsc * eps
      C1(8) = (-429.0_KTGT * epsc) / 262144.0_KTGT
      epsc = epsc * eps
      C1(9) = (-715.0_KTGT * epsc) / 589824.0_KTGT
