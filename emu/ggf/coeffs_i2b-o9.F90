!!! x+1
      epsc = 1.0_KTGT
      C2b(0) = ((((((-931.0_KTGT * eps2 - 1500.0_KTGT) * eps2 - 2816.0_KTGT) * eps2 - 7168.0_KTGT)  &
       & * eps2 - 49152.0_KTGT) * eps2 - 0.0_KTGT) * epsc) / 65536.0_KTGT
      epsc = epsc * eps
      C2b(1) = (((((59.0_KTGT * eps2 + 82.0_KTGT) * eps2 + 128.0_KTGT) * eps2 + 256.0_KTGT)  &
       & * eps2 + 2048.0_KTGT) * epsc) / 4096.0_KTGT
      epsc = epsc * eps
      C2b(2) = (((((557.0_KTGT * eps2 + 752.0_KTGT) * eps2 + 1120.0_KTGT) * eps2 + 2048.0_KTGT)  &
       & * eps2 + 12288.0_KTGT) * epsc) / 65536.0_KTGT
      epsc = epsc * eps
      C2b(3) = ((((191.0_KTGT * eps2 + 276.0_KTGT) * eps2 + 480.0_KTGT) * eps2 + 2560.0_KTGT)  &
       & * epsc) / 24576.0_KTGT
      epsc = epsc * eps
      C2b(4) = ((((94.0_KTGT * eps2 + 133.0_KTGT) * eps2 + 224.0_KTGT) * eps2 + 1120.0_KTGT)  &
       & * epsc) / 16384.0_KTGT
      epsc = epsc * eps
      C2b(5) = (((255.0_KTGT * eps2 + 420.0_KTGT) * eps2 + 2016.0_KTGT) * epsc) / 40960.0_KTGT
      epsc = epsc * eps
      C2b(6) = (((2607.0_KTGT * eps2 + 4224.0_KTGT) * eps2 + 19712.0_KTGT) * epsc) / 524288.0_KTGT
      epsc = epsc * eps
      C2b(7) = ((3003.0_KTGT * eps2 + 13728.0_KTGT) * epsc) / 458752.0_KTGT
      epsc = epsc * eps
      C2b(8) = ((1430.0_KTGT * eps2 + 6435.0_KTGT) * epsc) / 262144.0_KTGT
      epsc = epsc * eps
      C2b(9) = (12155.0_KTGT * epsc) / 589824.0_KTGT
      epsc = epsc * eps
      C2b(10) = (46189.0_KTGT * epsc) / 2621440.0_KTGT
