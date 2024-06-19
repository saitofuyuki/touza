!!! x+1
      epsc = 1.0_KTGT
      C2b(0) = (((((-375.0_KTGT * eps2 - 704.0_KTGT) * eps2 - 1792.0_KTGT) * eps2 - 12288.0_KTGT)  &
       & * eps2 - 0.0_KTGT) * epsc) / 16384.0_KTGT
      epsc = epsc * eps
      C2b(1) = (((((59.0_KTGT * eps2 + 82.0_KTGT) * eps2 + 128.0_KTGT) * eps2 + 256.0_KTGT)  &
       & * eps2 + 2048.0_KTGT) * epsc) / 4096.0_KTGT
      epsc = epsc * eps
      C2b(2) = ((((47.0_KTGT * eps2 + 70.0_KTGT) * eps2 + 128.0_KTGT) * eps2 + 768.0_KTGT) * epsc)  &
       & / 4096.0_KTGT
      epsc = epsc * eps
      C2b(3) = ((((191.0_KTGT * eps2 + 276.0_KTGT) * eps2 + 480.0_KTGT) * eps2 + 2560.0_KTGT)  &
       & * epsc) / 24576.0_KTGT
      epsc = epsc * eps
      C2b(4) = (((133.0_KTGT * eps2 + 224.0_KTGT) * eps2 + 1120.0_KTGT) * epsc) / 16384.0_KTGT
      epsc = epsc * eps
      C2b(5) = (((255.0_KTGT * eps2 + 420.0_KTGT) * eps2 + 2016.0_KTGT) * epsc) / 40960.0_KTGT
      epsc = epsc * eps
      C2b(6) = ((33.0_KTGT * eps2 + 154.0_KTGT) * epsc) / 4096.0_KTGT
      epsc = epsc * eps
      C2b(7) = ((3003.0_KTGT * eps2 + 13728.0_KTGT) * epsc) / 458752.0_KTGT
      epsc = epsc * eps
      C2b(8) = (6435.0_KTGT * epsc) / 262144.0_KTGT
      epsc = epsc * eps
      C2b(9) = (12155.0_KTGT * epsc) / 589824.0_KTGT
