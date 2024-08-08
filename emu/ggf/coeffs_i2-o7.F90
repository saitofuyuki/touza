      epsc = 1.0_KTGT
      C2(0) = (((((-375.0_KTGT * eps2 - 704.0_KTGT) * eps2 - 1792.0_KTGT) * eps2 - 12288.0_KTGT)  &
       & * eps2 + 16384.0_KTGT) * epsc) / 16384.0_KTGT
      epsc = epsc * eps
      C2(1) = ((((41.0_KTGT * eps2 + 64.0_KTGT) * eps2 + 128.0_KTGT) * eps2 + 1024.0_KTGT) * epsc)  &
       & / 2048.0_KTGT
      epsc = epsc * eps
      C2(2) = ((((47.0_KTGT * eps2 + 70.0_KTGT) * eps2 + 128.0_KTGT) * eps2 + 768.0_KTGT) * epsc)  &
       & / 4096.0_KTGT
      epsc = epsc * eps
      C2(3) = (((69.0_KTGT * eps2 + 120.0_KTGT) * eps2 + 640.0_KTGT) * epsc) / 6144.0_KTGT
      epsc = epsc * eps
      C2(4) = (((133.0_KTGT * eps2 + 224.0_KTGT) * eps2 + 1120.0_KTGT) * epsc) / 16384.0_KTGT
      epsc = epsc * eps
      C2(5) = ((105.0_KTGT * eps2 + 504.0_KTGT) * epsc) / 10240.0_KTGT
      epsc = epsc * eps
      C2(6) = ((33.0_KTGT * eps2 + 154.0_KTGT) * epsc) / 4096.0_KTGT
      epsc = epsc * eps
      C2(7) = (429.0_KTGT * epsc) / 14336.0_KTGT
      epsc = epsc * eps
      C2(8) = (6435.0_KTGT * epsc) / 262144.0_KTGT
