!!!_! test_calendar.F90 - touza/calendar test program
! Maintainer: SAITO Fuyuki
! Created: Mar 28 2012
#define TIME_STAMP 'Time-stamp: <2025/07/10 12:25:08 fuyuki test_calendar.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020-2025
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
!!!_! Includes
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_cal.h"
#include "touza_std.h"
!!!_* Macros
#ifndef   OPT_USE_BASE_UCALN
#  define OPT_USE_BASE_UCALN 0
#endif
#ifndef   WITH_TEST8_CCC2YH
#  define WITH_TEST8_CCC2YH 0  /* CCC2YH disabled for a while */
#endif
!!!_@ test_calendar - test program
program test_calendar_suite
#if OPT_USE_BASE_UCALN == 0
  use TOUZA_Cal_miroc
#endif /* OPT_USE_BASE_UCALN == 0 */
  use TOUZA_Std, only: KDBL, KFLT, ifpar => uin, jfpar => uout
  implicit none
  integer,parameter :: KRC = OPT_KIND_REAL

  integer :: iy_start, iy_end, iy_step
  integer :: im_start, im_end, im_step
  integer :: id_start, id_end, id_step

  integer :: im_start_ym, im_end_ym, im_step_ym
  integer :: id_start_ym, id_end_ym, id_step_ym

  real(kind=KRC) :: ss_start, ss_end, ss_step
  real(kind=KRC) :: rs_start, rs_end, rs_step

  real(kind=KRC) :: dtorgn

#if OPT_USE_BASE_UCALN
#else
  integer :: jy_start, jy_end
  integer :: jd_step
  integer :: orig(3)
#endif

  integer ierr
  integer KTEST
  namelist /NMTEST/ KTEST
!!!_ + Announce
#if OPT_USE_BASE_UCALN
  write (*, *) '@@@ TEST CALENDAR MIROC/BASE'
#else  /* not OPT_USE_BASE_UCALN */
  write (*, *) '@@@ TEST CALENDAR MIROC/TOUZA'
#endif /* not OPT_USE_BASE_UCALN */
!!!_ + Test switch
  rewind(ifpar, IOSTAT=ierr)
  KTEST = 0
  if (ierr.eq.0) then
     read(ifpar, NML=NMTEST, IOSTAT=ierr)
  endif
!!!_ + Initialization
#if OPT_USE_BASE_UCALN
  call CALNDR()
#else  /* not OPT_USE_BASE_UCALN */
  call CALNDR(ifpar, jfpar)
#endif /* not OPT_USE_BASE_UCALN */
  iy_start = 1
  ! iy_start = -100
  iy_end   = 3000
  iy_step  = 1

  im_start = 1
  im_end   = 12
  im_step  = 1

  im_start_ym = -13
  im_end_ym   = 25
  im_step_ym  = 1

  id_start_ym = 1
  id_end_ym   = 31
  id_step_ym  = 7

  ss_start = real(iy_start,KIND=KRC) * 366.0e0_KRC * 86400.0e0_KRC
  ss_end   = real(iy_end,  KIND=KRC) * 366.0e0_KRC * 86400.0e0_KRC
  ss_step  = 864000.0e0_KRC - 10.0e0_KRC

  rs_start = -86400.0e0_KRC
  rs_end   = 86400.0e0_KRC * 2
  rs_step  = 0.25e0_KRC

  ! id_start = (iy_start - 1) * 366 + 1
  id_start = (max (1, iy_start) - 1) * 366 + 1
  id_end   =          (iy_end   - 1) * 366 + 1
  id_step  = 11

!!!_ + CDAYMO
101 format('BEGIN TEST')
109 format('END TEST')
  write(jfpar, 101)
  if (KTEST.eq.0.or.KTEST.eq.1) then
     call test_CDAYMO (iy_start, iy_end, iy_step, im_start, im_end, im_step, jfpar)
  endif
!!!_ + C{DAY,MON}YR
  if (KTEST.eq.0.or.KTEST.eq.2) then
     call test_CxxxYR (iy_start, iy_end, iy_step, jfpar)
  endif
!!!_ + CSEC{DY,MI,HR}
  if (KTEST.eq.0.or.KTEST.eq.3) then
     call test_CSECxx (jfpar)
  endif
!!!_ + CSS2{DS,YH,YD,YM} and CYH2SS
  if (KTEST.eq.0.or.KTEST.eq.4) then
     call test_CSS2xx (ss_start, ss_end, ss_step, jfpar)
  endif
!!!_ + CDD2{YD,YM} and reverse
  if (KTEST.eq.0.or.KTEST.eq.5) then
     call test_CDD2xx (id_start, id_end, id_step, jfpar)
  endif
!!!_ + CYM2{DD,YD}
  if (KTEST.eq.0.or.KTEST.eq.6) then
     call test_CYM2xx &
       & (iy_start,    iy_end,    iy_step, &
       &  im_start_ym, im_end_ym, im_step_ym, &
       &  id_start_ym, id_end_ym, id_step_ym, jfpar)
  endif
!!!_ + CRS2HM and reverse
  if (KTEST.eq.0.or.KTEST.eq.7) then
     call test_CRS2HM (rs_start, rs_end, rs_step, jfpar)
  endif
!!!_ + CYH2CC and reverse
  if (KTEST.eq.0.or.KTEST.eq.8) then
     call test_CYH2CC (ss_start, ss_end, ss_step, jfpar)
  endif
!!!_ + CXX2SS family
  if (KTEST.eq.0.or.KTEST.eq.9) then
     dtorgn = 0.0e0_KRC
     call test_CXX2SS (1.0e0_KRC,   'SEC',  ss_start, ss_end, ss_step, dtorgn, jfpar)
     call test_CXX2SS (1.0e0_KRC,   'MIN',  ss_start, ss_end, ss_step, dtorgn, jfpar)
     call test_CXX2SS (1.0e0_KRC,   'HOUR', ss_start, ss_end, ss_step, dtorgn, jfpar)
     call test_CXX2SS (1.0e0_KRC,   'DAY',  ss_start, ss_end, ss_step, dtorgn, jfpar)

     call test_CXX2SS (1.0e0_KRC,   'MON',  ss_start, ss_end, ss_step, dtorgn, jfpar)
     call test_CXX2SS (2.0e0_KRC,   'MON',  ss_start, ss_end, ss_step, dtorgn, jfpar)
     call test_CXX2SS (3.0e0_KRC,   'MON',  ss_start, ss_end, ss_step, dtorgn, jfpar)
     call test_CXX2SS (6.0e0_KRC,   'MON',  ss_start, ss_end, ss_step, dtorgn, jfpar)
     call test_CXX2SS (12.0e0_KRC,  'MON',  ss_start, ss_end, ss_step, dtorgn, jfpar)

     call test_CXX2SS (1.0e0_KRC,   'YR',   ss_start, ss_end, ss_step, dtorgn, jfpar)
     call test_CXX2SS (5.0e0_KRC,   'YR',   ss_start, ss_end, ss_step, dtorgn, jfpar)
     call test_CXX2SS (10.0e0_KRC,  'YR',   ss_start, ss_end, ss_step, dtorgn, jfpar)
     call test_CXX2SS (20.0e0_KRC,  'YR',   ss_start, ss_end, ss_step, dtorgn, jfpar)
     call test_CXX2SS (50.0e0_KRC,  'YR',   ss_start, ss_end, ss_step, dtorgn, jfpar)
     call test_CXX2SS (100.0e0_KRC, 'YR',   ss_start, ss_end, ss_step, dtorgn, jfpar)
  endif
!!!_ + leap-cycle test
  if (KTEST.eq.0.or.KTEST.eq.10) then
#if OPT_USE_BASE_UCALN
     write (*, *) 'SKIP TEST = ', KTEST
#else  /* not OPT_USE_BASE_UCALN */
     ! orig = (/0, 3, 21/)
     orig = (/100, 3, 21/)
     jy_start = 0
     jy_end   = 801
     jd_step  = 1
     call test_PERIOD &
          & (jy_start,    jy_end,    jd_step, &
          &  orig,        jfpar)
#endif /* not OPT_USE_BASE_UCALN */
  endif

  if (KTEST.eq.0.or.KTEST.eq.11) then
#if OPT_USE_BASE_UCALN
     write (*, *) 'SKIP TEST = ', KTEST
#else  /* not OPT_USE_BASE_UCALN */
     orig = (/100, 3, 21/)
     jy_start = 0
     jy_end   = 801
     ss_step  = 86400.0
     call test_PERIOD_ss &
          & (jy_start,    jy_end,    ss_step, &
          &  orig,        jfpar)
#endif /* not OPT_USE_BASE_UCALN */
  endif

!!!_ + end
  write(jfpar, 109)
  stop
end program test_calendar_suite

!!!_ + Individual tests
!!!_  - (1) test_CDAYMO - number of days in date:Year/Month
subroutine test_CDAYMO &
     & (iy_start, iy_end, iy_step, im_start, im_end, im_step, jfpar)
#if OPT_USE_BASE_UCALN == 0
  use TOUZA_Cal_miroc
#endif /* OPT_USE_BASE_UCALN == 0 */
  implicit none
  integer,intent(in) :: iy_start, iy_end, iy_step
  integer,intent(in) :: im_start, im_end, im_step
  integer,intent(in) :: jfpar

  integer :: ndm
  integer :: iy, im

  im = im_start
111 format('CDAYMO: D << y m')
101 format('CDAYMO ', I0, 1x, I0, 1x, I0)
  write (jfpar, 111)
  do iy = iy_start, iy_end, iy_step
     do
        call CDAYMO (ndm, iy, im)
        write (jfpar, 101) ndm, iy, im
        im = im + im_step
        if (im.gt.im_end) then
           im = im - im_end
           exit
        endif
     enddo
  enddo
end subroutine test_CDAYMO

!!!_  - (2) test_CxxxYR - number of days, months in date:Year
subroutine test_CxxxYR &
     & (iy_start, iy_end, iy_step, jfpar)
#if OPT_USE_BASE_UCALN == 0
  use TOUZA_Cal_miroc
#endif /* OPT_USE_BASE_UCALN == 0 */
  implicit none
  integer,intent(in) :: iy_start, iy_end, iy_step
  integer,intent(in) :: jfpar

  integer :: ndy, nmy
  integer :: iy

111 format('CxxxYR: D M << y')
101 format('CxxxYR ', I0, 1x, I0, 1x, I0)
  write (jfpar, 111)
  do iy = iy_start, iy_end, iy_step
     call CMONYR (nmy, iy)
     call CDAYYR (ndy, iy)
     write (jfpar, 101) ndy, nmy, iy
  enddo
end subroutine test_CxxxYR

!!!_  - (3) test_CSECxx - number of seconds in a minute, an hour, a day
subroutine test_CSECxx &
     & (jfpar)
#if OPT_USE_BASE_UCALN == 0
  use TOUZA_Cal_miroc
#endif /* OPT_USE_BASE_UCALN == 0 */
  implicit none
  integer,intent(in) :: jfpar

  integer :: nsm, nsh, nsd

  call CSECMI (nsm)
  call CSECHR (nsh)
  call CSECDY (nsd)

111 format('CSECxx: S S S << 1M 1H 1D')
101 format('CSECxx ', I0, 1x, I0, 1x, I0)
  write (jfpar, 111)
  write (jfpar, 101) nsm, nsh, nsd
end subroutine test_CSECxx

!!!_  - (4) test_CSS2xx - date:Second to date:Day
subroutine test_CSS2xx &
     & (ss_start, ss_end, ss_step, jfpar)
#if OPT_USE_BASE_UCALN == 0
  use TOUZA_Cal_miroc
#endif /* OPT_USE_BASE_UCALN == 0 */
  use TOUZA_Std, only: KDBL, KFLT
  implicit none
  integer,parameter :: KRC = OPT_KIND_REAL
  real(kind=KRC),intent(in) :: ss_start, ss_end, ss_step
  integer,       intent(in) :: jfpar

  real(kind=KRC) :: ss
  real(kind=KRC) :: ss_rev_ds, ss_rev_yh
  integer :: iday
  integer :: idate (6)
  integer :: iyr, idy
  integer :: iyy, imm, idd
  logical :: check_yh_ym, check_rev_ds, check_rev_yh
  real(kind=KRC) :: rsec

  ss = ss_start
111 format('CSS2xx: S>>DS YMDHMS Dy')
101 format('CSS2xx ', F0.1, 1x, &
         & I0, 1x, F0.1, 1x, 6(I0,1x), I0, 1x, 4L1, 1x, F0.1)
  write (jfpar, 111)
  do
     if (ss.gt.ss_end) exit
     call CSS2DS (iday,      rsec, ss)
     call CDS2SS (ss_rev_ds, iday, rsec)
     check_rev_ds   = (ss.eq.ss_rev_ds)

     call CSS2YH (idate,       ss)
     call CYH2SS (ss_rev_yh,   idate)
     check_rev_yh   = (ss.eq.ss_rev_yh)

     call CSS2YM (iyy,   imm,  idd, ss)
     check_yh_ym = ((idate (1).eq.iyy) .and. (idate (2).eq.imm) .and. (idate (3).eq.idd))

     call CSS2YD (iyr,   idy,  ss)

     write (jfpar, 101) &
          & ss, iday, rsec, idate, idy, &
          & (idate (1).eq.iyr), check_yh_ym, check_rev_ds, check_rev_yh, ss_rev_yh

     ss = ss + ss_step
  enddo
end subroutine test_CSS2xx

!!!_  - (5) test_CDD2xx - date:Day to date:YMD date:YD
subroutine test_CDD2xx &
     & (id_start, id_end, id_step, jfpar)
#if OPT_USE_BASE_UCALN == 0
  use TOUZA_Cal_miroc
#endif /* OPT_USE_BASE_UCALN == 0 */
  use TOUZA_Std, only: KDBL, KFLT
  implicit none
  integer,parameter :: KRC = OPT_KIND_REAL
  integer,intent(in) :: id_start, id_end, id_step
  integer,intent(in) :: jfpar

  integer :: iday
  integer :: iyr, idy
  integer :: iyy, imm, idd
  integer :: idy_ym, iday_rev

111 format('CDD2xx: D >> YMD Dy ... Dy D')
101 format('CDD2xx ', I0, 1x, 3(I0,1x), I0, 1x, 3L1, 1x, I0, 1x, I0)
  write (jfpar, 111)
  do iday = id_start, id_end, id_step
     call CDD2YM (iyy,   imm,  idd,  iday)
     call CYM2DD (iday_rev, iyy,   imm,  idd)

     call CDD2YD (iyr,   idy,        iday)
     call CYM2YD (idy_ym,  iyy, imm, idd)

     write (jfpar, 101) &
          & iday, iyy, imm, idd, idy, &
          & (iyy.eq.iyr), (idy.eq.idy_ym), (iday.eq.iday_rev), idy_ym, iday_rev
  enddo
end subroutine test_CDD2xx

!!!_  - (6) test_CYM2xx - date:YMD to date:YD date:D
subroutine test_CYM2xx &
     & (iy_start, iy_end, iy_step, &
     &  im_start, im_end, im_step, &
     &  id_start, id_end, id_step,  jfpar)
#if OPT_USE_BASE_UCALN == 0
  use TOUZA_Cal_miroc
#endif /* OPT_USE_BASE_UCALN == 0 */
  implicit none
  integer,intent(in) :: iy_start, iy_end, iy_step
  integer,intent(in) :: im_start, im_end, im_step
  integer,intent(in) :: id_start, id_end, id_step
  integer,intent(in) :: jfpar

  integer :: iday, idy_ym
  integer :: iy, im, id

  im = im_start
111 format('CYM2xx: YMD >> D Dy')
101 format('CYM2xx ', 3(I0,1x), I0, 1x, I0)
  write (jfpar, 111)
  do iy = iy_start, iy_end, iy_step
     do
        do id = id_start, id_end, id_step
           call CYM2DD (iday,    iy,   im,  id)
           call CYM2YD (idy_ym,  iy,   im,  id)
           write (jfpar, 101) &
                & iy, im, id, iday, idy_ym
        enddo
        im = im + im_step
        if (im.gt.im_end) then
           im = (im - im_end - 1) + im_start
           exit
        endif
     enddo
  enddo
end subroutine test_CYM2xx

!!!_  - (7) test_CRS2HM
subroutine test_CRS2HM &
     & (rs_start, rs_end, rs_step, jfpar)
#if OPT_USE_BASE_UCALN == 0
  use TOUZA_Cal_miroc
#endif /* OPT_USE_BASE_UCALN == 0 */
  use TOUZA_Std, only: KDBL, KFLT
  implicit none
  integer,parameter :: KRC = OPT_KIND_REAL
  real(kind=KRC),intent(in) :: rs_start, rs_end, rs_step
  integer,       intent(in) :: jfpar

  real(kind=KRC) :: rs
  real(kind=KRC) :: rs_rev
  integer :: ihh, imm, iss

111 format('CRS2HM: S >> HMS S')
101 format('CRS2HM ', F0.2, 1x, 3(I0,1x), F0.2, 1x, L1)
  write (jfpar, 111)
  rs = rs_start
  do
     if (rs.gt.rs_end) exit

     call CRS2HM (ihh,    imm, iss,  rs)
     call CHM2RS (rs_rev, ihh, imm, iss)

     write (jfpar, 101) rs, ihh, imm, iss, rs_rev, (rs.eq.rs_rev)

     rs = rs + rs_step
  enddo
end subroutine test_CRS2HM

!!!_  - (8) test_CYH2CC
subroutine test_CYH2CC &
     & (ss_start, ss_end, ss_step, jfpar)
#if OPT_USE_BASE_UCALN == 0
  use TOUZA_Cal_miroc
#endif /* OPT_USE_BASE_UCALN == 0 */
  use TOUZA_Std, only: KDBL, KFLT
  implicit none
  integer,parameter :: KRC = OPT_KIND_REAL
  real(kind=KRC),intent(in) :: ss_start, ss_end, ss_step
  integer,       intent(in) :: jfpar

  real(kind=KRC) :: ss
  integer :: idate (6), idate_rev (6)
  character(len = 32) :: htime, htime_ss
  integer j
  logical chk_idate

111 format('CYH2CC: YMDHMS STR STR')
101 format('CYH2CC ', 6(I0,1x), A, 1x, A, 1x, L1)
  write (jfpar, 111)
  ss = ss_start
  do
     if (ss.gt.ss_end) exit
     call CSS2YH (idate,       ss)

     call CYH2CC (htime, idate)
#if WITH_TEST8_CCC2YH
     call CCC2YH (idate_rev, htime)
#endif /* WITH_TEST8_CCC2YH */
     call CSS2CC (htime_ss, ss)

     chk_idate = .true.
#if WITH_TEST8_CCC2YH
     do j = 1, 6
        if (idate (j) .ne. idate_rev (j)) chk_idate = .false.
     enddo
#endif /* WITH_TEST8_CCC2YH */
     write (jfpar, 101) &
          & (idate (j), j=1,6), trim (htime), trim (htime_ss), chk_idate
     ss = ss + ss_step
  enddo
end subroutine test_CYH2CC

!!!_  - (9) test_CXX2SS - time advancing
subroutine test_CXX2SS &
     & (dur, unit, ss_start, ss_end, ss_step, orgsec, jfpar)
#if OPT_USE_BASE_UCALN == 0
  use TOUZA_Cal_miroc
  use TOUZA_Cal,only: conv_sec_duration
#endif /* OPT_USE_BASE_UCALN == 0 */
  use TOUZA_Std, only: KDBL, KFLT
  implicit none
  integer,parameter :: KRC = OPT_KIND_REAL
  real(kind=KRC),  intent(in) :: dur, orgsec
  character(len=*),intent(in) :: unit
  real(kind=KRC),  intent(in) :: ss_start, ss_end, ss_step
  integer,         intent(in) :: jfpar

  real(kind=KRC) :: ss, ddsec, cnext, cprev
  real(kind=KRC) :: drev
  logical :: otp
#if OPT_USE_BASE_UCALN
  logical OINTVL
#endif
111 format('CXX2SS:', F0.1, 1x, A, 2x, 'S >> dS S+')
101 format('CXX2SS ', F0.2, 1x, F0.2, 1x, F0.2, 1x, F0.2, 1x, L1, 1x, F0.2, 1x, L1)
  write (jfpar, 111) dur, trim(unit)

  ss = ss_start
  cprev = ss
  ! call CSSAFT (cnext, cprev, dur, unit)
  cnext = cprev
  do
     if (ss.gt.ss_end) exit
     call CXX2SS (ddsec, dur, unit, ss)
     otp = OINTVL (ss, cprev, orgsec, dur, unit)
! #if OPT_USE_BASE_UCALN == 0
!      drev = conv_sec_duration(ddsec, unit, ss)
!      write (jfpar, 101) &
!           & ss, ddsec, cnext, cprev, otp, drev, dur.eq.drev
! #else
     write (jfpar, 101) &
          & ss, ddsec, cnext, cprev, otp
! #endif
     if (otp) then
        cprev = cnext
        call CSSAFT (cnext, cprev, dur, unit)
     endif
     ss = ss + ss_step
  enddo

end subroutine test_CXX2SS

!!!_  - (10) test_PERIOD - leap-cycle test
#if OPT_USE_BASE_UCALN
#else
subroutine test_PERIOD &
     & (iy_start, iy_end, id_step, &
     &  orig,     jfpar)
  use TOUZA_Cal_miroc
  use TOUZA_Cal
  implicit none
  integer,intent(in) :: iy_start, iy_end, id_step
  integer,intent(in) :: orig(*)
  integer,intent(in) :: jfpar

  integer :: iday,  iday_end
  integer :: jdnml, iynml
  integer :: iy, im, id

  integer :: mdper, myper
  integer :: jday_orig
  real(kind=KRC) :: apos, acyc

111 format('PERIOD: ', I0, '/', I0, '/', I0, 1x, F16.9, 1x, I0, '--', I0)
112 format('PERIOD: ', I0, '/', I0, '/', I0, 1x, F16.9, 1x, I0, '--', I0, 1x, 'SKIPPED')
101 format('PERIOD ', 3(I0,1x), I0, ' > ', I0, 1x, I0, 1x, F10.3)
  mdper = inq_nday_period()
  myper = inq_nyear_period()

  acyc = real(mdper, kind=KRC) / real(myper, kind=KRC)

  call CYM2DD(jday_orig,  orig(1), orig(2), orig(3))
  call CYM2DD(iday,       iy_start, 1, 1)
  call CYM2DD(iday_end,   iy_end, 1, 1)
  if (acyc.eq.0.0_KRC) then
     write (jfpar, 112) mdper, myper, jday_orig, acyc, iday, iday_end
  else
     write (jfpar, 111) mdper, myper, jday_orig, acyc, iday, iday_end

     do
        call CDD2YM(iy, im,  id, iday)
        if (iday.gt.iday_end) exit
        iynml = modulo(iy, myper)
        jdnml = modulo(iday - jday_orig, mdper)
        apos = (modulo(real(jdnml, kind=KRC), acyc) / acyc) * 360.0_KRC
        write(jfpar, 101) iy, im, id, iday, iynml, jdnml, apos
        iday = iday + id_step
     enddo
  endif
end subroutine test_PERIOD
#endif

!!!_  - (11) test_PERIOD_ss - leap-cycle test
#if OPT_USE_BASE_UCALN
#else
subroutine test_PERIOD_ss &
     & (iy_start, iy_end, ss_step, &
     &  orig,     jfpar)
  use TOUZA_Cal_miroc
  use TOUZA_Cal
  implicit none
  integer,intent(in) :: iy_start, iy_end
  real(kind=KRC),intent(in) :: ss_step
  integer,intent(in) :: orig(*)
  integer,intent(in) :: jfpar

  integer :: idate(6), idate_end(6)
  integer :: iday
  integer :: jdnml, iynml
  integer :: iy, im, id

  integer :: mdper, myper
  integer :: nsd
  integer :: jday_orig
  real(kind=KRC) :: ss,   sorg, ss_end
  real(kind=KRC) :: apos, acyc

  mdper = inq_nday_period()
  myper = inq_nyear_period()

  call CSECDY(nsd)
  acyc = (real(mdper, kind=KRC) / real(myper, kind=KRC)) &
       & * real(nsd, kind=KRC)

111 format('PERIODss: ', I0, '/', I0, 1x, F0.2, 1x, F0.2)
112 format('PERIODss: ', I0, '/', I0, 1x, F0.2, 1x, F0.2, 1x, 'SKIPPED')
102 format('PERIODss: ', F0.1, &
         & 1x, I0, '/', I0, '/', I0, &
         & 1x, I2.2, ':', I2.2, ':', I2.2, &
         & 1x, F0.1, 1x, F10.3)

  idate = (/iy_start, 1, 1, 0, 0, 0/)
  idate_end = (/iy_end, 1, 1, 0, 0, 0/)
  call CYH2SS(ss,idate)
  call CYH2SS(ss_end,idate_end)
  idate(1:3) = orig(1:3)
  idate(4:6) = 0
  call CYH2SS(sorg, idate)

  if (acyc.eq.0.0_KRC) then
     write (jfpar, 112) mdper, myper, acyc, sorg
  else
     write (jfpar, 111) mdper, myper, acyc, sorg
     do
        call CSS2YH(idate, ss)
        ! if (idate(1).gt.iy_end) exit
        if (ss.gt.ss_end) exit

        apos = (modulo(ss - sorg, acyc) / acyc) * 360.0_KRC
        write (jfpar, 102) &
             ss, idate, ss-sorg, apos
        ss = ss + ss_step
     enddo
  endif
  ! call CYM2DD(jday_orig,  orig(1), orig(2), orig(3))
  ! call CYM2DD(iday,       iy_start, 1, 1)

  ! write (jfpar, 111) mdper, myper, jday_orig, acyc

  ! do
  !    call CDD2YM(iy, im,  id, iday)
  !    if (iy.gt.iy_end) exit
  !    iynml = modulo(iy, myper)
  !    jdnml = modulo(iday - jday_orig, mdper)
  !    apos = (modulo(real(jdnml, kind=KRC), acyc) / acyc) * 360.0_KRC
  !    write(jfpar, 101) iy, im, id, iday, iynml, jdnml, apos
  !    iday = iday + id_step
  ! enddo
end subroutine test_PERIOD_ss
#endif

!!!_* compatible minimum procedures
#ifdef OPT_USE_BASE_UCALN
!!!_ & dgaus()
function dgaus &
     & (DX) &
     result (r)
  use TOUZA_Std, only: KDBL, KFLT
  implicit none
  integer,parameter :: KRC = OPT_KIND_REAL
  real(kind=KRC) :: r
  real(kind=KRC),intent(in) :: dx
  r = AINT (dx) + AINT (dx - AINT (dx) + 1.E0_KRC) - 1.E0_KRC
  return
end function dgaus

!!!_ & rewnml
subroutine rewnml (ifpar, jfpar)
  use TOUZA_Std, only: uin
  implicit none
  integer,intent(out) :: ifpar, jfpar
  ifpar = uin
  rewind (ifpar)
  call getjfp (jfpar)
  return
end subroutine rewnml

!!!_ & getjfp
subroutine getjfp (jfpar)
  use TOUZA_Std, only: uout
  implicit none
  integer,intent(out) :: jfpar
  jfpar = uout
end subroutine getjfp

#endif /* OPT_USE_BASE_UCALN */
