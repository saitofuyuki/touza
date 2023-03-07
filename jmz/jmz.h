/* jmz.h - jmz definitions */
/* Maintainer: SAITO Fuyuki */
/* Created: Nov 25 2021 */
/* Time-stamp: <2022/11/08 08:22:03 fuyuki jmz.h> */

/* Copyright (C) 2021 */
/*           Japan Agency for Marine-Earth Science and Technology */
/* Licensed under the Apache License, Version 2.0 */
/*   (https://www.apache.org/licenses/LICENSE-2.0) */

#ifndef    _JMZ_H
#  define  _JMZ_H

#include "touza.h"

#define ERR_EXHAUST 1   /* special recurn code at cueing */

#ifndef    OPT_CHAK_PRECISION
#  define  OPT_CHAK_PRECISION  0
#endif
#if OPT_CHAK_PRECISION == 1
#  define __KBUF KFLT
#else
#  define __KBUF KDBL
#endif

#endif /* not _JMZ_H */
