/* touza_ami.h - touza/ami common definition */
/* Maintainer: SAITO Fuyuki */
/* Created: May 2 2022 */
/* Time-stamp: <2022/05/02 09:44:40 fuyuki touza_ami.h> */
/* Copyright (C) 2022 */
/*           Japan Agency for Marine-Earth Science and Technology */
/* Licensed under the Apache License, Version 2.0 */
/*   (https://www.apache.org/licenses/LICENSE-2.0) */

#include "touza.h"
#ifndef    _TOUZA_AMI_H
#  define  _TOUZA_AMI_H 1

#  ifdef TOUZA_TRANSFORM
#    define TOUZA_Ami       TOUZA_TRANSFORM(Ami)
#  endif
#  ifdef TOUZA_TRANSFORM_MDL
#    define TOUZA_Ami_std   TOUZA_TRANSFORM_MDL(Ami,std)
#    define TOUZA_Ami_table TOUZA_TRANSFORM_MDL(Ami,table)
#  endif

#  ifndef GROUP_TAG
#  define GROUP_TAG 'Ami'
#  endif
#  ifndef __GRP__
#  define __GRP__ GROUP_TAG
#  endif

#ifndef    AMI_MSG_LEVEL
#  define  AMI_MSG_LEVEL 0
#endif

#endif /* not _TOUZA_AMI_H */
