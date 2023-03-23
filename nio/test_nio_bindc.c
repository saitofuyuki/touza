/* test_nio_bindc.c - test TOUZA/Nio bind(c) interfaces */
/* Maintainer: SAITO Fuyuki */
/* Created: Feb 16 2023 */
/* Time-stamp: <2023/03/19 14:16:02 fuyuki test_nio_bindc.c> */
/* Copyright (C) 2023 */
/*           Japan Agency for Marine-Earth Science and Technology */
/* Licensed under the Apache License, Version 2.0 */
/*   (https://www.apache.org/licenses/LICENSE-2.0) */

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

#include "touza_nio_interface.h"

int main (int argc, char **argv)
{
  int ierr;
  int mode = MODE_DEEPEST;
  int levv = -1;
  int j;
  int krect, nioh;
  int jg, ngrps;
  int jv, nvars;
  char var[64];
  int jd, nd;
  int dims[3];
  char attr[16];
  char co[16];
  int nrecs;

  ierr = tnb_init(levv, mode);

  for (j=1; j < argc; j++)
    {
      krect = tnb_file_is_nio(argv[j]);
      printf("check: %d %s\n", krect, argv[j]);
      if (krect < 0) continue;
      nioh = tnb_file_open(argv[j], 0);
      printf("  open: %d\n", nioh);
      if (nioh < 0) continue;

      ierr = tnb_file_diag(nioh, 2);
      ngrps = tnb_file_groups(nioh);
      printf("  groups: %d\n", ngrps);
      for (jg = 0; jg < ngrps; jg++)
        {
          nvars = tnb_group_vars(nioh, jg);
          nrecs = tnb_group_recs(nioh, jg);
          printf("  vars[%d]: %d\n", jg, nvars);
          printf("  recs[%d]: %d\n", jg, nrecs);
          ierr = tnb_get_attr(attr, "UTIM", nioh, jg, -1, -1);
          printf("  utime[%d]: %s\n", jg, attr);
          for (jv = 0; jv < nvars; jv++)
            {
              tnb_var_name(var, nioh, jg, jv);
              printf("    var[%d]: %s\n", jv, var);
              nd = tnb_var_nco(nioh, jg, jv);
              for (jd = 0; jd < nd; jd++)
                {
                  tnb_co_name(co, nioh, jg, jv, jd);
                  dims[jd] = tnb_co_size(nioh, jg, jv, jd);
                  printf("      dim[%d]: %d %s\n", jd, dims[jd], co);
                }
            }
        }
    }

  if (ierr == 0) ierr = tnb_diag(levv, mode);
  if (ierr == 0) ierr = tnb_finalize(levv, mode);
  return ierr;
}
