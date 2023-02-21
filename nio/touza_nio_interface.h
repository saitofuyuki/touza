/* touza_nio_interface.h - TOUZA/Nio c interfaces */
/* Maintainer: SAITO Fuyuki */
/* Created: Feb 16 2023 */
/* Time-stamp: <2023/02/22 07:18:09 fuyuki touza_nio_interface.h> */
/* Copyright (C) 2023 */
/*           Japan Agency for Marine-Earth Science and Technology */
/* Licensed under the Apache License, Version 2.0 */
/*   (https://www.apache.org/licenses/LICENSE-2.0) */

#ifndef    _TOUZA_NIO_INTERFACE_H
#  define  _TOUZA_NIO_INTERFACE_H 1

#  include "touza.h"

extern int tni_init(const int levv, const int mode);
extern int tni_diag(const int levv, const int mode);
extern int tni_finalize(const int levv, const int mode);

extern int tni_file_is_nio(const char *path);
extern int tni_file_open(const char *path, const int flag);
extern int tni_file_diag(const int handle, const int lev);
extern int tni_file_groups(const int handle);
extern int tni_group_vars(const int handle, const int gid);
extern int tni_group_recs(const int handle, const int gid);
extern int tni_var_name(const char *name,
                        const int handle, const int gid, const int vid);
/* extern int tni_var_dims(int *dims, */
/*                         const int handle, const int gid, const int vid); */
extern int tni_var_nco(const int handle, const int gid, const int vid);
extern int tni_var_id(const int handle, const int gid, const char *name);
extern int tni_get_attr(char * const attr, const char *item,
                        const int handle, const int gid, const int vid);
extern int tni_co_size(const int handle, const int gid, const int vid,
                       const int cid);
extern int tni_co_idx(const int handle, const int gid, const int vid,
                      const char *name);
extern int tni_co_name(char * const name,
                       const int handle, const int gid, const int vid,
                       const int cid);

extern int tni_var_read_float(float * const d,
                              const size_t rec, const size_t *start, const size_t *count,
                              const int handle, const int gid, const int vid);

#endif /* not _TOUZA_NIO_INTERFACE_H */
