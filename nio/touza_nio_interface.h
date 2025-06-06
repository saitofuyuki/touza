/* touza_nio_interface.h - TOUZA/Nio c interfaces */
/* Maintainer: SAITO Fuyuki */
/* Created: Feb 16 2023 */
/* Time-stamp: <2025/02/27 10:28:39 fuyuki touza_nio_interface.h> */
/* Copyright (C) 2023, 2024, 2025 */
/*           Japan Agency for Marine-Earth Science and Technology */
/* Licensed under the Apache License, Version 2.0 */
/*   (https://www.apache.org/licenses/LICENSE-2.0) */

#ifndef    _TOUZA_NIO_INTERFACE_H
#  define  _TOUZA_NIO_INTERFACE_H 1

#  include "touza.h"

#  include "touza_nio_param.h"

#  define TNB_HEADER_ITEMS NIO_HEADER_ITEMS   /* for backword compatibility */

extern int tnb_init(const int levv, const int mode);
extern int tnb_diag(const int levv, const int mode);
extern int tnb_finalize(const int levv, const int mode);

extern int tnb_file_is_nio(const char *path);
extern int tnb_file_open(const char *path, const int flag);
extern int tnb_file_diag(const int handle, const int lev);
extern int tnb_file_close(const int handle);
extern int tnb_file_is_opened(const char *path);
extern int tnb_file_groups(const int handle);

extern int tnb_group_coors(const int handle);
extern int tnb_group_co_idx(const int handle, const int cid);
extern int tnb_group_co_name(char * const name, const int handle, const int cid);
extern int tnb_group_co_range(int *jbgn, int *jend, const int handle, const int cid);
extern int tnb_group_vars(const int handle);
extern int tnb_group_recs(const int handle);
extern int tnb_group(const int handle, const int gidx);
extern int tnb_search_group(const int handle, const char *name, const int refh);
extern int tnb_group_name(const char *name, const int handle);

extern int tnb_var_name(const char *name,
                        const int handle, const int vid);
extern int tnb_search_var(const int handle, const char *name);
extern int tnb_var_recs(const int handle, const int vid);

extern int tnb_co_size(const int handle, const int vid);
extern int tnb_co_serial(const int handle, const int vid, const int cid);
extern int tnb_co_len(const int handle, const int vid, const int cid);
extern int tnb_co_idx(const int handle, const int vid, const char *name);
extern int tnb_co_name(char * const name, const int handle, const int vid,
                       const int cid);

extern int tnb_rec_time(char * const time,
                        const int handle, const int vid,
                        const int rec);
extern int tnb_rec_date(char * const date,
                        const int handle, const int vid,
                        const int rec);

extern int tnb_attr_size(const int handle, const int vid,
                         const int rec);
extern int tnb_attr_len(const char *item,
                        const int handle, const int vid,
                        const int rec);
extern int tnb_get_header(char * const head,
                          const int handle, const int vid,
                          const int rec);
extern int tnb_get_attr(char * const attr, const char *item,
                        const int handle, const int vid,
                        const int rec);
extern int tnb_get_attr_float(float * const attr, const char *item,
                              const int handle, const int vid,
                              const int rec);
extern int tnb_get_attr_byid(char * const attr, const int item,
                             const int handle, const int vid,
                             const int rec);
extern int tnb_get_attr_name(char * const attr, const int item);

extern int tnb_var_read_int(int * const d,
                            const size_t rec, const size_t *start, const size_t *count,
                            const int handle, const int vid);
extern int tnb_var_read_float(float * const d,
                              const size_t rec, const size_t *start, const size_t *count,
                              const int handle, const int vid);
extern int tnb_var_read_double(double * const d,
                               const size_t rec, const size_t *start, const size_t *count,
                               const int handle, const int vid);

extern int tnb_var_lread_int(int * const d,
                             const size_t rec, const size_t *start, const size_t *count,
                             const int handle, const int vid);
extern int tnb_var_lread_float(float * const d,
                               const size_t rec, const size_t *start, const size_t *count,
                               const int handle, const int vid);
extern int tnb_var_lread_double(double * const d,
                                const size_t rec, const size_t *start, const size_t *count,
                                const int handle, const int vid);

#endif /* not _TOUZA_NIO_INTERFACE_H */
