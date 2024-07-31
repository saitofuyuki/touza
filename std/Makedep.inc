### automatic dependency
$(builddir)/std.lo: $(srcdir)/std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std.lo: $(std_lo_MODULE_DEPS)
if enable_module_dependencies
std_lo_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_env.$(FC_MODEXT) $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_fun.$(FC_MODEXT) $(builddir)/touza_std_arg.$(FC_MODEXT) $(builddir)/touza_std_mwe.$(FC_MODEXT) $(builddir)/touza_std_sus.$(FC_MODEXT) $(builddir)/touza_std_bld.$(FC_MODEXT) $(builddir)/touza_std_wsh.$(FC_MODEXT) $(builddir)/touza_std_htb.$(FC_MODEXT) $(builddir)/touza_std_ipc.$(FC_MODEXT)
endif

$(builddir)/std.o: $(srcdir)/std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std.o: $(std_o_MODULE_DEPS)
if enable_module_dependencies
std_o_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_env.$(FC_MODEXT) $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_fun.$(FC_MODEXT) $(builddir)/touza_std_arg.$(FC_MODEXT) $(builddir)/touza_std_mwe.$(FC_MODEXT) $(builddir)/touza_std_sus.$(FC_MODEXT) $(builddir)/touza_std_bld.$(FC_MODEXT) $(builddir)/touza_std_wsh.$(FC_MODEXT) $(builddir)/touza_std_htb.$(FC_MODEXT) $(builddir)/touza_std_ipc.$(FC_MODEXT)
endif

$(builddir)/std_arg.lo: $(srcdir)/std_arg.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_arg.lo: $(std_arg_lo_MODULE_DEPS)
if enable_module_dependencies
std_arg_lo_MODULE_DEPS = $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_env.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_fun.$(FC_MODEXT) $(builddir)/touza_std_prc.$(FC_MODEXT)
endif

$(builddir)/std_arg.o: $(srcdir)/std_arg.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_arg.o: $(std_arg_o_MODULE_DEPS)
if enable_module_dependencies
std_arg_o_MODULE_DEPS = $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_env.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_fun.$(FC_MODEXT) $(builddir)/touza_std_prc.$(FC_MODEXT)
endif

$(builddir)/std_bld.lo: $(srcdir)/std_bld.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h $(top_builddir)/autorevision.h
$(builddir)/std_bld.lo: $(std_bld_lo_MODULE_DEPS)
if enable_module_dependencies
std_bld_lo_MODULE_DEPS = $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT)
endif

$(builddir)/std_bld.o: $(srcdir)/std_bld.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h $(top_builddir)/autorevision.h
$(builddir)/std_bld.o: $(std_bld_o_MODULE_DEPS)
if enable_module_dependencies
std_bld_o_MODULE_DEPS = $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT)
endif

$(builddir)/std_env.lo: $(srcdir)/std_env.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_env.lo: $(std_env_lo_MODULE_DEPS)
if enable_module_dependencies
std_env_lo_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT) $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_fun.$(FC_MODEXT) $(builddir)/touza_std_mwe.$(FC_MODEXT)
endif

$(builddir)/std_env.o: $(srcdir)/std_env.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_env.o: $(std_env_o_MODULE_DEPS)
if enable_module_dependencies
std_env_o_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT) $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_fun.$(FC_MODEXT) $(builddir)/touza_std_mwe.$(FC_MODEXT)
endif

$(builddir)/std_fun.lo: $(srcdir)/std_fun.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_fun.lo: $(std_fun_lo_MODULE_DEPS)
if enable_module_dependencies
std_fun_lo_MODULE_DEPS = $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_mwe.$(FC_MODEXT)
endif

$(builddir)/std_fun.o: $(srcdir)/std_fun.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_fun.o: $(std_fun_o_MODULE_DEPS)
if enable_module_dependencies
std_fun_o_MODULE_DEPS = $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_mwe.$(FC_MODEXT)
endif

$(builddir)/std_htb.lo: $(srcdir)/std_htb.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_htb.lo: $(std_htb_lo_MODULE_DEPS)
if enable_module_dependencies
std_htb_lo_MODULE_DEPS = $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT)
endif

$(builddir)/std_htb.o: $(srcdir)/std_htb.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_htb.o: $(std_htb_o_MODULE_DEPS)
if enable_module_dependencies
std_htb_o_MODULE_DEPS = $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT)
endif

$(builddir)/std_ipc.lo: $(srcdir)/std_ipc.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_ipc.lo: $(std_ipc_lo_MODULE_DEPS)
if enable_module_dependencies
std_ipc_lo_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT) $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT)
endif

$(builddir)/std_ipc.o: $(srcdir)/std_ipc.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_ipc.o: $(std_ipc_o_MODULE_DEPS)
if enable_module_dependencies
std_ipc_o_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT) $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT)
endif

$(builddir)/std_log.lo: $(srcdir)/std_log.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_log.lo: $(std_log_lo_MODULE_DEPS)
if enable_module_dependencies
std_log_lo_MODULE_DEPS = $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_prc.$(FC_MODEXT)
endif

$(builddir)/std_log.o: $(srcdir)/std_log.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_log.o: $(std_log_o_MODULE_DEPS)
if enable_module_dependencies
std_log_o_MODULE_DEPS = $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_prc.$(FC_MODEXT)
endif

$(builddir)/std_mwe.lo: $(srcdir)/std_mwe.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_mwe.lo: $(std_mwe_lo_MODULE_DEPS)
if enable_module_dependencies
std_mwe_lo_MODULE_DEPS = $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT)
endif

$(builddir)/std_mwe.o: $(srcdir)/std_mwe.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_mwe.o: $(std_mwe_o_MODULE_DEPS)
if enable_module_dependencies
std_mwe_o_MODULE_DEPS = $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT)
endif

$(builddir)/std_prc.lo: $(srcdir)/std_prc.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h

$(builddir)/std_prc.o: $(srcdir)/std_prc.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h

$(builddir)/std_sus.lo: $(srcdir)/std_sus.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_sus.lo: $(std_sus_lo_MODULE_DEPS)
if enable_module_dependencies
std_sus_lo_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT) $(builddir)/touza_std_env.$(FC_MODEXT) $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_fun.$(FC_MODEXT)
endif

$(builddir)/std_sus.o: $(srcdir)/std_sus.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_sus.o: $(std_sus_o_MODULE_DEPS)
if enable_module_dependencies
std_sus_o_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT) $(builddir)/touza_std_env.$(FC_MODEXT) $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_fun.$(FC_MODEXT)
endif

$(builddir)/std_utl.lo: $(srcdir)/std_utl.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_utl.lo: $(std_utl_lo_MODULE_DEPS)
if enable_module_dependencies
std_utl_lo_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT)
endif

$(builddir)/std_utl.o: $(srcdir)/std_utl.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_utl.o: $(std_utl_o_MODULE_DEPS)
if enable_module_dependencies
std_utl_o_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT)
endif

$(builddir)/std_wsh.lo: $(srcdir)/std_wsh.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_wsh.lo: $(std_wsh_lo_MODULE_DEPS)
if enable_module_dependencies
std_wsh_lo_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT)
endif

$(builddir)/std_wsh.o: $(srcdir)/std_wsh.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/std_wsh.o: $(std_wsh_o_MODULE_DEPS)
if enable_module_dependencies
std_wsh_o_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT)
endif

$(builddir)/touza_std.$(FC_MODEXT): $(srcdir)/std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/touza_std.$(FC_MODEXT): $(touza_std_mod_MODULE_DEPS)
if enable_module_dependencies
touza_std_mod_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_env.$(FC_MODEXT) $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_fun.$(FC_MODEXT) $(builddir)/touza_std_arg.$(FC_MODEXT) $(builddir)/touza_std_mwe.$(FC_MODEXT) $(builddir)/touza_std_sus.$(FC_MODEXT) $(builddir)/touza_std_bld.$(FC_MODEXT) $(builddir)/touza_std_wsh.$(FC_MODEXT) $(builddir)/touza_std_htb.$(FC_MODEXT) $(builddir)/touza_std_ipc.$(FC_MODEXT)
endif

$(builddir)/touza_std_arg.$(FC_MODEXT): $(srcdir)/std_arg.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/touza_std_arg.$(FC_MODEXT): $(touza_std_arg_mod_MODULE_DEPS)
if enable_module_dependencies
touza_std_arg_mod_MODULE_DEPS = $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_env.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_fun.$(FC_MODEXT) $(builddir)/touza_std_prc.$(FC_MODEXT)
endif

$(builddir)/touza_std_bld.$(FC_MODEXT): $(srcdir)/std_bld.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h $(top_builddir)/autorevision.h
$(builddir)/touza_std_bld.$(FC_MODEXT): $(touza_std_bld_mod_MODULE_DEPS)
if enable_module_dependencies
touza_std_bld_mod_MODULE_DEPS = $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT)
endif

$(builddir)/touza_std_env.$(FC_MODEXT): $(srcdir)/std_env.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/touza_std_env.$(FC_MODEXT): $(touza_std_env_mod_MODULE_DEPS)
if enable_module_dependencies
touza_std_env_mod_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT) $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_fun.$(FC_MODEXT) $(builddir)/touza_std_mwe.$(FC_MODEXT)
endif

$(builddir)/touza_std_fun.$(FC_MODEXT): $(srcdir)/std_fun.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/touza_std_fun.$(FC_MODEXT): $(touza_std_fun_mod_MODULE_DEPS)
if enable_module_dependencies
touza_std_fun_mod_MODULE_DEPS = $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_mwe.$(FC_MODEXT)
endif

$(builddir)/touza_std_htb.$(FC_MODEXT): $(srcdir)/std_htb.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/touza_std_htb.$(FC_MODEXT): $(touza_std_htb_mod_MODULE_DEPS)
if enable_module_dependencies
touza_std_htb_mod_MODULE_DEPS = $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT)
endif

$(builddir)/touza_std_ipc.$(FC_MODEXT): $(srcdir)/std_ipc.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/touza_std_ipc.$(FC_MODEXT): $(touza_std_ipc_mod_MODULE_DEPS)
if enable_module_dependencies
touza_std_ipc_mod_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT) $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT)
endif

$(builddir)/touza_std_log.$(FC_MODEXT): $(srcdir)/std_log.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/touza_std_log.$(FC_MODEXT): $(touza_std_log_mod_MODULE_DEPS)
if enable_module_dependencies
touza_std_log_mod_MODULE_DEPS = $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_prc.$(FC_MODEXT)
endif

$(builddir)/touza_std_mwe.$(FC_MODEXT): $(srcdir)/std_mwe.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/touza_std_mwe.$(FC_MODEXT): $(touza_std_mwe_mod_MODULE_DEPS)
if enable_module_dependencies
touza_std_mwe_mod_MODULE_DEPS = $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT)
endif

$(builddir)/touza_std_prc.$(FC_MODEXT): $(srcdir)/std_prc.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h

$(builddir)/touza_std_sus.$(FC_MODEXT): $(srcdir)/std_sus.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/touza_std_sus.$(FC_MODEXT): $(touza_std_sus_mod_MODULE_DEPS)
if enable_module_dependencies
touza_std_sus_mod_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT) $(builddir)/touza_std_env.$(FC_MODEXT) $(builddir)/touza_std_log.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT) $(builddir)/touza_std_fun.$(FC_MODEXT)
endif

$(builddir)/touza_std_utl.$(FC_MODEXT): $(srcdir)/std_utl.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/touza_std_utl.$(FC_MODEXT): $(touza_std_utl_mod_MODULE_DEPS)
if enable_module_dependencies
touza_std_utl_mod_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT)
endif

$(builddir)/touza_std_wsh.$(FC_MODEXT): $(srcdir)/std_wsh.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_std.h $(top_srcdir)/touza.h
$(builddir)/touza_std_wsh.$(FC_MODEXT): $(touza_std_wsh_mod_MODULE_DEPS)
if enable_module_dependencies
touza_std_wsh_mod_MODULE_DEPS = $(builddir)/touza_std_prc.$(FC_MODEXT) $(builddir)/touza_std_utl.$(FC_MODEXT)
endif

$(srcdir)/touza_std.h: 

$(top_builddir)/autorevision.h: 

$(top_builddir)/touza_config.h: 

$(top_srcdir)/touza.h: 

