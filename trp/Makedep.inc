### automatic dependency
$(builddir)/test_trapiche-trapiche.o: $(srcdir)/trapiche.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/test_trapiche-trapiche.o: $(test_trapiche_trapiche_o_MODULE_DEPS)
if enable_module_dependencies
test_trapiche_trapiche_o_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_pack.$(FC_MODEXT) $(builddir)/touza_trp_float.$(FC_MODEXT) $(builddir)/touza_trp_ctl.$(FC_MODEXT) $(builddir)/touza_trp.$(FC_MODEXT)
endif

$(builddir)/test_trapiche_ctl-trapiche_ctl.o: $(srcdir)/trapiche_ctl.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/test_trapiche_ctl-trapiche_ctl.o: $(test_trapiche_ctl_trapiche_ctl_o_MODULE_DEPS)
if enable_module_dependencies
test_trapiche_ctl_trapiche_ctl_o_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_ctl.$(FC_MODEXT)
endif

$(builddir)/test_trapiche_float-trapiche_float.o: $(srcdir)/trapiche_float.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/test_trapiche_float-trapiche_float.o: $(test_trapiche_float_trapiche_float_o_MODULE_DEPS)
if enable_module_dependencies
test_trapiche_float_trapiche_float_o_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_pack.$(FC_MODEXT) $(top_builddir)/std/touza_std_arg.$(FC_MODEXT) $(top_builddir)/std/touza_std_prc.$(FC_MODEXT) $(builddir)/touza_trp_float.$(FC_MODEXT)
endif

$(builddir)/test_trapiche_pack_32-trapiche_pack.o: $(srcdir)/trapiche_pack.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/test_trapiche_pack_32-trapiche_pack.o: $(test_trapiche_pack_32_trapiche_pack_o_MODULE_DEPS)
if enable_module_dependencies
test_trapiche_pack_32_trapiche_pack_o_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_pack.$(FC_MODEXT)
endif

$(builddir)/test_trapiche_pack_64-trapiche_pack.o: $(srcdir)/trapiche_pack.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/test_trapiche_pack_64-trapiche_pack.o: $(test_trapiche_pack_64_trapiche_pack_o_MODULE_DEPS)
if enable_module_dependencies
test_trapiche_pack_64_trapiche_pack_o_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_pack.$(FC_MODEXT)
endif

$(builddir)/test_trapiche_pack_uip-trapiche_pack.o: $(srcdir)/trapiche_pack.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/test_trapiche_pack_uip-trapiche_pack.o: $(test_trapiche_pack_uip_trapiche_pack_o_MODULE_DEPS)
if enable_module_dependencies
test_trapiche_pack_uip_trapiche_pack_o_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_pack.$(FC_MODEXT)
endif

$(builddir)/test_trapiche_std-trapiche_std.o: $(srcdir)/trapiche_std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/test_trapiche_std-trapiche_std.o: $(test_trapiche_std_trapiche_std_o_MODULE_DEPS)
if enable_module_dependencies
test_trapiche_std_trapiche_std_o_MODULE_DEPS = $(top_builddir)/std/touza_std_prc.$(FC_MODEXT) $(top_builddir)/std/touza_std_utl.$(FC_MODEXT) $(top_builddir)/std/touza_std_log.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_trp_std.$(FC_MODEXT)
endif

$(builddir)/test_trapiche_transfer-trapiche_transfer.o: $(srcdir)/trapiche_transfer.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/test_trapiche_transfer-trapiche_transfer.o: $(test_trapiche_transfer_trapiche_transfer_o_MODULE_DEPS)
if enable_module_dependencies
test_trapiche_transfer_trapiche_transfer_o_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_float.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_trp_pack.$(FC_MODEXT) $(builddir)/touza_trp_transfer.$(FC_MODEXT) $(builddir)/touza_trp.$(FC_MODEXT)
endif

$(builddir)/touza_trp.$(FC_MODEXT): $(srcdir)/trapiche.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/touza_trp.$(FC_MODEXT): $(touza_trp_mod_MODULE_DEPS)
if enable_module_dependencies
touza_trp_mod_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_pack.$(FC_MODEXT) $(builddir)/touza_trp_float.$(FC_MODEXT) $(builddir)/touza_trp_ctl.$(FC_MODEXT)
endif

$(builddir)/touza_trp_ctl.$(FC_MODEXT): $(srcdir)/trapiche_ctl.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/touza_trp_ctl.$(FC_MODEXT): $(touza_trp_ctl_mod_MODULE_DEPS)
if enable_module_dependencies
touza_trp_ctl_mod_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT)
endif

$(builddir)/touza_trp_float.$(FC_MODEXT): $(srcdir)/trapiche_float.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/touza_trp_float.$(FC_MODEXT): $(touza_trp_float_mod_MODULE_DEPS)
if enable_module_dependencies
touza_trp_float_mod_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_pack.$(FC_MODEXT) $(top_builddir)/std/touza_std_arg.$(FC_MODEXT) $(top_builddir)/std/touza_std_prc.$(FC_MODEXT)
endif

$(builddir)/touza_trp_pack.$(FC_MODEXT): $(srcdir)/trapiche_pack.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/touza_trp_pack.$(FC_MODEXT): $(touza_trp_pack_mod_MODULE_DEPS)
if enable_module_dependencies
touza_trp_pack_mod_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT)
endif

$(builddir)/touza_trp_std.$(FC_MODEXT): $(srcdir)/trapiche_std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/touza_trp_std.$(FC_MODEXT): $(touza_trp_std_mod_MODULE_DEPS)
if enable_module_dependencies
touza_trp_std_mod_MODULE_DEPS = $(top_builddir)/std/touza_std_prc.$(FC_MODEXT) $(top_builddir)/std/touza_std_utl.$(FC_MODEXT) $(top_builddir)/std/touza_std_log.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/touza_trp_transfer.$(FC_MODEXT): $(srcdir)/trapiche_transfer.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/touza_trp_transfer.$(FC_MODEXT): $(touza_trp_transfer_mod_MODULE_DEPS)
if enable_module_dependencies
touza_trp_transfer_mod_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_float.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_trp_pack.$(FC_MODEXT) $(builddir)/touza_trp.$(FC_MODEXT)
endif

$(builddir)/trapiche.lo: $(srcdir)/trapiche.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/trapiche.lo: $(trapiche_lo_MODULE_DEPS)
if enable_module_dependencies
trapiche_lo_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_pack.$(FC_MODEXT) $(builddir)/touza_trp_float.$(FC_MODEXT) $(builddir)/touza_trp_ctl.$(FC_MODEXT)
endif

$(builddir)/trapiche.o: $(srcdir)/trapiche.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/trapiche.o: $(trapiche_o_MODULE_DEPS)
if enable_module_dependencies
trapiche_o_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_pack.$(FC_MODEXT) $(builddir)/touza_trp_float.$(FC_MODEXT) $(builddir)/touza_trp_ctl.$(FC_MODEXT) $(builddir)/touza_trp.$(FC_MODEXT)
endif

$(builddir)/trapiche_ctl.lo: $(srcdir)/trapiche_ctl.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/trapiche_ctl.lo: $(trapiche_ctl_lo_MODULE_DEPS)
if enable_module_dependencies
trapiche_ctl_lo_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT)
endif

$(builddir)/trapiche_ctl.o: $(srcdir)/trapiche_ctl.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/trapiche_ctl.o: $(trapiche_ctl_o_MODULE_DEPS)
if enable_module_dependencies
trapiche_ctl_o_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_ctl.$(FC_MODEXT)
endif

$(builddir)/trapiche_float.lo: $(srcdir)/trapiche_float.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/trapiche_float.lo: $(trapiche_float_lo_MODULE_DEPS)
if enable_module_dependencies
trapiche_float_lo_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_pack.$(FC_MODEXT)
endif

$(builddir)/trapiche_float.o: $(srcdir)/trapiche_float.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/trapiche_float.o: $(trapiche_float_o_MODULE_DEPS)
if enable_module_dependencies
trapiche_float_o_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_pack.$(FC_MODEXT) $(top_builddir)/std/touza_std_arg.$(FC_MODEXT) $(top_builddir)/std/touza_std_prc.$(FC_MODEXT) $(builddir)/touza_trp_float.$(FC_MODEXT)
endif

$(builddir)/trapiche_pack.lo: $(srcdir)/trapiche_pack.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/trapiche_pack.lo: $(trapiche_pack_lo_MODULE_DEPS)
if enable_module_dependencies
trapiche_pack_lo_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT)
endif

$(builddir)/trapiche_pack.o: $(srcdir)/trapiche_pack.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/trapiche_pack.o: $(trapiche_pack_o_MODULE_DEPS)
if enable_module_dependencies
trapiche_pack_o_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_pack.$(FC_MODEXT)
endif

$(builddir)/trapiche_std.lo: $(srcdir)/trapiche_std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/trapiche_std.lo: $(trapiche_std_lo_MODULE_DEPS)
if enable_module_dependencies
trapiche_std_lo_MODULE_DEPS = $(top_builddir)/std/touza_std_prc.$(FC_MODEXT) $(top_builddir)/std/touza_std_utl.$(FC_MODEXT) $(top_builddir)/std/touza_std_log.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/trapiche_std.o: $(srcdir)/trapiche_std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/trapiche_std.o: $(trapiche_std_o_MODULE_DEPS)
if enable_module_dependencies
trapiche_std_o_MODULE_DEPS = $(top_builddir)/std/touza_std_prc.$(FC_MODEXT) $(top_builddir)/std/touza_std_utl.$(FC_MODEXT) $(top_builddir)/std/touza_std_log.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_trp_std.$(FC_MODEXT)
endif

$(builddir)/trapiche_transfer.lo: $(srcdir)/trapiche_transfer.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/trapiche_transfer.lo: $(trapiche_transfer_lo_MODULE_DEPS)
if enable_module_dependencies
trapiche_transfer_lo_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_float.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/trapiche_transfer.o: $(srcdir)/trapiche_transfer.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_trp.h $(top_srcdir)/touza.h
$(builddir)/trapiche_transfer.o: $(trapiche_transfer_o_MODULE_DEPS)
if enable_module_dependencies
trapiche_transfer_o_MODULE_DEPS = $(builddir)/touza_trp_std.$(FC_MODEXT) $(builddir)/touza_trp_float.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_trp_pack.$(FC_MODEXT) $(builddir)/touza_trp_transfer.$(FC_MODEXT) $(builddir)/touza_trp.$(FC_MODEXT)
endif

$(srcdir)/touza_trp.h: 

$(top_builddir)/std/touza_std.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_arg.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_log.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_prc.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_utl.$(FC_MODEXT): 

$(top_builddir)/touza_config.h: 

$(top_srcdir)/touza.h: 

