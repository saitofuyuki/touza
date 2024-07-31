### automatic dependency
$(builddir)/ami.lo: $(srcdir)/ami.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami.lo: $(ami_lo_MODULE_DEPS)
if enable_module_dependencies
ami_lo_MODULE_DEPS = $(builddir)/touza_ami_legacy.$(FC_MODEXT) $(builddir)/touza_ami_nio.$(FC_MODEXT) $(builddir)/touza_ami_table.$(FC_MODEXT) $(builddir)/touza_ami_xform.$(FC_MODEXT) $(builddir)/touza_ami_std.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/ami.o: $(srcdir)/ami.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami.o: $(ami_o_MODULE_DEPS)
if enable_module_dependencies
ami_o_MODULE_DEPS = $(builddir)/touza_ami_legacy.$(FC_MODEXT) $(builddir)/touza_ami_nio.$(FC_MODEXT) $(builddir)/touza_ami_table.$(FC_MODEXT) $(builddir)/touza_ami_xform.$(FC_MODEXT) $(builddir)/touza_ami_std.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/ami_legacy.lo: $(srcdir)/ami_legacy.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami_legacy.lo: $(ami_legacy_lo_MODULE_DEPS)
if enable_module_dependencies
ami_legacy_lo_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT)
endif

$(builddir)/ami_legacy.o: $(srcdir)/ami_legacy.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami_legacy.o: $(ami_legacy_o_MODULE_DEPS)
if enable_module_dependencies
ami_legacy_o_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(builddir)/touza_ami_legacy.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/ami_miroc.o: $(srcdir)/ami_miroc.F90 $(top_builddir)/touza_config.h
$(builddir)/ami_miroc.o: $(ami_miroc_o_MODULE_DEPS)
if enable_module_dependencies
ami_miroc_o_MODULE_DEPS = $(builddir)/touza_ami_legacy.$(FC_MODEXT) $(builddir)/touza_ami_std.$(FC_MODEXT) $(builddir)/touza_ami_miroc.$(FC_MODEXT)
endif

$(builddir)/ami_nio.lo: $(srcdir)/ami_nio.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami_nio.lo: $(ami_nio_lo_MODULE_DEPS)
if enable_module_dependencies
ami_nio_lo_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(top_builddir)/nio/touza_nio.$(FC_MODEXT)
endif

$(builddir)/ami_nio.o: $(srcdir)/ami_nio.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami_nio.o: $(ami_nio_o_MODULE_DEPS)
if enable_module_dependencies
ami_nio_o_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(top_builddir)/nio/touza_nio.$(FC_MODEXT) $(builddir)/touza_ami_legacy.$(FC_MODEXT) $(builddir)/touza_ami_nio.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/ami_std.lo: $(srcdir)/ami_std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami_std.lo: $(ami_std_lo_MODULE_DEPS)
if enable_module_dependencies
ami_std_lo_MODULE_DEPS = $(top_builddir)/std/touza_std_prc.$(FC_MODEXT) $(top_builddir)/std/touza_std_utl.$(FC_MODEXT) $(top_builddir)/std/touza_std_log.$(FC_MODEXT) $(top_builddir)/std/touza_std_env.$(FC_MODEXT) $(top_builddir)/std/touza_std_fun.$(FC_MODEXT) $(top_builddir)/std/touza_std_sus.$(FC_MODEXT) $(top_builddir)/std/touza_std_mwe.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/ami_std.o: $(srcdir)/ami_std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami_std.o: $(ami_std_o_MODULE_DEPS)
if enable_module_dependencies
ami_std_o_MODULE_DEPS = $(top_builddir)/std/touza_std_prc.$(FC_MODEXT) $(top_builddir)/std/touza_std_utl.$(FC_MODEXT) $(top_builddir)/std/touza_std_log.$(FC_MODEXT) $(top_builddir)/std/touza_std_env.$(FC_MODEXT) $(top_builddir)/std/touza_std_fun.$(FC_MODEXT) $(top_builddir)/std/touza_std_sus.$(FC_MODEXT) $(top_builddir)/std/touza_std_mwe.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/ami_table.lo: $(srcdir)/ami_table.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami_table.lo: $(ami_table_lo_MODULE_DEPS)
if enable_module_dependencies
ami_table_lo_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(top_builddir)/emu/touza_emu.$(FC_MODEXT) $(top_builddir)/emu/touza_emu_ugg.$(FC_MODEXT)
endif

$(builddir)/ami_table.o: $(srcdir)/ami_table.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami_table.o: $(ami_table_o_MODULE_DEPS)
if enable_module_dependencies
ami_table_o_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(top_builddir)/emu/touza_emu.$(FC_MODEXT) $(top_builddir)/emu/touza_emu_ugg.$(FC_MODEXT) $(builddir)/touza_ami_table.$(FC_MODEXT) $(top_builddir)/nio/touza_nio.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/ami_utils.lo: $(srcdir)/ami_utils.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami_utils.lo: $(ami_utils_lo_MODULE_DEPS)
if enable_module_dependencies
ami_utils_lo_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT)
endif

$(builddir)/ami_utils.o: $(srcdir)/ami_utils.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami_utils.o: $(ami_utils_o_MODULE_DEPS)
if enable_module_dependencies
ami_utils_o_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(builddir)/touza_ami_utils.$(FC_MODEXT)
endif

$(builddir)/ami_xform.lo: $(srcdir)/ami_xform.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami_xform.lo: $(ami_xform_lo_MODULE_DEPS)
if enable_module_dependencies
ami_xform_lo_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT)
endif

$(builddir)/ami_xform.o: $(srcdir)/ami_xform.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami_xform.o: $(ami_xform_o_MODULE_DEPS)
if enable_module_dependencies
ami_xform_o_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(builddir)/touza_ami_xform.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/test_ami_legacy-ami_legacy.o: $(srcdir)/ami_legacy.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/test_ami_legacy-ami_legacy.o: $(test_ami_legacy_ami_legacy_o_MODULE_DEPS)
if enable_module_dependencies
test_ami_legacy_ami_legacy_o_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(builddir)/touza_ami_legacy.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/test_ami_miroc-ami_miroc.o: $(srcdir)/ami_miroc.F90 $(top_builddir)/touza_config.h
$(builddir)/test_ami_miroc-ami_miroc.o: $(test_ami_miroc_ami_miroc_o_MODULE_DEPS)
if enable_module_dependencies
test_ami_miroc_ami_miroc_o_MODULE_DEPS = $(builddir)/touza_ami_legacy.$(FC_MODEXT) $(builddir)/touza_ami_std.$(FC_MODEXT) $(builddir)/touza_ami_miroc.$(FC_MODEXT)
endif

$(builddir)/test_ami_nio-ami_nio.o: $(srcdir)/ami_nio.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/test_ami_nio-ami_nio.o: $(test_ami_nio_ami_nio_o_MODULE_DEPS)
if enable_module_dependencies
test_ami_nio_ami_nio_o_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(top_builddir)/nio/touza_nio.$(FC_MODEXT) $(builddir)/touza_ami_legacy.$(FC_MODEXT) $(builddir)/touza_ami_nio.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/test_ami_table-ami_table.o: $(srcdir)/ami_table.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/test_ami_table-ami_table.o: $(test_ami_table_ami_table_o_MODULE_DEPS)
if enable_module_dependencies
test_ami_table_ami_table_o_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(top_builddir)/emu/touza_emu.$(FC_MODEXT) $(top_builddir)/emu/touza_emu_ugg.$(FC_MODEXT) $(builddir)/touza_ami_table.$(FC_MODEXT) $(top_builddir)/nio/touza_nio.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/test_ami_utils-ami_utils.o: $(srcdir)/ami_utils.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/test_ami_utils-ami_utils.o: $(test_ami_utils_ami_utils_o_MODULE_DEPS)
if enable_module_dependencies
test_ami_utils_ami_utils_o_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(builddir)/touza_ami_utils.$(FC_MODEXT)
endif

$(builddir)/test_ami_xform-ami_xform.o: $(srcdir)/ami_xform.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/test_ami_xform-ami_xform.o: $(test_ami_xform_ami_xform_o_MODULE_DEPS)
if enable_module_dependencies
test_ami_xform_ami_xform_o_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(builddir)/touza_ami_xform.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/touza_ami.$(FC_MODEXT): $(srcdir)/ami.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/touza_ami.$(FC_MODEXT): $(touza_ami_mod_MODULE_DEPS)
if enable_module_dependencies
touza_ami_mod_MODULE_DEPS = $(builddir)/touza_ami_legacy.$(FC_MODEXT) $(builddir)/touza_ami_nio.$(FC_MODEXT) $(builddir)/touza_ami_table.$(FC_MODEXT) $(builddir)/touza_ami_xform.$(FC_MODEXT) $(builddir)/touza_ami_std.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/touza_ami_legacy.$(FC_MODEXT): $(srcdir)/ami_legacy.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/touza_ami_legacy.$(FC_MODEXT): $(touza_ami_legacy_mod_MODULE_DEPS)
if enable_module_dependencies
touza_ami_legacy_mod_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/touza_ami_miroc.$(FC_MODEXT): $(srcdir)/ami_miroc.F90 $(top_builddir)/touza_config.h
$(builddir)/touza_ami_miroc.$(FC_MODEXT): $(touza_ami_miroc_mod_MODULE_DEPS)
if enable_module_dependencies
touza_ami_miroc_mod_MODULE_DEPS = $(builddir)/touza_ami_legacy.$(FC_MODEXT) $(builddir)/touza_ami_std.$(FC_MODEXT)
endif

$(builddir)/touza_ami_nio.$(FC_MODEXT): $(srcdir)/ami_nio.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/touza_ami_nio.$(FC_MODEXT): $(touza_ami_nio_mod_MODULE_DEPS)
if enable_module_dependencies
touza_ami_nio_mod_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(top_builddir)/nio/touza_nio.$(FC_MODEXT) $(builddir)/touza_ami_legacy.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/touza_ami_std.$(FC_MODEXT): $(srcdir)/ami_std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/touza_ami_std.$(FC_MODEXT): $(touza_ami_std_mod_MODULE_DEPS)
if enable_module_dependencies
touza_ami_std_mod_MODULE_DEPS = $(top_builddir)/std/touza_std_prc.$(FC_MODEXT) $(top_builddir)/std/touza_std_utl.$(FC_MODEXT) $(top_builddir)/std/touza_std_log.$(FC_MODEXT) $(top_builddir)/std/touza_std_env.$(FC_MODEXT) $(top_builddir)/std/touza_std_fun.$(FC_MODEXT) $(top_builddir)/std/touza_std_sus.$(FC_MODEXT) $(top_builddir)/std/touza_std_mwe.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/touza_ami_table.$(FC_MODEXT): $(srcdir)/ami_table.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/touza_ami_table.$(FC_MODEXT): $(touza_ami_table_mod_MODULE_DEPS)
if enable_module_dependencies
touza_ami_table_mod_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(top_builddir)/emu/touza_emu.$(FC_MODEXT) $(top_builddir)/emu/touza_emu_ugg.$(FC_MODEXT) $(top_builddir)/nio/touza_nio.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/touza_ami_utils.$(FC_MODEXT): $(srcdir)/ami_utils.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/touza_ami_utils.$(FC_MODEXT): $(touza_ami_utils_mod_MODULE_DEPS)
if enable_module_dependencies
touza_ami_utils_mod_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT)
endif

$(builddir)/touza_ami_xform.$(FC_MODEXT): $(srcdir)/ami_xform.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/touza_ami_xform.$(FC_MODEXT): $(touza_ami_xform_mod_MODULE_DEPS)
if enable_module_dependencies
touza_ami_xform_mod_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(srcdir)/touza_ami.h: 

$(top_builddir)/emu/touza_emu.$(FC_MODEXT): 

$(top_builddir)/emu/touza_emu_ugg.$(FC_MODEXT): 

$(top_builddir)/nio/touza_nio.$(FC_MODEXT): 

$(top_builddir)/std/touza_std.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_env.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_fun.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_log.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_mwe.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_prc.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_sus.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_utl.$(FC_MODEXT): 

$(top_builddir)/touza_config.h: 

$(top_srcdir)/touza.h: 

