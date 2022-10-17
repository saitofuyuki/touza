### automatic dependency
$(builddir)/ami_std.lo: $(srcdir)/ami_std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami_std.lo: $(ami_std_lo_MODULE_DEPS)
if enable_module_dependencies
ami_std_lo_MODULE_DEPS = $(top_builddir)/std/touza_std_prc.$(FC_MODEXT) $(top_builddir)/std/touza_std_utl.$(FC_MODEXT) $(top_builddir)/std/touza_std_log.$(FC_MODEXT) $(top_builddir)/std/touza_std_mwe.$(FC_MODEXT) $(top_builddir)/std/touza_std_env.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/ami_std.o: $(srcdir)/ami_std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami_std.o: $(ami_std_o_MODULE_DEPS)
if enable_module_dependencies
ami_std_o_MODULE_DEPS = $(top_builddir)/std/touza_std_prc.$(FC_MODEXT) $(top_builddir)/std/touza_std_utl.$(FC_MODEXT) $(top_builddir)/std/touza_std_log.$(FC_MODEXT) $(top_builddir)/std/touza_std_mwe.$(FC_MODEXT) $(top_builddir)/std/touza_std_env.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/ami_table.lo: $(srcdir)/ami_table.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami_table.lo: $(ami_table_lo_MODULE_DEPS)
if enable_module_dependencies
ami_table_lo_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT)
endif

$(builddir)/ami_table.o: $(srcdir)/ami_table.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/ami_table.o: $(ami_table_o_MODULE_DEPS)
if enable_module_dependencies
ami_table_o_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(builddir)/touza_ami_table.$(FC_MODEXT)
endif

$(builddir)/test_ami_table-ami_table.o: $(srcdir)/ami_table.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/test_ami_table-ami_table.o: $(test_ami_table_ami_table_o_MODULE_DEPS)
if enable_module_dependencies
test_ami_table_ami_table_o_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT) $(builddir)/touza_ami_table.$(FC_MODEXT)
endif

$(builddir)/touza_ami_std.$(FC_MODEXT): $(srcdir)/ami_std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/touza_ami_std.$(FC_MODEXT): $(touza_ami_std_mod_MODULE_DEPS)
if enable_module_dependencies
touza_ami_std_mod_MODULE_DEPS = $(top_builddir)/std/touza_std_prc.$(FC_MODEXT) $(top_builddir)/std/touza_std_utl.$(FC_MODEXT) $(top_builddir)/std/touza_std_log.$(FC_MODEXT) $(top_builddir)/std/touza_std_mwe.$(FC_MODEXT) $(top_builddir)/std/touza_std_env.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/touza_ami_table.$(FC_MODEXT): $(srcdir)/ami_table.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ami.h $(top_srcdir)/touza.h
$(builddir)/touza_ami_table.$(FC_MODEXT): $(touza_ami_table_mod_MODULE_DEPS)
if enable_module_dependencies
touza_ami_table_mod_MODULE_DEPS = $(builddir)/touza_ami_std.$(FC_MODEXT)
endif

$(srcdir)/touza_ami.h: 

$(top_builddir)/std/touza_std.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_env.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_log.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_mwe.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_prc.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_utl.$(FC_MODEXT): 

$(top_builddir)/touza_config.h: 

$(top_srcdir)/touza.h: 

