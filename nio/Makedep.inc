### automatic dependency
$(builddir)/calendar_miroc.o: $(builddir)/calendar_miroc.F90 $(top_builddir)/touza_config.h $(top_srcdir)/cal/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/calendar_miroc.o: $(calendar_miroc_o_MODULE_DEPS)
if enable_module_dependencies
calendar_miroc_o_MODULE_DEPS = $(top_builddir)/cal/touza_cal.$(FC_MODEXT) $(top_builddir)/cal/touza_cal_primitive.$(FC_MODEXT) $(builddir)/touza_cal_miroc.$(FC_MODEXT) $(top_builddir)/cal/touza_cal_core.$(FC_MODEXT)
endif

$(builddir)/nio.lo: $(srcdir)/nio.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/nio.lo: $(nio_lo_MODULE_DEPS)
if enable_module_dependencies
nio_lo_MODULE_DEPS = $(builddir)/touza_nio_std.$(FC_MODEXT) $(builddir)/touza_nio_header.$(FC_MODEXT) $(builddir)/touza_nio_record.$(FC_MODEXT)
endif

$(builddir)/nio.o: $(srcdir)/nio.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/nio.o: $(nio_o_MODULE_DEPS)
if enable_module_dependencies
nio_o_MODULE_DEPS = $(builddir)/touza_nio_std.$(FC_MODEXT) $(builddir)/touza_nio_header.$(FC_MODEXT) $(builddir)/touza_nio_record.$(FC_MODEXT) $(builddir)/touza_nio.$(FC_MODEXT)
endif

$(builddir)/nio_header.lo: $(srcdir)/nio_header.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/nio_header.lo: $(nio_header_lo_MODULE_DEPS)
if enable_module_dependencies
nio_header_lo_MODULE_DEPS = $(builddir)/touza_nio_std.$(FC_MODEXT)
endif

$(builddir)/nio_header.o: $(srcdir)/nio_header.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/nio_header.o: $(nio_header_o_MODULE_DEPS)
if enable_module_dependencies
nio_header_o_MODULE_DEPS = $(builddir)/touza_nio_std.$(FC_MODEXT) $(builddir)/touza_nio_header.$(FC_MODEXT)
endif

$(builddir)/nio_miroc.o: $(srcdir)/nio_miroc.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/nio_miroc.o: $(nio_miroc_o_MODULE_DEPS)
if enable_module_dependencies
nio_miroc_o_MODULE_DEPS = $(builddir)/touza_nio.$(FC_MODEXT) $(builddir)/touza_nio_header.$(FC_MODEXT) $(builddir)/touza_nio_std.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_nio_miroc.$(FC_MODEXT) $(builddir)/touza_nio_record.$(FC_MODEXT)
endif

$(builddir)/nio_record.lo: $(srcdir)/nio_record.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/nio_record.lo: $(nio_record_lo_MODULE_DEPS)
if enable_module_dependencies
nio_record_lo_MODULE_DEPS = $(builddir)/touza_nio_std.$(FC_MODEXT) $(builddir)/touza_nio_header.$(FC_MODEXT) $(top_builddir)/trp/touza_trp.$(FC_MODEXT)
endif

$(builddir)/nio_record.o: $(srcdir)/nio_record.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/nio_record.o: $(nio_record_o_MODULE_DEPS)
if enable_module_dependencies
nio_record_o_MODULE_DEPS = $(builddir)/touza_nio_std.$(FC_MODEXT) $(builddir)/touza_nio_header.$(FC_MODEXT) $(top_builddir)/trp/touza_trp.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_nio_record.$(FC_MODEXT) $(top_builddir)/std/touza_std_sus.$(FC_MODEXT)
endif

$(builddir)/nio_std.lo: $(srcdir)/nio_std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/nio_std.lo: $(nio_std_lo_MODULE_DEPS)
if enable_module_dependencies
nio_std_lo_MODULE_DEPS = $(top_builddir)/std/touza_std_prc.$(FC_MODEXT) $(top_builddir)/std/touza_std_utl.$(FC_MODEXT) $(top_builddir)/std/touza_std_log.$(FC_MODEXT) $(top_builddir)/std/touza_std_env.$(FC_MODEXT) $(top_builddir)/std/touza_std_fun.$(FC_MODEXT) $(top_builddir)/std/touza_std_sus.$(FC_MODEXT) $(top_builddir)/std/touza_std_bld.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/nio_std.o: $(srcdir)/nio_std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/nio_std.o: $(nio_std_o_MODULE_DEPS)
if enable_module_dependencies
nio_std_o_MODULE_DEPS = $(top_builddir)/std/touza_std_prc.$(FC_MODEXT) $(top_builddir)/std/touza_std_utl.$(FC_MODEXT) $(top_builddir)/std/touza_std_log.$(FC_MODEXT) $(top_builddir)/std/touza_std_env.$(FC_MODEXT) $(top_builddir)/std/touza_std_fun.$(FC_MODEXT) $(top_builddir)/std/touza_std_sus.$(FC_MODEXT) $(top_builddir)/std/touza_std_bld.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/test_nio-nio.o: $(srcdir)/nio.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/test_nio-nio.o: $(test_nio_nio_o_MODULE_DEPS)
if enable_module_dependencies
test_nio_nio_o_MODULE_DEPS = $(builddir)/touza_nio_std.$(FC_MODEXT) $(builddir)/touza_nio_header.$(FC_MODEXT) $(builddir)/touza_nio_record.$(FC_MODEXT) $(builddir)/touza_nio.$(FC_MODEXT)
endif

$(builddir)/test_nio_header-nio_header.o: $(srcdir)/nio_header.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/test_nio_header-nio_header.o: $(test_nio_header_nio_header_o_MODULE_DEPS)
if enable_module_dependencies
test_nio_header_nio_header_o_MODULE_DEPS = $(builddir)/touza_nio_std.$(FC_MODEXT) $(builddir)/touza_nio_header.$(FC_MODEXT)
endif

$(builddir)/test_nio_miroc-calendar_miroc.o: $(builddir)/calendar_miroc.F90 $(top_builddir)/touza_config.h $(top_srcdir)/cal/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/test_nio_miroc-calendar_miroc.o: $(test_nio_miroc_calendar_miroc_o_MODULE_DEPS)
if enable_module_dependencies
test_nio_miroc_calendar_miroc_o_MODULE_DEPS = $(top_builddir)/cal/touza_cal.$(FC_MODEXT) $(top_builddir)/cal/touza_cal_primitive.$(FC_MODEXT) $(builddir)/touza_cal_miroc.$(FC_MODEXT) $(top_builddir)/cal/touza_cal_core.$(FC_MODEXT)
endif

$(builddir)/test_nio_miroc-nio_miroc.o: $(srcdir)/nio_miroc.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/test_nio_miroc-nio_miroc.o: $(test_nio_miroc_nio_miroc_o_MODULE_DEPS)
if enable_module_dependencies
test_nio_miroc_nio_miroc_o_MODULE_DEPS = $(builddir)/touza_nio.$(FC_MODEXT) $(builddir)/touza_nio_header.$(FC_MODEXT) $(builddir)/touza_nio_std.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_nio_miroc.$(FC_MODEXT) $(builddir)/touza_nio_record.$(FC_MODEXT)
endif

$(builddir)/test_nio_record-nio_record.o: $(srcdir)/nio_record.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/test_nio_record-nio_record.o: $(test_nio_record_nio_record_o_MODULE_DEPS)
if enable_module_dependencies
test_nio_record_nio_record_o_MODULE_DEPS = $(builddir)/touza_nio_std.$(FC_MODEXT) $(builddir)/touza_nio_header.$(FC_MODEXT) $(top_builddir)/trp/touza_trp.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_nio_record.$(FC_MODEXT) $(top_builddir)/std/touza_std_sus.$(FC_MODEXT)
endif

$(builddir)/touza_cal_miroc.$(FC_MODEXT): $(builddir)/calendar_miroc.F90 $(top_builddir)/touza_config.h $(top_srcdir)/cal/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/touza_cal_miroc.$(FC_MODEXT): $(touza_cal_miroc_mod_MODULE_DEPS)
if enable_module_dependencies
touza_cal_miroc_mod_MODULE_DEPS = $(top_builddir)/cal/touza_cal.$(FC_MODEXT) $(top_builddir)/cal/touza_cal_primitive.$(FC_MODEXT) $(top_builddir)/cal/touza_cal_core.$(FC_MODEXT)
endif

$(builddir)/touza_nio.$(FC_MODEXT): $(srcdir)/nio.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/touza_nio.$(FC_MODEXT): $(touza_nio_mod_MODULE_DEPS)
if enable_module_dependencies
touza_nio_mod_MODULE_DEPS = $(builddir)/touza_nio_std.$(FC_MODEXT) $(builddir)/touza_nio_header.$(FC_MODEXT) $(builddir)/touza_nio_record.$(FC_MODEXT)
endif

$(builddir)/touza_nio_header.$(FC_MODEXT): $(srcdir)/nio_header.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/touza_nio_header.$(FC_MODEXT): $(touza_nio_header_mod_MODULE_DEPS)
if enable_module_dependencies
touza_nio_header_mod_MODULE_DEPS = $(builddir)/touza_nio_std.$(FC_MODEXT)
endif

$(builddir)/touza_nio_miroc.$(FC_MODEXT): $(srcdir)/nio_miroc.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/touza_nio_miroc.$(FC_MODEXT): $(touza_nio_miroc_mod_MODULE_DEPS)
if enable_module_dependencies
touza_nio_miroc_mod_MODULE_DEPS = $(builddir)/touza_nio.$(FC_MODEXT) $(builddir)/touza_nio_header.$(FC_MODEXT) $(builddir)/touza_nio_std.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_nio_record.$(FC_MODEXT)
endif

$(builddir)/touza_nio_record.$(FC_MODEXT): $(srcdir)/nio_record.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/touza_nio_record.$(FC_MODEXT): $(touza_nio_record_mod_MODULE_DEPS)
if enable_module_dependencies
touza_nio_record_mod_MODULE_DEPS = $(builddir)/touza_nio_std.$(FC_MODEXT) $(builddir)/touza_nio_header.$(FC_MODEXT) $(top_builddir)/trp/touza_trp.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(top_builddir)/std/touza_std_sus.$(FC_MODEXT)
endif

$(builddir)/touza_nio_std.$(FC_MODEXT): $(srcdir)/nio_std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_nio.h $(top_srcdir)/touza.h
$(builddir)/touza_nio_std.$(FC_MODEXT): $(touza_nio_std_mod_MODULE_DEPS)
if enable_module_dependencies
touza_nio_std_mod_MODULE_DEPS = $(top_builddir)/std/touza_std_prc.$(FC_MODEXT) $(top_builddir)/std/touza_std_utl.$(FC_MODEXT) $(top_builddir)/std/touza_std_log.$(FC_MODEXT) $(top_builddir)/std/touza_std_env.$(FC_MODEXT) $(top_builddir)/std/touza_std_fun.$(FC_MODEXT) $(top_builddir)/std/touza_std_sus.$(FC_MODEXT) $(top_builddir)/std/touza_std_bld.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(srcdir)/touza_nio.h: 

$(top_builddir)/cal/touza_cal.$(FC_MODEXT): 

$(top_builddir)/cal/touza_cal_core.$(FC_MODEXT): 

$(top_builddir)/cal/touza_cal_primitive.$(FC_MODEXT): 

$(top_builddir)/std/touza_std.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_bld.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_env.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_fun.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_log.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_prc.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_sus.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_utl.$(FC_MODEXT): 

$(top_builddir)/touza_config.h: 

$(top_builddir)/trp/touza_trp.$(FC_MODEXT): 

$(top_srcdir)/cal/touza_cal.h: 

$(top_srcdir)/touza.h: 

