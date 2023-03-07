### automatic dependency
$(builddir)/calendar.lo: $(srcdir)/calendar.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/calendar.lo: $(calendar_lo_MODULE_DEPS)
if enable_module_dependencies
calendar_lo_MODULE_DEPS = $(builddir)/touza_cal_primitive.$(FC_MODEXT) $(builddir)/touza_cal_core.$(FC_MODEXT)
endif

$(builddir)/calendar.o: $(srcdir)/calendar.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/calendar.o: $(calendar_o_MODULE_DEPS)
if enable_module_dependencies
calendar_o_MODULE_DEPS = $(builddir)/touza_cal_primitive.$(FC_MODEXT) $(builddir)/touza_cal_core.$(FC_MODEXT) $(builddir)/touza_cal.$(FC_MODEXT)
endif

$(builddir)/calendar_coco.o: $(srcdir)/calendar_coco.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/calendar_coco.o: $(calendar_coco_o_MODULE_DEPS)
if enable_module_dependencies
calendar_coco_o_MODULE_DEPS = $(builddir)/touza_cal_primitive.$(FC_MODEXT) $(builddir)/touza_cal.$(FC_MODEXT) $(builddir)/touza_cal_coco.$(FC_MODEXT)
endif

$(builddir)/calendar_core.lo: $(srcdir)/calendar_core.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/calendar_core.lo: $(calendar_core_lo_MODULE_DEPS)
if enable_module_dependencies
calendar_core_lo_MODULE_DEPS = $(builddir)/touza_cal_primitive.$(FC_MODEXT)
endif

$(builddir)/calendar_core.o: $(srcdir)/calendar_core.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/calendar_core.o: $(calendar_core_o_MODULE_DEPS)
if enable_module_dependencies
calendar_core_o_MODULE_DEPS = $(builddir)/touza_cal_primitive.$(FC_MODEXT) $(builddir)/touza_cal_core.$(FC_MODEXT)
endif

$(builddir)/calendar_ils.lo: $(srcdir)/calendar_ils.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h $(top_srcdir)/std/touza_std.h
$(builddir)/calendar_ils.lo: $(calendar_ils_lo_MODULE_DEPS)
if enable_module_dependencies
calendar_ils_lo_MODULE_DEPS = $(builddir)/touza_cal.$(FC_MODEXT)
endif

$(builddir)/calendar_ils.o: $(srcdir)/calendar_ils.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h $(top_srcdir)/std/touza_std.h
$(builddir)/calendar_ils.o: $(calendar_ils_o_MODULE_DEPS)
if enable_module_dependencies
calendar_ils_o_MODULE_DEPS = $(builddir)/touza_cal.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_cal_ils.$(FC_MODEXT)
endif

$(builddir)/calendar_matsiro.lo: $(srcdir)/calendar_matsiro.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h $(top_srcdir)/std/touza_std.h
$(builddir)/calendar_matsiro.lo: $(calendar_matsiro_lo_MODULE_DEPS)
if enable_module_dependencies
calendar_matsiro_lo_MODULE_DEPS = $(builddir)/touza_cal.$(FC_MODEXT)
endif

$(builddir)/calendar_matsiro.o: $(srcdir)/calendar_matsiro.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h $(top_srcdir)/std/touza_std.h
$(builddir)/calendar_matsiro.o: $(calendar_matsiro_o_MODULE_DEPS)
if enable_module_dependencies
calendar_matsiro_o_MODULE_DEPS = $(builddir)/touza_cal.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_cal_matsiro.$(FC_MODEXT)
endif

$(builddir)/calendar_miroc.o: $(srcdir)/calendar_miroc.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/calendar_miroc.o: $(calendar_miroc_o_MODULE_DEPS)
if enable_module_dependencies
calendar_miroc_o_MODULE_DEPS = $(builddir)/touza_cal.$(FC_MODEXT) $(builddir)/touza_cal_primitive.$(FC_MODEXT) $(builddir)/touza_cal_miroc.$(FC_MODEXT) $(builddir)/touza_cal_core.$(FC_MODEXT)
endif

$(builddir)/calendar_primitive.lo: $(srcdir)/calendar_primitive.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/calendar_primitive.lo: $(calendar_primitive_lo_MODULE_DEPS)
if enable_module_dependencies
calendar_primitive_lo_MODULE_DEPS = $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/calendar_primitive.o: $(srcdir)/calendar_primitive.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/calendar_primitive.o: $(calendar_primitive_o_MODULE_DEPS)
if enable_module_dependencies
calendar_primitive_o_MODULE_DEPS = $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/test_calendar-calendar.o: $(srcdir)/calendar.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/test_calendar-calendar.o: $(test_calendar_calendar_o_MODULE_DEPS)
if enable_module_dependencies
test_calendar_calendar_o_MODULE_DEPS = $(builddir)/touza_cal_primitive.$(FC_MODEXT) $(builddir)/touza_cal_core.$(FC_MODEXT) $(builddir)/touza_cal.$(FC_MODEXT)
endif

$(builddir)/test_calendar-calendar_miroc.o: $(srcdir)/calendar_miroc.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/test_calendar-calendar_miroc.o: $(test_calendar_calendar_miroc_o_MODULE_DEPS)
if enable_module_dependencies
test_calendar_calendar_miroc_o_MODULE_DEPS = $(builddir)/touza_cal.$(FC_MODEXT) $(builddir)/touza_cal_primitive.$(FC_MODEXT) $(builddir)/touza_cal_miroc.$(FC_MODEXT) $(builddir)/touza_cal_core.$(FC_MODEXT)
endif

$(builddir)/test_calendar.o: $(srcdir)/test_calendar.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h $(top_srcdir)/std/touza_std.h
$(builddir)/test_calendar.o: $(test_calendar_o_MODULE_DEPS)
if enable_module_dependencies
test_calendar_o_MODULE_DEPS = $(builddir)/touza_cal_miroc.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/test_calendar_coco-calendar.o: $(srcdir)/calendar.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/test_calendar_coco-calendar.o: $(test_calendar_coco_calendar_o_MODULE_DEPS)
if enable_module_dependencies
test_calendar_coco_calendar_o_MODULE_DEPS = $(builddir)/touza_cal_primitive.$(FC_MODEXT) $(builddir)/touza_cal_core.$(FC_MODEXT)
endif

$(builddir)/test_calendar_coco-calendar_coco.o: $(srcdir)/calendar_coco.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/test_calendar_coco-calendar_coco.o: $(test_calendar_coco_calendar_coco_o_MODULE_DEPS)
if enable_module_dependencies
test_calendar_coco_calendar_coco_o_MODULE_DEPS = $(builddir)/touza_cal_primitive.$(FC_MODEXT) $(builddir)/touza_cal.$(FC_MODEXT) $(builddir)/touza_cal_coco.$(FC_MODEXT)
endif

$(builddir)/test_calendar_coco-calendar_core.o: $(srcdir)/calendar_core.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/test_calendar_coco-calendar_core.o: $(test_calendar_coco_calendar_core_o_MODULE_DEPS)
if enable_module_dependencies
test_calendar_coco_calendar_core_o_MODULE_DEPS = $(builddir)/touza_cal_primitive.$(FC_MODEXT)
endif

$(builddir)/test_calendar_coco-calendar_primitive.o: $(srcdir)/calendar_primitive.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/test_calendar_coco-calendar_primitive.o: $(test_calendar_coco_calendar_primitive_o_MODULE_DEPS)
if enable_module_dependencies
test_calendar_coco_calendar_primitive_o_MODULE_DEPS = $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/test_calendar_core-calendar_core.o: $(srcdir)/calendar_core.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/test_calendar_core-calendar_core.o: $(test_calendar_core_calendar_core_o_MODULE_DEPS)
if enable_module_dependencies
test_calendar_core_calendar_core_o_MODULE_DEPS = $(builddir)/touza_cal_primitive.$(FC_MODEXT) $(builddir)/touza_cal_core.$(FC_MODEXT)
endif

$(builddir)/test_calendar_ils-calendar_ils.o: $(srcdir)/calendar_ils.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h $(top_srcdir)/std/touza_std.h
$(builddir)/test_calendar_ils-calendar_ils.o: $(test_calendar_ils_calendar_ils_o_MODULE_DEPS)
if enable_module_dependencies
test_calendar_ils_calendar_ils_o_MODULE_DEPS = $(builddir)/touza_cal.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_cal_ils.$(FC_MODEXT)
endif

$(builddir)/test_calendar_matsiro-calendar_matsiro.o: $(srcdir)/calendar_matsiro.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h $(top_srcdir)/std/touza_std.h
$(builddir)/test_calendar_matsiro-calendar_matsiro.o: $(test_calendar_matsiro_calendar_matsiro_o_MODULE_DEPS)
if enable_module_dependencies
test_calendar_matsiro_calendar_matsiro_o_MODULE_DEPS = $(builddir)/touza_cal.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_cal_matsiro.$(FC_MODEXT)
endif

$(builddir)/test_calendar_trad-test_calendar.o: $(srcdir)/test_calendar.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h $(top_srcdir)/std/touza_std.h
$(builddir)/test_calendar_trad-test_calendar.o: $(test_calendar_trad_test_calendar_o_MODULE_DEPS)
if enable_module_dependencies
test_calendar_trad_test_calendar_o_MODULE_DEPS = $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/touza_cal.$(FC_MODEXT): $(srcdir)/calendar.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/touza_cal.$(FC_MODEXT): $(touza_cal_mod_MODULE_DEPS)
if enable_module_dependencies
touza_cal_mod_MODULE_DEPS = $(builddir)/touza_cal_primitive.$(FC_MODEXT) $(builddir)/touza_cal_core.$(FC_MODEXT)
endif

$(builddir)/touza_cal_coco.$(FC_MODEXT): $(srcdir)/calendar_coco.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/touza_cal_coco.$(FC_MODEXT): $(touza_cal_coco_mod_MODULE_DEPS)
if enable_module_dependencies
touza_cal_coco_mod_MODULE_DEPS = $(builddir)/touza_cal_primitive.$(FC_MODEXT) $(builddir)/touza_cal.$(FC_MODEXT)
endif

$(builddir)/touza_cal_core.$(FC_MODEXT): $(srcdir)/calendar_core.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/touza_cal_core.$(FC_MODEXT): $(touza_cal_core_mod_MODULE_DEPS)
if enable_module_dependencies
touza_cal_core_mod_MODULE_DEPS = $(builddir)/touza_cal_primitive.$(FC_MODEXT)
endif

$(builddir)/touza_cal_ils.$(FC_MODEXT): $(srcdir)/calendar_ils.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h $(top_srcdir)/std/touza_std.h
$(builddir)/touza_cal_ils.$(FC_MODEXT): $(touza_cal_ils_mod_MODULE_DEPS)
if enable_module_dependencies
touza_cal_ils_mod_MODULE_DEPS = $(builddir)/touza_cal.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/touza_cal_matsiro.$(FC_MODEXT): $(srcdir)/calendar_matsiro.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h $(top_srcdir)/std/touza_std.h
$(builddir)/touza_cal_matsiro.$(FC_MODEXT): $(touza_cal_matsiro_mod_MODULE_DEPS)
if enable_module_dependencies
touza_cal_matsiro_mod_MODULE_DEPS = $(builddir)/touza_cal.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/touza_cal_miroc.$(FC_MODEXT): $(srcdir)/calendar_miroc.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/touza_cal_miroc.$(FC_MODEXT): $(touza_cal_miroc_mod_MODULE_DEPS)
if enable_module_dependencies
touza_cal_miroc_mod_MODULE_DEPS = $(builddir)/touza_cal.$(FC_MODEXT) $(builddir)/touza_cal_primitive.$(FC_MODEXT) $(builddir)/touza_cal_core.$(FC_MODEXT)
endif

$(builddir)/touza_cal_primitive.$(FC_MODEXT): $(srcdir)/calendar_primitive.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_cal.h $(top_srcdir)/touza.h
$(builddir)/touza_cal_primitive.$(FC_MODEXT): $(touza_cal_primitive_mod_MODULE_DEPS)
if enable_module_dependencies
touza_cal_primitive_mod_MODULE_DEPS = $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(srcdir)/touza_cal.h: 

$(top_builddir)/std/touza_std.$(FC_MODEXT): 

$(top_builddir)/touza_config.h: 

$(top_srcdir)/std/touza_std.h: 

$(top_srcdir)/touza.h: 

