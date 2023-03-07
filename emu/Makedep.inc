### automatic dependency
$(builddir)/emu.lo: $(srcdir)/emu.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_emu.h $(top_srcdir)/std/touza_std.h $(top_srcdir)/touza.h
$(builddir)/emu.lo: $(emu_lo_MODULE_DEPS)
if enable_module_dependencies
emu_lo_MODULE_DEPS = $(builddir)/touza_emu_usi.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/emu.o: $(srcdir)/emu.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_emu.h $(top_srcdir)/std/touza_std.h $(top_srcdir)/touza.h
$(builddir)/emu.o: $(emu_o_MODULE_DEPS)
if enable_module_dependencies
emu_o_MODULE_DEPS = $(builddir)/touza_emu_usi.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/emu_msg.o: $(srcdir)/emu_msg.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_emu.h $(top_srcdir)/std/touza_std.h $(top_srcdir)/touza.h
$(builddir)/emu_msg.o: $(emu_msg_o_MODULE_DEPS)
if enable_module_dependencies
emu_msg_o_MODULE_DEPS = $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_emu_msg.$(FC_MODEXT)
endif

$(builddir)/emu_rpx.lo: $(srcdir)/emu_rpx.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_emu.h

$(builddir)/emu_rpx.o: $(srcdir)/emu_rpx.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_emu.h

$(builddir)/emu_usi.lo: $(srcdir)/emu_usi.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_emu.h $(top_srcdir)/std/touza_std.h $(top_srcdir)/touza.h
$(builddir)/emu_usi.lo: $(emu_usi_lo_MODULE_DEPS)
if enable_module_dependencies
emu_usi_lo_MODULE_DEPS = $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_emu_usi.$(FC_MODEXT)
endif

$(builddir)/emu_usi.o: $(srcdir)/emu_usi.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_emu.h $(top_srcdir)/std/touza_std.h $(top_srcdir)/touza.h
$(builddir)/emu_usi.o: $(emu_usi_o_MODULE_DEPS)
if enable_module_dependencies
emu_usi_o_MODULE_DEPS = $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_emu_usi.$(FC_MODEXT)
endif

$(builddir)/test_emu_msg-emu_msg.o: $(srcdir)/emu_msg.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_emu.h $(top_srcdir)/std/touza_std.h $(top_srcdir)/touza.h
$(builddir)/test_emu_msg-emu_msg.o: $(test_emu_msg_emu_msg_o_MODULE_DEPS)
if enable_module_dependencies
test_emu_msg_emu_msg_o_MODULE_DEPS = $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_emu_msg.$(FC_MODEXT)
endif

$(builddir)/test_emu_usi-emu_usi.o: $(srcdir)/emu_usi.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_emu.h $(top_srcdir)/std/touza_std.h $(top_srcdir)/touza.h
$(builddir)/test_emu_usi-emu_usi.o: $(test_emu_usi_emu_usi_o_MODULE_DEPS)
if enable_module_dependencies
test_emu_usi_emu_usi_o_MODULE_DEPS = $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_emu_usi.$(FC_MODEXT)
endif

$(builddir)/touza_emu.$(FC_MODEXT): $(srcdir)/emu.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_emu.h $(top_srcdir)/std/touza_std.h $(top_srcdir)/touza.h
$(builddir)/touza_emu.$(FC_MODEXT): $(touza_emu_mod_MODULE_DEPS)
if enable_module_dependencies
touza_emu_mod_MODULE_DEPS = $(builddir)/touza_emu_usi.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/touza_emu_msg.$(FC_MODEXT): $(srcdir)/emu_msg.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_emu.h $(top_srcdir)/std/touza_std.h $(top_srcdir)/touza.h
$(builddir)/touza_emu_msg.$(FC_MODEXT): $(touza_emu_msg_mod_MODULE_DEPS)
if enable_module_dependencies
touza_emu_msg_mod_MODULE_DEPS = $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/touza_emu_rpx.$(FC_MODEXT): $(srcdir)/emu_rpx.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_emu.h

$(builddir)/touza_emu_usi.$(FC_MODEXT): $(srcdir)/emu_usi.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_emu.h $(top_srcdir)/std/touza_std.h $(top_srcdir)/touza.h
$(builddir)/touza_emu_usi.$(FC_MODEXT): $(touza_emu_usi_mod_MODULE_DEPS)
if enable_module_dependencies
touza_emu_usi_mod_MODULE_DEPS = $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(srcdir)/touza_emu.h: 

$(top_builddir)/std/touza_std.$(FC_MODEXT): 

$(top_builddir)/touza_config.h: 

$(top_srcdir)/std/touza_std.h: 

$(top_srcdir)/touza.h: 

