### automatic dependency
$(builddir)/cal/touza_cal.$(FC_MODEXT): 

$(builddir)/nio/touza_nio.$(FC_MODEXT): 

$(builddir)/ppp/touza_ppp.$(FC_MODEXT): 

$(builddir)/std/touza_std.$(FC_MODEXT): 

$(builddir)/test_touza-touza.o: $(srcdir)/touza.F90 $(builddir)/touza_config.h $(srcdir)/touza.h
$(builddir)/test_touza-touza.o: $(test_touza_touza_o_MODULE_DEPS)
if enable_module_dependencies
test_touza_touza_o_MODULE_DEPS = $(builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/cal/touza_cal.$(FC_MODEXT) $(builddir)/trp/touza_trp.$(FC_MODEXT) $(builddir)/nio/touza_nio.$(FC_MODEXT) $(builddir)/ppp/touza_ppp.$(FC_MODEXT) $(builddir)/touza.$(FC_MODEXT)
endif

$(builddir)/touza.lo: $(srcdir)/touza.F90 $(builddir)/touza_config.h $(srcdir)/touza.h
$(builddir)/touza.lo: $(touza_lo_MODULE_DEPS)
if enable_module_dependencies
touza_lo_MODULE_DEPS = $(builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/cal/touza_cal.$(FC_MODEXT) $(builddir)/trp/touza_trp.$(FC_MODEXT) $(builddir)/nio/touza_nio.$(FC_MODEXT) $(builddir)/ppp/touza_ppp.$(FC_MODEXT)
endif

$(builddir)/touza.$(FC_MODEXT): $(srcdir)/touza.F90 $(builddir)/touza_config.h $(srcdir)/touza.h
$(builddir)/touza.$(FC_MODEXT): $(touza_mod_MODULE_DEPS)
if enable_module_dependencies
touza_mod_MODULE_DEPS = $(builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/cal/touza_cal.$(FC_MODEXT) $(builddir)/trp/touza_trp.$(FC_MODEXT) $(builddir)/nio/touza_nio.$(FC_MODEXT) $(builddir)/ppp/touza_ppp.$(FC_MODEXT)
endif

$(builddir)/touza.o: $(srcdir)/touza.F90 $(builddir)/touza_config.h $(srcdir)/touza.h
$(builddir)/touza.o: $(touza_o_MODULE_DEPS)
if enable_module_dependencies
touza_o_MODULE_DEPS = $(builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/cal/touza_cal.$(FC_MODEXT) $(builddir)/trp/touza_trp.$(FC_MODEXT) $(builddir)/nio/touza_nio.$(FC_MODEXT) $(builddir)/ppp/touza_ppp.$(FC_MODEXT) $(builddir)/touza.$(FC_MODEXT)
endif

$(builddir)/touza_config.h: 

$(builddir)/trp/touza_trp.$(FC_MODEXT): 

$(srcdir)/touza.h: 

