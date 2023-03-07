### automatic dependency
$(builddir)/ppp.lo: $(srcdir)/ppp.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/ppp.lo: $(ppp_lo_MODULE_DEPS)
if enable_module_dependencies
ppp_lo_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT) $(builddir)/touza_ppp_king.$(FC_MODEXT) $(builddir)/touza_ppp_comm.$(FC_MODEXT)
endif

$(builddir)/ppp.o: $(srcdir)/ppp.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/ppp.o: $(ppp_o_MODULE_DEPS)
if enable_module_dependencies
ppp_o_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT) $(builddir)/touza_ppp_king.$(FC_MODEXT) $(builddir)/touza_ppp_comm.$(FC_MODEXT) $(builddir)/touza_ppp.$(FC_MODEXT)
endif

$(builddir)/ppp_amng.lo: $(srcdir)/ppp_amng.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/ppp_amng.lo: $(ppp_amng_lo_MODULE_DEPS)
if enable_module_dependencies
ppp_amng_lo_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/ppp_amng.o: $(srcdir)/ppp_amng.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/ppp_amng.o: $(ppp_amng_o_MODULE_DEPS)
if enable_module_dependencies
ppp_amng_o_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT)
endif

$(builddir)/ppp_comm.lo: $(srcdir)/ppp_comm.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/ppp_comm.lo: $(ppp_comm_lo_MODULE_DEPS)
if enable_module_dependencies
ppp_comm_lo_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT)
endif

$(builddir)/ppp_comm.o: $(srcdir)/ppp_comm.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/ppp_comm.o: $(ppp_comm_o_MODULE_DEPS)
if enable_module_dependencies
ppp_comm_o_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT) $(builddir)/touza_ppp_comm.$(FC_MODEXT)
endif

$(builddir)/ppp_king.lo: $(srcdir)/ppp_king.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/ppp_king.lo: $(ppp_king_lo_MODULE_DEPS)
if enable_module_dependencies
ppp_king_lo_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT)
endif

$(builddir)/ppp_king.o: $(srcdir)/ppp_king.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/ppp_king.o: $(ppp_king_o_MODULE_DEPS)
if enable_module_dependencies
ppp_king_o_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT) $(builddir)/touza_ppp_king.$(FC_MODEXT)
endif

$(builddir)/ppp_miroc.o: $(srcdir)/ppp_miroc.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/ppp_miroc.o: $(ppp_miroc_o_MODULE_DEPS)
if enable_module_dependencies
ppp_miroc_o_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(builddir)/touza_ppp.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(top_builddir)/emu/touza_emu.$(FC_MODEXT) $(builddir)/touza_ppp_miroc.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT)
endif

$(builddir)/ppp_std.lo: $(srcdir)/ppp_std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/ppp_std.lo: $(ppp_std_lo_MODULE_DEPS)
if enable_module_dependencies
ppp_std_lo_MODULE_DEPS = $(top_builddir)/std/touza_std_utl.$(FC_MODEXT) $(top_builddir)/std/touza_std_log.$(FC_MODEXT) $(top_builddir)/std/touza_std_mwe.$(FC_MODEXT) $(top_builddir)/std/touza_std_env.$(FC_MODEXT) $(top_builddir)/std/touza_std_htb.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/ppp_std.o: $(srcdir)/ppp_std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/ppp_std.o: $(ppp_std_o_MODULE_DEPS)
if enable_module_dependencies
ppp_std_o_MODULE_DEPS = $(top_builddir)/std/touza_std_utl.$(FC_MODEXT) $(top_builddir)/std/touza_std_log.$(FC_MODEXT) $(top_builddir)/std/touza_std_mwe.$(FC_MODEXT) $(top_builddir)/std/touza_std_env.$(FC_MODEXT) $(top_builddir)/std/touza_std_htb.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_ppp_std.$(FC_MODEXT)
endif

$(builddir)/test_ppp-ppp.o: $(srcdir)/ppp.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/test_ppp-ppp.o: $(test_ppp_ppp_o_MODULE_DEPS)
if enable_module_dependencies
test_ppp_ppp_o_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT) $(builddir)/touza_ppp_king.$(FC_MODEXT) $(builddir)/touza_ppp_comm.$(FC_MODEXT) $(builddir)/touza_ppp.$(FC_MODEXT)
endif

$(builddir)/test_ppp_amng-ppp_amng.o: $(srcdir)/ppp_amng.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/test_ppp_amng-ppp_amng.o: $(test_ppp_amng_ppp_amng_o_MODULE_DEPS)
if enable_module_dependencies
test_ppp_amng_ppp_amng_o_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT)
endif

$(builddir)/test_ppp_comm-ppp_comm.o: $(srcdir)/ppp_comm.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/test_ppp_comm-ppp_comm.o: $(test_ppp_comm_ppp_comm_o_MODULE_DEPS)
if enable_module_dependencies
test_ppp_comm_ppp_comm_o_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT) $(builddir)/touza_ppp_comm.$(FC_MODEXT)
endif

$(builddir)/test_ppp_king-ppp_king.o: $(srcdir)/ppp_king.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/test_ppp_king-ppp_king.o: $(test_ppp_king_ppp_king_o_MODULE_DEPS)
if enable_module_dependencies
test_ppp_king_ppp_king_o_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT) $(builddir)/touza_ppp_king.$(FC_MODEXT)
endif

$(builddir)/test_ppp_miroc_a-ppp_miroc.o: $(srcdir)/ppp_miroc.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/test_ppp_miroc_a-ppp_miroc.o: $(test_ppp_miroc_a_ppp_miroc_o_MODULE_DEPS)
if enable_module_dependencies
test_ppp_miroc_a_ppp_miroc_o_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(builddir)/touza_ppp.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(top_builddir)/emu/touza_emu.$(FC_MODEXT) $(builddir)/touza_ppp_miroc.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT)
endif

$(builddir)/test_ppp_miroc_o-ppp_miroc.o: $(srcdir)/ppp_miroc.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/test_ppp_miroc_o-ppp_miroc.o: $(test_ppp_miroc_o_ppp_miroc_o_MODULE_DEPS)
if enable_module_dependencies
test_ppp_miroc_o_ppp_miroc_o_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(builddir)/touza_ppp.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(top_builddir)/emu/touza_emu.$(FC_MODEXT) $(builddir)/touza_ppp_miroc.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT)
endif

$(builddir)/test_ppp_miroc_s-ppp_miroc.o: $(srcdir)/ppp_miroc.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/test_ppp_miroc_s-ppp_miroc.o: $(test_ppp_miroc_s_ppp_miroc_o_MODULE_DEPS)
if enable_module_dependencies
test_ppp_miroc_s_ppp_miroc_o_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(builddir)/touza_ppp.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(top_builddir)/emu/touza_emu.$(FC_MODEXT) $(builddir)/touza_ppp_miroc.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT)
endif

$(builddir)/test_ppp_std-ppp_std.o: $(srcdir)/ppp_std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/test_ppp_std-ppp_std.o: $(test_ppp_std_ppp_std_o_MODULE_DEPS)
if enable_module_dependencies
test_ppp_std_ppp_std_o_MODULE_DEPS = $(top_builddir)/std/touza_std_utl.$(FC_MODEXT) $(top_builddir)/std/touza_std_log.$(FC_MODEXT) $(top_builddir)/std/touza_std_mwe.$(FC_MODEXT) $(top_builddir)/std/touza_std_env.$(FC_MODEXT) $(top_builddir)/std/touza_std_htb.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(builddir)/touza_ppp_std.$(FC_MODEXT)
endif

$(builddir)/touza_ppp.$(FC_MODEXT): $(srcdir)/ppp.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/touza_ppp.$(FC_MODEXT): $(touza_ppp_mod_MODULE_DEPS)
if enable_module_dependencies
touza_ppp_mod_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT) $(builddir)/touza_ppp_king.$(FC_MODEXT) $(builddir)/touza_ppp_comm.$(FC_MODEXT)
endif

$(builddir)/touza_ppp_amng.$(FC_MODEXT): $(srcdir)/ppp_amng.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/touza_ppp_amng.$(FC_MODEXT): $(touza_ppp_amng_mod_MODULE_DEPS)
if enable_module_dependencies
touza_ppp_amng_mod_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(builddir)/touza_ppp_comm.$(FC_MODEXT): $(srcdir)/ppp_comm.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/touza_ppp_comm.$(FC_MODEXT): $(touza_ppp_comm_mod_MODULE_DEPS)
if enable_module_dependencies
touza_ppp_comm_mod_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT)
endif

$(builddir)/touza_ppp_king.$(FC_MODEXT): $(srcdir)/ppp_king.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/touza_ppp_king.$(FC_MODEXT): $(touza_ppp_king_mod_MODULE_DEPS)
if enable_module_dependencies
touza_ppp_king_mod_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT)
endif

$(builddir)/touza_ppp_miroc.$(FC_MODEXT): $(srcdir)/ppp_miroc.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/touza_ppp_miroc.$(FC_MODEXT): $(touza_ppp_miroc_mod_MODULE_DEPS)
if enable_module_dependencies
touza_ppp_miroc_mod_MODULE_DEPS = $(builddir)/touza_ppp_std.$(FC_MODEXT) $(builddir)/touza_ppp.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT) $(top_builddir)/emu/touza_emu.$(FC_MODEXT) $(builddir)/touza_ppp_amng.$(FC_MODEXT)
endif

$(builddir)/touza_ppp_std.$(FC_MODEXT): $(srcdir)/ppp_std.F90 $(top_builddir)/touza_config.h $(srcdir)/touza_ppp.h $(top_srcdir)/touza.h
$(builddir)/touza_ppp_std.$(FC_MODEXT): $(touza_ppp_std_mod_MODULE_DEPS)
if enable_module_dependencies
touza_ppp_std_mod_MODULE_DEPS = $(top_builddir)/std/touza_std_utl.$(FC_MODEXT) $(top_builddir)/std/touza_std_log.$(FC_MODEXT) $(top_builddir)/std/touza_std_mwe.$(FC_MODEXT) $(top_builddir)/std/touza_std_env.$(FC_MODEXT) $(top_builddir)/std/touza_std_htb.$(FC_MODEXT) $(top_builddir)/std/touza_std.$(FC_MODEXT)
endif

$(srcdir)/touza_ppp.h: 

$(top_builddir)/emu/touza_emu.$(FC_MODEXT): 

$(top_builddir)/std/touza_std.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_env.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_htb.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_log.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_mwe.$(FC_MODEXT): 

$(top_builddir)/std/touza_std_utl.$(FC_MODEXT): 

$(top_builddir)/touza_config.h: 

$(top_srcdir)/touza.h: 

