SHELL=/bin/sh
#
# This makefile was produced by /usr/bin/fmgen at 11:21:32 AM on 04/12/96
# If it is invoked by the command line
#	make -f makefile
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable named a.out.
#
# If it is invoked by the command line
#	make -f makefile a.out.prof
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable which profiles
# named a.out.prof.
#
# To remove all the objects but leave the executables use the command line
#	make -f makefile clean
#
# To remove everything but the source files use the command line
#	make -f makefile clobber
#
# To remove the source files created by /usr/bin/fmgen and this makefile
# use the command line
#	make -f makefile void
#
# The parameters SRCS and OBJS should not need to be changed.  If, however,
# you need to add a new module add the name of the source module to the
# SRCS parameter and add the name of the resulting object file to the OBJS
# parameter.  The new modules are not limited to fortran, but may be C, YACC,
# LEX, or CAL.  An explicit rule will need to be added for PASCAL modules.
#
SRCS=	main_prepbufr.f90 read_prepbufr_raw.f90 read_prepbufr_sfc_ps.f90 \
	ead_prepbufr_sfc_ps_mor.f90 read_prepbufr_sfc_q.f90 read_prepbufr_sfc_t.f90 \
	read_prepbufr_sfc_w.f90 read_prepbufr_sfc_q_mor.f90 read_prepbufr_sfc_t_mor.f90 read_prepbufr_sfc_w_mor.f90 \   
	read_prepbufr_wind.f90 winds.f90 convinfo.f90 \
	convinfo_adpupa.f90
 

OBJS=   main_prepbufr.o read_prepbufr_raw.o read_prepbufr_sfc_ps.o read_prepbufr_sfc_ps_mor.o \
	read_prepbufr_sfc_q.o read_prepbufr_sfc_t.o read_prepbufr_sfc_w.o \
	read_prepbufr_sfc_q_mor.o read_prepbufr_sfc_t_mor.o read_prepbufr_sfc_w_mor.o \
	read_prepbufr_wind.o winds.o convinfo.o convinfo_adpupa.o

.f90.o  :
        @echo
        @echo '---> Compiling $<'
        $(CF) $(FFLAGS) -c $<


# Tunable parameters
#
# CF		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
CF =	        ifort
FC =	        ifort
LDFLAGS =	
#LIBS =          -L/nwprod/lib -lw3nco_4 -lmdl_4
LIBS =          -L/nwprod/lib -lw3nco_4 -lbufr_v10.2.0_d_64
CMD =		prepbufr.x
PROFLIB =	

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
#IBM FFLAGS = -qfixed -qarch=auto -qintlog -O3 -qstrict -qintsize=4 -qrealsize=4
FFLAGS =	-O3 -nofree -auto -fpscomp logicals -fp-model strict -assume byterecl

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(CF) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:	$(OBJS)
	$(CF) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
