program efsoi_main
!$$$  main program documentation block
!
! program:  efsoi_main                  high level driver program for 
!                                       efsoi calculations.
!
! prgmmr: Ota         org: EMC/JMA               date: 2012
! prgmmr: Groff       org: EMC                   date: 2018
!
! abstract: This is the main program for EFSOI code. It does the following:
!           a) initialize MPI, read EFSOI namelist from efsoi.nml on each task.
!           b) reads observation sensitivity files.
!           c) read horizontal grid information (lat/lon of each grid point) and
!              pressure at each grid point/vertical level.
!           d) decomposition of horizontal grid points and observation
!              priors to minimize load imbalance for EFSOI calcs.
!           e) read forecast and analysis states necessary for
!              EFSOI calculations.
!           f) Initialize/allocate for EFSOI moist/dry/kinetic
!              total output
!           g) Estimate the location of observation response
!              (i.e. advect the localization).  Default estimation
!              approach is to multiply meridional and zonal wind
!              by 0.75.
!           h) Perform EFSOI calculations for all observations
!              considered for assimilation during the EnSRF update.
!           i) write EFSOI calculations and ancillary EFSOI
!              information to file
!           j) deallocate all allocatable arrays, finalize MPI.
!
! program history log:
!   2018-04-30  Initial development. Adaptation of enkf_main program
!               towards EFSOI calculation process
!
! usage:
!   input files: Update to FV3 nomenclature
!     sigfAT_YYYYMMDDHH_mem* - Ensemble of forecasts valid at advance time
!                              (AT) hours beyond the initial time
!     sigfAT_YYYYMMDDHH_ensmean - Ensemble mean forecast AT hours from
!                                 initial time
!     sigfAT+6_YYYYMMDDHH-6_ensmean - Ensemble mean forecast AT+6 hours
!                                     from initial time minus 6 hours
!     siganl.YYYYMMDDHH.gdas - 
!   output files: 
!     obimpact_YYYYMMDDHH.dat - observation impact file
!                         
! comments: This program is a wrapper for components needed to peform
!           EFSOI calculations
!
! attributes:
!   language: f95
!
!$$$

 use kinds, only: r_double,i_kind
 ! reads namelist parameters.
 ! applying enkf namelist apparatus
 use params, only : read_namelist,nanals
 ! mpi functions and variables.
 use mpisetup, only:  mpi_initialize, mpi_initialize_io, mpi_cleanup, nproc, &
                      mpi_wtime, mpi_comm_world
 ! model state vector 
 use statevec_efsoi, only: read_state_efsoi, statevec_cleanup_efsoi, init_statevec_efsoi
 ! load balancing
 use loadbal, only: load_balance, loadbal_cleanup
 ! efsoi update
 use efsoi, only: efsoi_update
 ! Observation sensitivity usage
 use enkf_obs_sensitivity, only: init_ob_sens, print_ob_sens, destroy_ob_sens, read_ob_sens
 use loc_advection, only: loc_advection_efsoi
 ! Scatter chunks for EFSOI
 use scatter_chunks_efsoi, only: scatter_chunks_ob_impact
 
 implicit none
 integer :: ierr
 real(r_double) t1,t2

 ! initialize MPI.
 call mpi_initialize()
 if (nproc==0) call w3tagb('EFSOI_CALC',2018,0319,0055,'NP25')

 ! read namelist.
 call read_namelist()

 ! initialize MPI communicator for IO tasks.
 call mpi_initialize_io(nanals)

 ! read the necessary inputs for
 ! the EFSOI calculation from file
 t1 = mpi_wtime()
 call read_ob_sens()
 t2 = mpi_wtime()
 if (nproc == 0) print *, 'time in read_ob_sens = ',t2-t1,'on proc', nproc

 ! Halt processors until all are completed
 call mpi_barrier(mpi_comm_world, ierr)

 ! Initialize state vector 
 ! information
 call init_statevec_efsoi()

 ! read in ensemble forecast members,
 ! valid at the evaluation forecast
 ! time, distribute pieces to each task.
 t1 = mpi_wtime()
 call read_state_efsoi()
 t2 = mpi_wtime()
 if (nproc == 0) print *,'time in read_stae_efsoi =',t2-t1,'on proc',nproc

 ! do load balancing (partitioning of grid points
 ! and observations among processors)
 t1 = mpi_wtime()
 call load_balance()
 t2 = mpi_wtime()
 if (nproc == 0) print *,'time in load_balance =',t2-t1,'on proc',nproc

 ! apply scattering of efsoi chunks
 t1 = mpi_wtime()
 call scatter_chunks_ob_impact()  ! ensemble scattering
 t2 = mpi_wtime()
 if (nproc == 0) print *,'time to scatter observation impact chunks =',t2-t1,'on proc',nproc

 ! Initialize EFSOI variables
 t1 = mpi_wtime()
 call init_ob_sens()
 t2 = mpi_wtime() 
 if (nproc == 0) print *,'time to allocate ob sensitivity variables =',t2-t1,'on proc',nproc

 ! Calculate the estimated location of observation
 ! response at evaluation time
 t1 = mpi_wtime()
 call loc_advection_efsoi()
 t2 = mpi_wtime()
 if (nproc == 0) print *,'time in loc_advection_efsoi =',t2-t1,'on proc',nproc

 ! Perform the EFSOI calcs
 t1 = mpi_wtime()
 call efsoi_update()
 t2 = mpi_wtime()
 if (nproc == 0) print *,'time in efsoi_update =',t2-t1,'on proc',nproc

 ! print EFSOI sensitivity i/o on root task.
 t1 = mpi_wtime()
 call print_ob_sens()
 t2 = mpi_wtime()
 if (nproc == 0) print *,'time needed to write observation impact file =',t2-t1,'on proc',nproc

 ! Cleanup for EFSOI configuration
 call statevec_cleanup_efsoi()
 call loadbal_cleanup()
 call destroy_ob_sens()

 ! finalize MPI.
 if (nproc==0) call w3tage('EFSOI_CALC')
 call mpi_cleanup()

end program efsoi_main
