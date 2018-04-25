program efso_main
!$$$  main program documentation block
!
! program:  efso_main                  high level driver program for 
!                                      efso calculations.
!
! prgmmr: Ota         org: EMC/JMA               date: 2012
! prgmmr: Groff       org: EMC                   date: 2018        
!
! abstract: This is the main program for the EFSO code. It does the following:
!           a) initialize MPI, read namelist from enkf.nml on each task.
!           b) reads observation sensitivity files (from diag file
!              output by GSI forward operator code). Print innovation
!              statistics for prior.
!           c) read horizontal grid information (lat/lon of each grid point) and
!              pressure at each grid point/vertical level. 
!           d) decomposition of horizontal grid points and observation
!              priors to minimize load imbalance for efso calcs.
!           e) read forecast and analysis states necessary for
!              EFSOI calculations.
!           f) Perform EFSOI calculations for all observations
!              assimilated during the EnSRF update.
!           h) write out the analysis ensemble and updated radiance bias
!              correction coefficients. Print innovation statistics for
!              posterior.
!           i) deallocate all allocatable arrays, finalize MPI.
!
! program history log:
!   2009-02-23  Initial version.
!   2011-06-03  Added the option for LETKF.
!   2016-02-01  Initialize mpi communicator for IO tasks (1st nanals tasks).
!
! usage:
!   input files:
!     sfg_YYYYMMDDHH_fhr06_mem* - first guess ensemble members, plus 
!                     ensemble mean (sfg_YYYYMMDDHH_fhr06_ensmean).
!     satbias_angle - satellite angle dependent file
!     satbias_in    - satellite bias correction coefficient file
!     satinfo       - satellite channel info file
!     convinfo      - convential data (prepufr) info file
!     ozinfo        - ozone retrieval info file
!     diag_YYYYMMDDHH_ges_mem*  - observation diagnostic files for each ensemble member
!                     created GSI forward operator.
!     hybens_info   - if parameter readin_localization is true, contains 
!                      vertical profile of horizontal and vertical localization
!                      length scales (along with static and ensemble weights
!                      used in hybrid).
!
!   output files: 
!     sanl_YYYYMMDDHH_mem* - analysis ensemble members. A separate program
!                            may be run to add system noise to these files.
!     covinflate.dat - multiplicative inflation (inflate_ens in module inflation).
!     satbias_out    - output satellite bias correction file.
!                         
! comments:
!
! This program is run after the forward operator code is run on each ensemble
! member to create the diag*mem* input files.
!
! attributes:
!   language: f95
!
!$$$

 use kinds, only: r_kind,r_double,i_kind
 ! reads namelist parameters.
 use params, only : read_namelist,letkf_flag,readin_localization,lupd_satbiasc,&
                    numiter, nanals
 ! mpi functions and variables.
 use mpisetup, only:  mpi_initialize, mpi_initialize_io, mpi_cleanup, nproc, &
                      numproc, mpi_wtime
 ! obs and ob priors, associated metadata.
 use enkf_obsmod, only : readobs, obfit_prior, obsprd_prior, &
                    deltapredx, nobs_sat, obfit_post, obsprd_post, &
                    obsmod_cleanup, biasprednorminv
 ! innovation statistics.
 use innovstats, only: print_innovstats
 ! grid information
 use gridinfo, only: getgridinfo, gridinfo_cleanup, npts,lonsgrd,latsgrd
 ! model state vector 
 use statevec, only: read_ensemble, write_ensemble, statevec_cleanup
 ! load balancing
 use loadbal, only: load_balance, loadbal_cleanup
 ! enkf update
 use enkf, only: enkf_update
 ! letkf update
 use letkf, only: letkf_update
 ! radiance bias correction coefficients.
 use radinfo, only: radinfo_write, predx, jpch_rad, npred
 ! posterior ensemble inflation.
 use inflation, only: inflate_ens
 ! initialize radinfo variables
 use radinfo, only: init_rad, init_rad_vars
 use omp_lib, only: omp_get_max_threads
 ! Observation sensitivity usage
 use enkf_obs_sensitivity, only: init_ob_sens, print_ob_sens, destroy_ob_sens

 implicit none
 integer(i_kind) j,n,nth
 real(r_double) t1,t2
 logical no_inflate_flag

 ! initialize MPI.
 call mpi_initialize()
 if (nproc==0) call w3tagb('EFSO_ANL',2011,0319,0055,'NP25')

 ! read namelist.
 call read_efso_namelist()

 ! initialize MPI communicator for IO tasks.
 call mpi_initialize_io(nanals)

 ! read horizontal grid information and pressure fields from
 ! 6-h forecast ensemble mean file.
 call getgridinfo()

 ! Initialize efso arrays
 if(fso_cycling) call init_ob_sens()

 ! do load balancing (partitioning of grid points, observations among
 ! processors)
 t1 = mpi_wtime()
 call load_balance()  ! EFSO mods necessary?
 t2 = mpi_wtime()
 if (nproc == 0) print *,'time in load_balance =',t2-t1,'on proc',nproc

! necessary
 ! read in ensemble members, distribute pieces to each task.
 t1 = mpi_wtime()
 call read_ensemble() ! Definite EFSO mods necessary
 t2 = mpi_wtime()
 if (nproc == 0) print *,'time in read_ensemble =',t2-t1,'on proc',nproc

! not necessary
 t1 = mpi_wtime()
 call fso_update()
 t2 = mpi_wtime()
 if (nproc == 0) print *,'time in fso_update =',t2-t1,'on proc',nproc

 ! print EFSO sensitivity i/o on root task.
 if(fso_cycling) call print_ob_sens()

 ! Cleanup for EFSO configuration
 call gridinfo_cleanup()  ! Probably nothing here
 call statevec_cleanup()  ! Probably nothing here
 call loadbal_cleanup()   ! Probably nothing here
 if(fso_cycling) call destroy_ob_sens()

 ! write log file (which script can check to verify completion).
 if (nproc .eq. 0) then
    call write_logfile()
 endif

 ! finalize MPI.
 if (nproc==0) call w3tage('ENKF_ANL')
 call mpi_cleanup()

end program efso_main
