subroutine read_locinfo_obs()
   ! READ H and V Localization Radii for Individual Obs-types from text file (obs_locinfo)
   ! Thomas Jones - 2016-09-25

   use kinds, only : r_kind,i_kind,r_single
   use params, only : nlevs,corrlengthnh,corrlengthtr,corrlengthsh,letkf_flag
   use enkf_obsmod, only: obloc, oblnp, corrlengthsq, lnsigl, nobstot, &
                          obpress, obtype, nobs_conv, nobs_oz, oberrvar, ob, stattype
   use kdtree2_module, only: kdtree2, kdtree2_create, kdtree2_destroy, &
                             kdtree2_result, kdtree2_n_nearest
   use constants, only: zero, rearth
   use gridinfo, only: gridloc, logp
   use mpisetup
   logical lexist
   character(len=40)  :: fname = 'obs_locinfo'
   character(len=20), allocatable, dimension(:) :: obname
   character(len=10) temptype
   character(len=3)  temptype2
   real(r_kind) oblnp_indx(1)
   real(r_single), allocatable, dimension(:)   :: hlength,vlength,lnsigl1,corrlengthsq1
   integer(i_kind), allocatable, dimension(:)  :: obtypenum
   real(r_kind) logp_tmp(nlevs)
   type(kdtree2),pointer :: kdtree_grid
   type(kdtree2_result),dimension(:),allocatable :: sresults
   integer(i_kind) k, msig, iunit, n1, n2 ,ideln, nob, ierr, nobtype, maxobstype, io, xx, n
   iunit = 20

   ! READ IN HORIZONTAL AND VERTICAL LOCALIZATION RADII FOR INDIVIDUAL OBS TYPES
   ! First, check the status of input file
   maxobstype = 100
   nobtype = 0

   inquire(file=trim(fname),exist=lexist)

   if ( lexist ) then
      allocate(corrlengthsq1(nobstot),lnsigl1(nobstot))

      open(iunit,file=trim(fname),form='formatted', status="old",action="read" )

      n = 0
       do
        read(iunit,*,end=1)
        n = n+1
       end do
      1 rewind(iunit)

      nobtype = n
      allocate(hlength(nobtype), vlength(nobtype), obname(nobtype), obtypenum(nobtype) )
      do k=1, nobtype
        !read(iunit,*, iostat=io) obname(k), hlength(k),vlength(k)
        read(iunit,FMT='(a3,4x,i3,5x,2f7.2)', iostat=io) obname(k), obtypenum(k), hlength(k),vlength(k)
         
        hlength(k) = hlength(k)        !/0.388
        vlength(k) = abs(vlength(k))   !/0.388
        ! factor of 0.388 to convert from e-folding scale
        ! to distance Gaspari-Cohn function goes to zero.
        if (nproc .eq. 0) then
           print *,'obs=',k,obname(k), obtypenum(k), 'localization scales (horiz,vert)=',hlength(k),vlength(k)
           if (hlength(k) .le. 0.0 ) print *,'*** WARNING...USING ZERO OR NEGATIVE H-LOCALIZATION...FIX!'
           if (vlength(k) .le. 0.0 ) print *,'*** WARNING...USING ZERO OR NEGATIVE V-LOCALIZATION...FIX!'
        endif       

        !if (io /= 0) exit
      end do
      close(iunit)
   else 
     write(6,*) 'READ_LOCINFO_OBS:  ***ERROR*** INPUT FILE MISSING -- ',trim(fname)
     call stop2(124)
   end if 



   ! MPI STUFF
   100 format(I4)
   101 format(F8.1,3x,F6.2)
    kdtree_grid => kdtree2_create(gridloc,sort=.false.,rearrange=.true.)
    allocate(sresults(1))
    if (nobstot > numproc) then
       ideln = int(real(nobstot)/real(numproc))
       n1 = 1 + nproc*ideln
       n2 = (nproc+1)*ideln
       if (nproc == numproc-1) n2 = nobstot
    else
       if(nproc < nobstot)then
         n1 = nproc+1
         n2 = n1
       else
         n1=1
         n2=0
       end if
    end if

    !SET NEW LOCALIZATION VALUES TO DEFAULT
    lnsigl1=zero
    corrlengthsq1=zero

    !LOOP THROUGH OBS TYPES. MATCH OBS-TYPE TO INPUT LOCALIZATION RADII
    do nob=n1,n2
       if (oberrvar(nob) .lt. 1.e20) then
 
         ! OBS TYPE LOOP
         xx = 0
         do k=1, nobtype            

            temptype = obtype(nob)
!            print*, len_trim(obname(k)), len_trim(obtype(nob)), stattype(nob), obtypenum(k), obname(k), obtype(nob)
            if ( len_trim(obtype(nob)) > 3) then
                temptype2 = temptype
                !print*, len_trim(obname(k)),  len_trim(temptype2), temptype2, obname(k), stattype(nob), obtypenum(k)
                !if ( temptype2 == obname(k) ) print*, '**** MATCH', k
            else
                temptype2 = temptype
            end if

!print*, len_trim(obname(k)), len_trim(obtype(nob)), stattype(nob), obtypenum(k), obname(k), obtype(nob), temptype2 

            ! **** NEED TO WORK ON CASES WHERE OBS TYPE HAS SPACES AT START
!            if ( trim(obtype(nob)) == trim(obname(k)) .and. stattype(nob) == obtypenum(k) ) then
            if ( trim(temptype2) == trim(obname(k)) .and. stattype(nob) == obtypenum(k) ) then
                corrlengthsq1(nob) = ( hlength(k) * 1.e3_r_single/rearth)**2
                lnsigl1(nob) = vlength(k)   
                xx = 1
                !print*, 'CHANGING LOCALIZATION', obname(k), trim(obtype(nob)), temptype2,  hlength(k), corrlengthsq1(nob), vlength(k)  
            else
                xx = 0
            end if

            if (xx == 1) exit
            
         end do

           ! USE DEFAULT VALUES IF NO MATCH IS FOUND IN OBS LIST
           if (xx == 0) then
              corrlengthsq1(nob) = corrlengthsq(nob)
              lnsigl1(nob) = lnsigl(nob)
           end if
       else
          ! REVERT TO NAMELIST VALUES FOR BIG OBSVAR (non assim ob)
          corrlengthsq1(nob) = corrlengthsq(nob)
          lnsigl1(nob) = lnsigl(nob)
       end if

       ! SET CWP=0 vertical localization to very large value
       if (trim(obtype(nob)) == 'cwp' .and. ob(nob) == 0.0 .and. obpress(nob) .gt. 450.0 )  lnsigl1(nob) = 5

        !print*, nob,  oberrvar(nob), obtype(nob), stattype(nob), corrlengthsq1(nob), lnsigl1(nob)

    enddo

    if (nproc .eq. 0) close(iunit)
    ! distribute the results to all processors.
    call mpi_allreduce(lnsigl1,lnsigl,nobstot,mpi_real4,mpi_sum,mpi_comm_world,ierr)
    call mpi_allreduce(corrlengthsq1,corrlengthsq,nobstot,mpi_real4,mpi_sum,mpi_comm_world,ierr)
    call kdtree2_destroy(kdtree_grid)

    ! For LETKF, modify values of corrlengthnh,tr,sh for use in observation box
    ! calculation to be equal to maximum value for any level.
    !if (letkf_flag) then
    !  corrlengthnh = corrlengthnh
    !  corrlengthtr = corrlengthnh
    !  corrlengthsh = corrlengthnh
    !endif
    deallocate(sresults,hlength,vlength,corrlengthsq1,lnsigl1)
end subroutine read_locinfo_obs
