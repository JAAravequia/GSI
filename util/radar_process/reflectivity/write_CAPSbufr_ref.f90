  subroutine write_bufr_capsref(maxlvl,nlon,nlat,numref,ref3d_column,idate)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    write_bufr_cpasref
!   prgmmr: hu           org: essl/gsd                date: 2008-12-01
!          (originally copied from write_bufr_nsslref and modified.)
!   
! abstract: write column formated MRMS mosaic reflectivity into dbzbufr for
!           CAPS reflectivity DA
!   
! program history log:
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  linux 
!
!$$$
    use constants, only: zero, one
    use kinds, only: r_kind,i_kind
    implicit none

    REAL(r_kind) :: ref3d_column(maxlvl+4,nlon*nlat)   ! 3D reflectivity in column
    real(r_kind) :: hdr(7),obs(1,35)
    character(80):: hdrstr='SID XOB YOB DHR TYP IOB JOB'
    character(80):: obsstr='HREF'

    REAL(i_kind),PARAMETER ::  MXBF = 160000_i_kind
    INTEGER(i_kind) :: ibfmsg = MXBF/4_i_kind

    character(8) subset,sid
    integer(i_kind) :: ludx,lendian_in,idate

    INTEGER(i_kind)  ::  maxlvl,nlon,nlat
    INTEGER(i_kind)  ::  numlvl,numref
    INTEGER(i_kind)  ::  i,n,k,iret


!mhu    idate=2008120100
    subset='ADPUPA'
    sid='NSSLREF'
    ludx=22
    lendian_in=10

    open(ludx,file='prepobs_prep.bufrtable',action='read')
    open(lendian_in,file='dbzbufr_mrmsg2',action='write',form='unformatted')

    call datelen(10)
    call openbf(lendian_in,'OUT',ludx)
    do n=1,numref
      hdr(1)=transfer(sid,hdr(1))
      hdr(2)=ref3d_column(1,n)
      hdr(3)=ref3d_column(2,n)
      hdr(4)=0.
      hdr(5)=500.
      hdr(6)=ref3d_column(3,n)
      hdr(7)=ref3d_column(4,n)

      do k=1,maxlvl
        obs(1,k)=ref3d_column(4+k,n)
      enddo
      call openmb(lendian_in,subset,idate)
      call ufbint(lendian_in,hdr,7,   1,iret,hdrstr)
      call ufbint(lendian_in,obs,1,maxlvl,iret,obsstr)
      call writsb(lendian_in,ibfmsg,iret)
    enddo
    call closbf(lendian_in)
    write(6,*) 'write_bufr_capsref, DONE: write columns:',numref

end subroutine write_bufr_capsref
