program prepbufr_decode_all
!
! read all observations out from prepbufr. 
! read bufr table from prepbufr file
!
 implicit none

 integer, parameter :: mxmn=35, mxlv=250
! character(80):: hdstr='SID CLAT CLON DHR TYP ELV SAID SAZA FOVN'
 character(80):: hdstr='SAID SIID YEAR MNTH DAYS HOUR MINU SECW RCVCH LL2BCT' !for ECMWF ADM BUFR
! character(80):: obstr='CLATH CLONH HEITH ELEV HLSW' ! for ECMWF ADM BUFR
! character(80):: obstr='HLSW HLSWEE CONFLG PRES TMDBST BKSTR DWPRS DWTMP DWBR' ! for ECMWF ADM BUFR
 character(80):: obstr='HLSW HLSWEE CONFLG CLATH CLONH HEITH ELEV BEARAZ' ! for ECMWF ADM BUFR
 !real(8) :: hdr(10),obs(5,25)
 real(8) :: hdr(10),obs(9)
! character(80):: hdstr='SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON CLATH CLONH HOLS' ! for tempest-d bufr
! character(80):: obstr='SAZA SOZA BEARAZ SOLAZI SANG' ! for tempest-d bufr
! real(8) :: hdr(13),obs(5),tb(5)
! character(80):: obstr='POB QOB TOB ZOB UOB VOB PWO CAT PRSS'
! character(80):: qcstr='PQM QQM TQM ZQM WQM NUL PWQ     '
! character(80):: oestr='POE QOE TOE NUL WOE NUL PWE     '
! real(8) :: obs(mxmn,mxlv),qcf(mxmn,mxlv),oer(mxmn,mxlv)

 INTEGER        :: ireadmg,ireadsb

 character(8)   :: subset
 integer        :: unit_in=10,idate,nmsg,ntb

 character(8)   :: c_sid
 real(8)        :: rstation_id
 equivalence(rstation_id,c_sid)

 integer        :: i,j,k,iret
!
!
! open(24,file='mhsbufr.table')
! open(24,file='tempestbufr.table')
! open(24,file='table_aeolusbufr.txt')
 open(unit_in,file='prepbufr',form='unformatted',status='old')
! open(unit_in,file='prepbufr_out',action='read',form='unformatted')
 call openbf(unit_in,'IN',unit_in)
! call dxdump(unit_in,24)
 call datelen(10)
   nmsg=0
   msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
     nmsg=nmsg+1
     ntb = 0
!     write(*,*)
     write(*,'(3a,i14)') 'subset=',subset,' cycle time =',idate
     sb_report: do while (ireadsb(unit_in) == 0)
       ntb = ntb+1
       call ufbint(unit_in,hdr,  19,   1,iret,hdstr)
       call ufbint(unit_in,obs,  9,   1,iret,obstr)
!       call ufbint(unit_in,hdr,  13,   1,iret,hdstr)
!       call ufbint(unit_in,obs,   5,   1,iret,obstr)
!       call ufbrep(unit_in, tb,   1,   5,iret,'TMBR')
!       call ufbint(unit_in,oer,mxmn,mxlv,iret,oestr)
!       call ufbint(unit_in,qcf,mxmn,mxlv,iret,qcstr)
       rstation_id=hdr(1)
!       if(int(hdr(5))==120 .or. int(hdr(5))==220) then
          write(*,*)
          write(*,'(2I10,a15,8f15.1)') ntb,iret,c_sid,(hdr(i),i=2,8)
          DO k=1,iret
            write(*,'(i3,a10,9f15.1)') k,'obs=',(obs(i,k),i=1,9)
            write(*,'(i3,a10,9f15.1)') k,'oer=',(oer(i,k),i=1,7)
            write(*,'(i3,a10,9f15.1)') k,'qcf=',(qcf(i,k),i=1,7)
          ENDDO
!       endif
     enddo sb_report
   enddo msg_report
 call closbf(unit_in)

end program
