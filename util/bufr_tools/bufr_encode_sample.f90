program bufr_encode_sample
!
!  example of writing one value into a bufr file
!
 implicit none
 
 character(80):: obstr='CHWL AOPT'
 character(80):: hdstr='SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SOZA SOLAZI RSST VAOTQ QPLR'
 real(8) :: hdr(13),obs(2,1)
 
 character(8) subset
 integer :: unit_out=10,unit_table=20
 integer :: idate,iret
 
! set data values
 hdr(1)=75.;hdr(2)=30.;hdr(3)=-0.1  
 hdr(1)=224 !SAT ID
 hdr(2)=40; hdr(3)=-83
 hdr(4)=2015; hdr(5)=09; hdr(6)=10; hdr(7)=0; hdr(8)=0
 hdr(9)=40; hdr(10)=0
 hdr(11)=0; hdr(12)=0; hdr(13)=3
 
 obs(2,1)=0.95
 obs(1,1)=0.95
 idate=2015091000  ! YYYYMMDDHH
 subset='NC008043'   ! upper-air reports
 
! encode
 open(unit_table,file='table_prepbufr.txt')
 open(unit_out,file='viirs_aod_singleob.bufr',action='write' &
               ,form='unformatted')
 call datelen(10)
 call openbf(unit_out,'OUT',unit_table)
   call openmb(unit_out,subset,idate)
      call ufbint(unit_out,hdr,13,1,iret,hdstr)
      call ufbint(unit_out,obs,2,1,iret,obstr)
      call writsb(unit_out)
   call closmg(unit_out)
 call closbf(unit_out)

end program
