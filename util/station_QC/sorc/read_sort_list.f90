!! the program is to read the lists (bias, rejection, and other list) and 
!!  sort out, if the station with 3 or fewer levels is on the list, the station 
!!  will remove from the list

     
          
     character(len=8),dimension(300) :: stdid
     real(4), dimension(300,2) :: cid
     real(4),dimension(300,43,2) :: std,std_no,rmean,break
     real(4),dimension(300,43,2) :: xmed,xb,sb,smad
     real(4),dimension(300,43,2) :: tdata_no2
     integer,dimension(300) :: nrank

      character*50 file,outfile  
      character*5 dtype
      character*8 station_id 

      real*4 rlat,rlon,rno,meanb,meana,stdb,stda,all_no1,all_no2
      real*4 meda,medb,xba,xbb,sba,sbb,smada,smadb,break1,break2,break3,break4 

       namelist/input/dtype,file
           nread=0

            open(11,file=file,form='formatted')

          loopd: do
              read(11,100,IOSTAT=iflag) station_id,n,rlon,rlat,rno,meanb,meana,stdb,stda,all_no1,all_no2  
               if( iflag /= 0 ) exit loopd
               nread=nread+1
               read(11,110) k,medb,meda,xbb,xba,sbb,sba,smadb,smadb
               read(11,120) break1,break2,break3
                if(nread ==1) then
                     stdid(nread) = station_id
                     cid(nread,1)=rlon
                     cid(nread,2)=rlat
                     std_no(nread,k)=rno
                     rmean(nread,k,1)=meanb
                     rmean(nread,k,2)=meana
                     std(nread,k,1)=stdb
                     std(nread,k,2)=stda
                     tdata_no2(nread,k,1)=all_no1 
                     tdata_no2(nread,k,2)=all_no2 
                     xmed(nread,k,1)=medb
                     xmed(nread,k,2)=meda
                     xb(nread,k,1)=xbb
                     xb(nread,k,2)=xba
                     sb(nread,k,1)=sbb
                     sb(nread,k,2)=sba
                     
                     

                         
                 
                 
