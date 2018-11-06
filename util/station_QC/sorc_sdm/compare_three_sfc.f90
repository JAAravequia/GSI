! the subroutine is to compare two station list to see wether there are
! same stations from both different observation types

  subroutine compare_three_sfc(type,fileout,stdidall,ncount_all,fileexist,itype,ndata)


   character(5) atype,atype1,atype2,atype3
   character(20) fileout
   character(8),dimension(17000) :: stdid
   character(5),dimension(17000) :: atype_fnl 
   character(8),dimension(4,ndata) :: stdidall
   integer,dimension(4) :: ncount_all,itype
   integer type

   logical,dimension(4) :: fileexist


   open(21,file=fileout,form='formatted')
   write(21,100)

          nexist=0
          atype='-----'
          atype1='-----'
          atype2='-----'
          atype3='-----'
          atype_fnl='-----'
          nobs=0
          nnobs=0
          do i=1,4
             if(fileexist(i) .and. itype (i) ==0) then
                nexist=nexist+1
                if(nexist ==1) then
                   ii=i
                   if (ii ==1) then
                      atype1(i:i)='P'
                   else
                      atype1(i+1:i+1)='P'
                   endif
                 print *,'atype1=',atype1
                else if (nexist ==2) then
                   jj=i
                   if (jj ==1) then
                      atype2(i:i)='P'
                    else
                       atype2(i+1:i+1)='P'
                    endif
                 print *,'atype2=',atype2
                else if (nexist ==3) then
                   kk=i
                   if (kk ==1) then
                      atype3(i:i)='P'
                    else
                       atype3(i+1:i+1)='P'
                    endif
                 print *,'atype3=',atype3
                endif
             endif
          enddo
          nobs=ncount_all(jj)
          nnobs=nobs
          print *,'nobs=',nobs,ncount_all(ii)
          stdid(1:nobs)=stdidall(jj,1:nobs)
          atype_fnl(1:nobs)=atype2
          print *,'atype_fnl(10)=',atype_fnl(10)
          do i=1,ncount_all(ii)
             do j=1,ncount_all(jj)
                if (trim(stdidall(ii,i)) == trim(stdidall(jj,i))) then 
                   atype=atype_fnl(j)
                   print *,'atype=',atype
                   if( ii ==1) then
                      atype(ii:ii) =atype1(ii:ii)
                   else
                      atype(ii+1:ii+1) =atype1(ii+1:ii+1)
                   endif
                   atype_fnl(j)=atype
                   write(6,200) atype,atype_fnl(j),stdidall(ii,i),i,j
                   exit
                else if( j ==ncount_all(jj)) then
                    nnobs=nnobs+1
                    stdid(nnobs)=stdidall(ii,i)
                    atype_fnl(nnobs)=atype1
                endif
             enddo
          enddo
          print *,'nnobs=',nnobs,ncount_all(kk)
          nobs=nnobs
          do i=1,ncount_all(kk)
          do j=1,nnobs
             if(trim(stdidall(kk,i)) == trim(stdid(j))) then
                atype=atype_fnl(j)
                atype(kk+1:kk+1)=atype3(kk+1:kk+1)
                atype_fnl(j)=atype
                   write(6,200)atype,atype_fnl(j),stdidall(kk,i),i,j
                exit
             else if( j ==nnobs) then
                    nobs=nobs+1
                    stdid(nobs)=stdidall(kk,i)
                    atype_fnl(nobs)=atype3
             endif
          enddo 
          enddo 


          do i=1,nnobs
             if( type ==181 .or. type == 187 .or. type ==281 .or. type ==287) then
                write(21,110) stdid(i),atype_fnl(i)
             else if( type ==180 .or. type ==280 ) then
                write(21,120) stdid(i),atype_fnl(i)
             else if( type ==188 .or. type ==288 ) then
                write(21,130) stdid(i),atype_fnl(i)
             endif
          enddo

100 format('STATION',3x,'N_LAT',2x,'W_LON',2x,'YYYYMMDDHH',2x,&
           'TYP  PZTQW  LTYP  PRESSURE(S)  MSG_TYPE  WMO_BULLHDR  LAT_LON_BDRY  ITP  NCEP-ANL-NETWORK(S)')
110 format(a8,2x,'-----  -----  ----------',2x,'SFC',2x,a5,2x,&
           '----  -----------  --------  -----------  ------------',&
           2x,'---  -------------------')   
120 format(a8,2x,'-----  -----  ----------',2x,'SHP',2x,a5,2x,&
           '----  -----------  --------  -----------  ------------',&
           2x,'---  -------------------')   
130 format(a8,2x,'-----  -----  ----------',2x,'MSO',2x,a5,2x,&
           '----  -----------  --------  -----------  ------------',&
           2x,'---  -------------------')   


 200 format('atype,atype_fnl(j),stdidall(ii,i),ii,jj=',a5,2x,a5,2x,a8,2x,2i3) 

     return
     end


   
