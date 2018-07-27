!This module contains routines used in cov_calc.f90
!Kristen Bathmann
!5-2015

module obs_tools

use kinds, only: r_kind,i_kind

implicit none
public:: cdist
public:: get_filename

contains

subroutine cdist(po1,po2,dist)
!This function takes two points, whose positions are specified by a latitude and
!longitude, and computes the distance, in km, between the two by converting to
!cartesian coordinates
use constants, only: rad, pi
implicit none
real(r_kind),dimension(2),intent(in):: po1,po2       !the two points, given by (lat,lon)
real(r_kind), intent(out):: dist
real(r_kind):: x1,y1,z1, x2, y2, z2                   !cartesian coordinates
real(r_kind)::d1
real(r_kind):: sinphi1, cosphi1, costhe1, sinthe1    !trig functions related to po1
real(r_kind):: sinphi2, cosphi2, costhe2,sinthe2     !trig functions related to po2
real(r_kind),dimension(2):: p1, p2                   !manipulations on po1 and po2


p1(1)=(90.0_r_kind-po1(1))*pi/180.0_r_kind !phi
p2(1)=(90.0_r_kind-po2(1))*pi/180.0_r_kind !phi
p1(2)=po1(2)*pi/180.0_r_kind !theta
p2(2)=po2(2)*pi/180.0_r_kind !theta

sinphi1=sin(p1(1))
sinphi2=sin(p2(1))
cosphi1=cos(p1(1))
cosphi2=cos(p2(1))
costhe1=cos(p1(2))
costhe2=cos(p2(2))
sinthe1=sin(p1(2))
sinthe2=sin(p2(2))

x1=sinphi1*costhe1
x2=sinphi2*costhe2
y1=sinphi1*sinthe1
y2=sinphi2*sinthe2
z1=cosphi1
z2=cosphi2

d1=(x1-x2)**2+(y1-y2)**2+(z1-z2)**2
dist=rad*sqrt(d1)
end subroutine cdist

!KAB
subroutine cld_params(no_chn,ccld,cclr)
implicit none
integer(i_kind), intent(in):: no_chn
real(r_kind), dimension(:), intent(inout):: ccld, cclr
integer(i_kind):: i

ccld=0.0_r_kind
cclr=0.0_r_kind
if (no_chn==15) then !amsua
  cclr(1)=0.05_r_kind
  cclr(2)=0.03_r_kind
  cclr(3)=0.03_r_kind
  cclr(4)=0.02_r_kind
  cclr(6)=0.1_r_kind
  cclr(15)=0.03_r_kind

  ccld(1)=0.6_r_kind
  ccld(2)=0.45_r_kind
  ccld(3)=0.4_r_kind
  ccld(4)=0.45_r_kind
  ccld(5)=1.0_r_kind
  ccld(6)=1.5_r_kind
  ccld(15)=0.2_r_kind

elseif (no_chn==22) then !atms
  cclr(1)=0.03_r_kind
  cclr(2)=0.03_r_kind
  cclr(3)=0.03_r_kind
  cclr(4)=0.02_r_kind
  cclr(5)=0.03_r_kind
  cclr(6)=0.08_r_kind
  cclr(7)=0.15_r_kind
  cclr(16)=0.02_r_kind
  cclr(17)=0.03_r_kind
  cclr(18)=0.03_r_kind
  cclr(19)=0.03_r_kind
  cclr(20)=0.03_r_kind
  cclr(21)=0.05_r_kind
  cclr(22)=0.1_r_kind

  ccld(1)=0.35_r_kind
  ccld(2)=0.38_r_kind
  ccld(3)=0.4_r_kind
  ccld(4)=0.45_r_kind
  ccld(5)=0.5_r_kind
  ccld(6)=1.0_r_kind
  ccld(7)=1.0_r_kind
  ccld(16)=0.35_r_kind
  do i=17,22
    ccld(i)=0.5_r_kind
  end do
ccld=ccld/5
end if

end subroutine cld_params
subroutine get_filename(T,ext,filename)
!At a given time step T, this subroutine outputs the name of the file to be read
!in.  The file will either be an analysis diag file or a ges diag file
!(specified by ext)
implicit none
integer,intent(in):: T                  !Time step of diag file to be read in
character(5),intent(in)::ext            !specifies either anl or ges diag file
character(9),intent(out):: filename 
real(r_kind):: tem
integer(i_kind):: t1i,t2i,t3i, t4i
character(1)::t1,t2,t3, t4

tem=T/1000
t1i=floor(tem)
tem=(T-1000*t1i)/100
t2i=floor(tem)
tem=(T-1000*t1i-100*t2i)/10
t3i=floor(tem)
t4i=T-1000*t1i-100*t2i-10*t3i
t1=ACHAR(t1i+48)
t2=ACHAR(t2i+48)
t3=ACHAR(t3i+48)
t4=ACHAR(t4i+48)
filename(1:5)=ext
filename(6:6)=t1
filename(7:7)=t2
filename(8:8)=t3
filename(9:9)=t4
end subroutine get_filename

end module obs_tools

