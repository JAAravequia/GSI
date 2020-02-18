MODULE write_fv3_restarts

  USE kinds, ONLY: r_single, i_kind, r_double, r_single
  USE netcdf, ONLY: nf90_put_var,nf90_inq_varid,nf90_inquire_variable
  USE netcdf_mod, ONLY: nc_check

  INCLUDE 'netcdf.inc'

  INTEGER :: xtype

  PUBLIC write_fv3_restart_data1d,write_fv3_restart_data2d
  PUBLIC write_fv3_restart_data3d,write_fv3_restart_data4d
  
CONTAINS

  SUBROUTINE write_fv3_restart_data1d(varname,filename,file_id,data_arr)
    REAL(r_single), INTENT(inout), DIMENSION(:) :: data_arr
    CHARACTER(len=*), INTENT(in) :: varname
    CHARACTER(len=*), INTENT(in) :: filename
    INTEGER(i_kind), INTENT(in) :: file_id

    CHARACTER(len=24),PARAMETER :: myname_ = 'write_fv3_restart_data1d'
    REAL(r_double), ALLOCATABLE, DIMENSION(:) :: tmp
    INTEGER(i_kind) :: var_id

    ALLOCATE(tmp(SIZE(data_arr,1)))
    tmp=data_arr
    INCLUDE "write_fv3_restart_data_mzp.f90"

  END SUBROUTINE write_fv3_restart_data1d

  SUBROUTINE write_fv3_restart_data2d(varname,filename,file_id,data_arr)
    REAL(r_single), INTENT(inout), DIMENSION(:,:) :: data_arr
    CHARACTER(len=*), INTENT(in) :: varname
    CHARACTER(len=*), INTENT(in) :: filename
    INTEGER(i_kind), INTENT(in) :: file_id

    CHARACTER(len=24),PARAMETER :: myname_ = 'write_fv3_restart_data2d'
    REAL(r_double), ALLOCATABLE, DIMENSION(:,:) :: tmp
    INTEGER(i_kind) :: var_id

    ALLOCATE(tmp(SIZE(data_arr,1),SIZE(data_arr,2)))
    tmp=data_arr
    INCLUDE "write_fv3_restart_data_mzp.f90"

  END SUBROUTINE write_fv3_restart_data2d

  SUBROUTINE write_fv3_restart_data3d(varname,filename,file_id,data_arr)
    REAL(r_single), INTENT(inout), DIMENSION(:,:,:) :: data_arr
    CHARACTER(len=*), INTENT(in) :: varname
    CHARACTER(len=*), INTENT(in) :: filename
    INTEGER(i_kind), INTENT(in) :: file_id

    CHARACTER(len=24),PARAMETER :: myname_ = 'write_fv3_restart_data3d'
    REAL(r_double), ALLOCATABLE, DIMENSION(:,:,:) :: tmp
    INTEGER(i_kind) :: var_id
    ALLOCATE(tmp(SIZE(data_arr,1),SIZE(data_arr,2),SIZE(data_arr,3)))
    tmp=data_arr(:,:,UBOUND(tmp,3):LBOUND(tmp,3):-1)
    INCLUDE "write_fv3_restart_data_mzp.f90"

  END SUBROUTINE write_fv3_restart_data3d

  SUBROUTINE write_fv3_restart_data4d(varname,filename,file_id,data_arr)
    REAL(r_single), INTENT(inout), DIMENSION(:,:,:,:) :: data_arr
    CHARACTER(len=*), INTENT(in) :: varname
    CHARACTER(len=*), INTENT(in) :: filename
    INTEGER(i_kind), INTENT(in) :: file_id

    CHARACTER(len=24),PARAMETER :: myname_ = 'write_fv3_restart_data4d'
    REAL(r_double), ALLOCATABLE, DIMENSION(:,:,:,:) :: tmp
    INTEGER(i_kind) :: var_id
    ALLOCATE(tmp(SIZE(data_arr,1),SIZE(data_arr,2),&
         &SIZE(data_arr,3),SIZE(data_arr,4)))

    tmp=data_arr(:,:,UBOUND(tmp,3):LBOUND(tmp,3):-1,:)
    INCLUDE "write_fv3_restart_data_mzp.f90"

  END SUBROUTINE write_fv3_restart_data4d

END MODULE  write_fv3_restarts
