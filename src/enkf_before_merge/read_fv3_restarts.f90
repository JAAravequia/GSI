MODULE read_fv3_restarts

USE kinds, ONLY: r_single, i_kind, r_double, r_single
USE netcdf, ONLY: nf90_get_var,nf90_inq_varid
USE netcdf_mod, only: nc_check

IMPLICIT NONE

private
PUBLIC :: read_fv3_restart_data1d, read_fv3_restart_data2d, &
          read_fv3_restart_data3d, read_fv3_restart_data4d

CONTAINS

  SUBROUTINE read_fv3_restart_data1d(varname,filename,file_id,data_arr)
    REAL(r_single), INTENT(inout), DIMENSION(:) :: data_arr
    CHARACTER(len=*), INTENT(in) :: varname
    CHARACTER(len=*), INTENT(in) :: filename
    INTEGER(i_kind), INTENT(in) :: file_id

    CHARACTER(len=24),PARAMETER :: myname_ = 'read_fv3_restart_data1d'
    REAL(r_double), ALLOCATABLE, DIMENSION(:) :: tmp
    INTEGER(i_kind) :: var_id
    
    ALLOCATE(tmp(SIZE(data_arr,1)))
    INCLUDE "read_fv3_restart_data_mzp.f90"
    data_arr=tmp
  END SUBROUTINE read_fv3_restart_data1d

  SUBROUTINE read_fv3_restart_data2d(varname,filename,file_id,data_arr)
    REAL(r_single), INTENT(inout), DIMENSION(:,:) :: data_arr
    CHARACTER(len=*), INTENT(in) :: varname
    CHARACTER(len=*), INTENT(in) :: filename
    INTEGER(i_kind), INTENT(in) :: file_id

    CHARACTER(len=24),PARAMETER :: myname_ = 'read_fv3_restart_data2d'
    REAL(r_single), ALLOCATABLE, DIMENSION(:,:) :: tmp
    INTEGER(i_kind) :: var_id

    ALLOCATE(tmp(SIZE(data_arr,1),SIZE(data_arr,2)))
    INCLUDE "read_fv3_restart_data_mzp.f90"
    data_arr=tmp

  END SUBROUTINE read_fv3_restart_data2d

  SUBROUTINE read_fv3_restart_data3d(varname,filename,file_id,data_arr)
    REAL(r_single), INTENT(inout), DIMENSION(:,:,:) :: data_arr
    CHARACTER(len=*), INTENT(in) :: varname
    CHARACTER(len=*), INTENT(in) :: filename
    INTEGER(i_kind), INTENT(in) :: file_id

    CHARACTER(len=24),PARAMETER :: myname_ = 'read_fv3_restart_data3d'
    REAL(r_double), ALLOCATABLE, DIMENSION(:,:,:) :: tmp
    INTEGER(i_kind) :: var_id
    ALLOCATE(tmp(SIZE(data_arr,1),SIZE(data_arr,2),SIZE(data_arr,3)))
    INCLUDE "read_fv3_restart_data_mzp.f90"
    data_arr=tmp(:,:,UBOUND(tmp,3):LBOUND(tmp,3):-1)
  END SUBROUTINE read_fv3_restart_data3d

  SUBROUTINE read_fv3_restart_data4d(varname,filename,file_id,data_arr)
    REAL(r_single), INTENT(inout), DIMENSION(:,:,:,:) :: data_arr
    CHARACTER(len=*), INTENT(in) :: varname
    CHARACTER(len=*), INTENT(in) :: filename
    INTEGER(i_kind), INTENT(in) :: file_id

    CHARACTER(len=24),PARAMETER :: myname_ = 'read_fv3_restart_data4d'
    REAL(r_double), ALLOCATABLE, DIMENSION(:,:,:,:) :: tmp
    INTEGER(i_kind) :: var_id

    ALLOCATE(tmp(SIZE(data_arr,1),SIZE(data_arr,2),&
         &SIZE(data_arr,3),SIZE(data_arr,4)))
    INCLUDE "read_fv3_restart_data_mzp.f90"

    data_arr=tmp(:,:,UBOUND(tmp,3):LBOUND(tmp,3):-1,:)
  END SUBROUTINE read_fv3_restart_data4d

END MODULE read_fv3_restarts
