program calc_increment_main

  use kinds
  use namelist_def, only : datapath, analysis_filename, firstguess_filename, increment_filename, debug,&
   zero_cwmrinc
  use calc_increment_interface

  implicit none

  character(len=10) :: bufchar

  call getarg(1, analysis_filename)
  call getarg(2, firstguess_filename)
  call getarg(3, increment_filename)
  call getarg(4, bufchar)
  read(bufchar,'(L)') debug
  call getarg(5, bufchar)
  read(bufchar,'(L)') zero_cwmrinc

  !write(6,*) 'DATAPATH        = ', trim(datapath)
  write(6,*) 'ANALYSIS FILENAME   = ', trim(analysis_filename)
  write(6,*) 'FIRSTGUESS FILENAME = ', trim(firstguess_filename)
  write(6,*) 'INCREMENT FILENAME  = ', trim(increment_filename)
  write(6,*) 'DEBUG           = ', debug
  write(6,*) 'ZERO_CWMRINC    = ', zero_cwmrinc

  call calc_increment()

end program calc_increment_main
