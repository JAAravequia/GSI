!********************************************************
!* program: create_markers                              *
!* This program reads a control file with station info  *
!* and write out a list of google map markers           *
!* with the station name, lat/lon info                  *
!* Yangrong Ling, 06/2011                               *
!********************************************************

program create_markes_multilevel
 use strings
 implicit none

 integer, parameter :: MAX_CHAR_LENGTH=400 !maximum string length
 integer, parameter :: MAX_NUM_TOKENS=20 !maximum number of tokens in a line
 character(len=80) :: input_ctl_file, output_5mb, output_30mb, output_125mb, &
                       output_225mb, output_325mb, output_425mb, output_525mb, & 
                       output_625mb, output_725mb, output_825mb, output_925mb

 character(len=40) :: png_spd_obs_5mb, png_spd_omg_5mb, png_wdr_obs_5mb, png_wdr_omg_5mb, &
                      png_spd_obs_30mb, png_spd_omg_30mb, png_wdr_obs_30mb, png_wdr_omg_30mb, &
                      png_spd_obs_125mb, png_spd_omg_125mb, png_wdr_obs_125mb, png_wdr_omg_125mb, &
                      png_spd_obs_225mb, png_spd_omg_225mb, png_wdr_obs_225mb, png_wdr_omg_225mb, &
                      png_spd_obs_325mb, png_spd_omg_325mb, png_wdr_obs_325mb, png_wdr_omg_325mb, &
                      png_spd_obs_425mb, png_spd_omg_425mb, png_wdr_obs_425mb, png_wdr_omg_425mb, &
                      png_spd_obs_525mb, png_spd_omg_525mb, png_wdr_obs_525mb, png_wdr_omg_525mb, &
                      png_spd_obs_625mb, png_spd_omg_625mb, png_wdr_obs_625mb, png_wdr_omg_625mb, &
                      png_spd_obs_725mb, png_spd_omg_725mb, png_wdr_obs_725mb, png_wdr_omg_725mb, &
                      png_spd_obs_825mb, png_spd_omg_825mb, png_wdr_obs_825mb, png_wdr_omg_825mb, &
                      png_spd_obs_925mb, png_spd_omg_925mb, png_wdr_obs_925mb, png_wdr_omg_925mb

 character(len=MAX_CHAR_LENGTH) :: line_buffer
 character(len=MAX_CHAR_LENGTH) :: tokens(MAX_NUM_TOKENS)
 character(len=8) :: station_id
 character(len=5) :: type
 real :: lat_temp, lon_temp, lat, lon
 character(len=5) :: lat_temp_c
 character(len=6) :: lon_temp_c
 character(len=4) :: qflag !quality flag for normal, bias, reject, and qreject

 integer :: num_stations, num_tokens, ios, argc, i

 argc=command_argument_count()
 if(argc<1) then
   print *, "Usage: create_markers_multilevel <input_ctl_file>"
   call exit(1)
 end if

 call get_command_argument(1, input_ctl_file)

 !open the input sonde dictionary file
 open (unit=21, file=trim(input_ctl_file), status="old", iostat=ios)

 call parse (input_ctl_file, '_', tokens, num_tokens)
 type=tokens(1)

 output_5mb = trim(type)//"_5mb.php"
 output_30mb = trim(type)//"_30mb.php"
 output_125mb = trim(type)//"_125mb.php"
 output_225mb = trim(type)//"_225mb.php"
 output_325mb = trim(type)//"_325mb.php"
 output_425mb = trim(type)//"_425mb.php"
 output_525mb = trim(type)//"_525mb.php"
 output_625mb = trim(type)//"_625mb.php"
 output_725mb = trim(type)//"_725mb.php"
 output_825mb = trim(type)//"_825mb.php"
 output_925mb = trim(type)//"_925mb.php"

 !open the output php files
 open (unit=51, file=trim(output_5mb), status="new", iostat=ios)
 open (unit=52, file=trim(output_30mb), status="new", iostat=ios)
 open (unit=53, file=trim(output_125mb), status="new", iostat=ios)
 open (unit=54, file=trim(output_225mb), status="new", iostat=ios)
 open (unit=55, file=trim(output_325mb), status="new", iostat=ios)
 open (unit=56, file=trim(output_425mb), status="new", iostat=ios)
 open (unit=57, file=trim(output_525mb), status="new", iostat=ios)
 open (unit=58, file=trim(output_625mb), status="new", iostat=ios)
 open (unit=59, file=trim(output_725mb), status="new", iostat=ios)
 open (unit=60, file=trim(output_825mb), status="new", iostat=ios)
 open (unit=61, file=trim(output_925mb), status="new", iostat=ios)


 num_stations = 0

 91   Format("  + '", '<a href="../', a36, '" target="_blank"> WSD_OBS</a> <br>',"'")
 92   Format("  + '", '<a href="../', a36, '" target="_blank"> WSD_OMG</a> <br>',"'")
 93   Format("  + '", '<a href="../', a36, '" target="_blank"> WDR_OBS</a> <br>',"'")
 94   Format("  + '", '<a href="../', a36, '" target="_blank"> WDR_OMG</a> <br>',"'")

 95   Format("[", f7.2, ",", f8.2, " ,", "'", a4, "'", ",  '<rfont>", a8, "</rfont> <br>'")
! 95   Format("[", f7.2, ",", f8.2, ",  '<rfont>", a8, "</rfont> <br>'")
! 96   Format('  +', "'", '<a href="../pngs/', a25, '" target="_blank" >', "WSD</a> <br>'")  
! 97   Format('  +', "'", '<a href="../pngs/', a25, '" target="_blank" >', "WDR</a> <br>'")

 write(51, *)"sites = ["
 write(52, *)"sites = ["
 write(53, *)"sites = ["
 write(54, *)"sites = ["
 write(55, *)"sites = ["
 write(56, *)"sites = ["
 write(57, *)"sites = ["
 write(58, *)"sites = ["
 write(59, *)"sites = ["
 write(60, *)"sites = ["
 write(61, *)"sites = ["


 do
   read (21, '(A)', iostat=ios) line_buffer
   if (ios<0) exit !Negative ios means end-of-file

   call parse (line_buffer, ';', tokens, num_tokens)
   if(num_tokens .gt. 2) then
      num_stations = num_stations + 1
   
      station_id = tokens(1)
      lat_temp_c = tokens(2)
      lon_temp_c = tokens(3)
      qflag = tokens(4)

      call value(lat_temp_c, lat_temp, ios)
      call value(lon_temp_c, lon_temp, ios)

      lat = lat_temp
      if(lon_temp<180) then
         lon = lon_temp 
      else
         lon = lon_temp - 360
      end if

      png_spd_obs_5mb = trim(type)//'/'//trim(type)//'_sp_obs_'//trim(station_id)//'_5mb.png'
      png_spd_omg_5mb = trim(type)//'/'//trim(type)//'_sp_'//trim(station_id)//'_5mb.png'
      png_wdr_obs_5mb = trim(type)//'/'//trim(type)//'_dir_obs_'//trim(station_id)//'_5mb.png'
      png_wdr_omg_5mb = trim(type)//'/'//trim(type)//'_dir_'//trim(station_id)//'_5mb.png'

      png_spd_obs_30mb = trim(type)//'/'//trim(type)//'_sp_obs_'//trim(station_id)//'_30mb.png'
      png_spd_omg_30mb = trim(type)//'/'//trim(type)//'_sp_'//trim(station_id)//'_30mb.png'
      png_wdr_obs_30mb = trim(type)//'/'//trim(type)//'_dir_obs_'//trim(station_id)//'_30mb.png'
      png_wdr_omg_30mb = trim(type)//'/'//trim(type)//'_dir_'//trim(station_id)//'_30mb.png'

      png_spd_obs_125mb = trim(type)//'/'//trim(type)//'_sp_obs_'//trim(station_id)//'_125mb.png'
      png_spd_omg_125mb = trim(type)//'/'//trim(type)//'_sp_'//trim(station_id)//'_125mb.png'
      png_wdr_obs_125mb = trim(type)//'/'//trim(type)//'_dir_obs_'//trim(station_id)//'_125mb.png'
      png_wdr_omg_125mb = trim(type)//'/'//trim(type)//'_dir_'//trim(station_id)//'_125mb.png'

      png_spd_obs_225mb = trim(type)//'/'//trim(type)//'_sp_obs_'//trim(station_id)//'_225mb.png'
      png_spd_omg_225mb = trim(type)//'/'//trim(type)//'_sp_'//trim(station_id)//'_225mb.png'
      png_wdr_obs_225mb = trim(type)//'/'//trim(type)//'_dir_obs_'//trim(station_id)//'_225mb.png'
      png_wdr_omg_225mb = trim(type)//'/'//trim(type)//'_dir_'//trim(station_id)//'_225mb.png'

      png_spd_obs_325mb = trim(type)//'/'//trim(type)//'_sp_obs_'//trim(station_id)//'_325mb.png'
      png_spd_omg_325mb = trim(type)//'/'//trim(type)//'_sp_'//trim(station_id)//'_325mb.png'
      png_wdr_obs_325mb = trim(type)//'/'//trim(type)//'_dir_obs_'//trim(station_id)//'_325mb.png'
      png_wdr_omg_325mb = trim(type)//'/'//trim(type)//'_dir_'//trim(station_id)//'_325mb.png'

      png_spd_obs_425mb = trim(type)//'/'//trim(type)//'_sp_obs_'//trim(station_id)//'_425mb.png'
      png_spd_omg_425mb = trim(type)//'/'//trim(type)//'_sp_'//trim(station_id)//'_425mb.png'
      png_wdr_obs_425mb = trim(type)//'/'//trim(type)//'_dir_obs_'//trim(station_id)//'_425mb.png'
      png_wdr_omg_425mb = trim(type)//'/'//trim(type)//'_dir_'//trim(station_id)//'_425mb.png'

      png_spd_obs_525mb = trim(type)//'/'//trim(type)//'_sp_obs_'//trim(station_id)//'_525mb.png'
      png_spd_omg_525mb = trim(type)//'/'//trim(type)//'_sp_'//trim(station_id)//'_525mb.png'
      png_wdr_obs_525mb = trim(type)//'/'//trim(type)//'_dir_obs_'//trim(station_id)//'_525mb.png'
      png_wdr_omg_525mb = trim(type)//'/'//trim(type)//'_dir_'//trim(station_id)//'_525mb.png'

      png_spd_obs_625mb = trim(type)//'/'//trim(type)//'_sp_obs_'//trim(station_id)//'_625mb.png'
      png_spd_omg_625mb = trim(type)//'/'//trim(type)//'_sp_'//trim(station_id)//'_625mb.png'
      png_wdr_obs_625mb = trim(type)//'/'//trim(type)//'_dir_obs_'//trim(station_id)//'_625mb.png'
      png_wdr_omg_625mb = trim(type)//'/'//trim(type)//'_dir_'//trim(station_id)//'_625mb.png'

      png_spd_obs_725mb = trim(type)//'/'//trim(type)//'_sp_obs_'//trim(station_id)//'_725mb.png'
      png_spd_omg_725mb = trim(type)//'/'//trim(type)//'_sp_'//trim(station_id)//'_725mb.png'
      png_wdr_obs_725mb = trim(type)//'/'//trim(type)//'_dir_obs_'//trim(station_id)//'_725mb.png'
      png_wdr_omg_725mb = trim(type)//'/'//trim(type)//'_dir_'//trim(station_id)//'_725mb.png'

      png_spd_obs_825mb = trim(type)//'/'//trim(type)//'_sp_obs_'//trim(station_id)//'_825mb.png'
      png_spd_omg_825mb = trim(type)//'/'//trim(type)//'_sp_'//trim(station_id)//'_825mb.png'
      png_wdr_obs_825mb = trim(type)//'/'//trim(type)//'_dir_obs_'//trim(station_id)//'_825mb.png'
      png_wdr_omg_825mb = trim(type)//'/'//trim(type)//'_dir_'//trim(station_id)//'_825mb.png'

      png_spd_obs_925mb = trim(type)//'/'//trim(type)//'_sp_obs_'//trim(station_id)//'_925mb.png'
      png_spd_omg_925mb = trim(type)//'/'//trim(type)//'_sp_'//trim(station_id)//'_925mb.png'
      png_wdr_obs_925mb = trim(type)//'/'//trim(type)//'_dir_obs_'//trim(station_id)//'_925mb.png'
      png_wdr_omg_925mb = trim(type)//'/'//trim(type)//'_dir_'//trim(station_id)//'_925mb.png'

      write(51, fmt=95) lat, lon, qflag, station_id
      write(51, fmt=91) adjustl(png_spd_obs_5mb)
      write(51, fmt=92) adjustl(png_spd_omg_5mb)
      write(51, fmt=93) adjustl(png_wdr_obs_5mb)
      write(51, fmt=94) adjustl(png_wdr_omg_5mb)

!      write(51, *) '  +',"'", '<a href="../', trim(png_spd_obs_5mb),'" target="_blank" >',  "WSD_OBS</a> <br>'"
!      write(51, *) '  +',"'", '<a href="../', trim(png_spd_omg_5mb),'" target="_blank" >',  "WSD_OMG</a> <br>'"
!      write(51, *) '  +',"'", '<a href="../', trim(png_wdr_obs_5mb),'" target="_blank" >',  "WDR_OBS</a> <br>'"
!      write(51, *) '  +',"'", '<a href="../', trim(png_wdr_omg_5mb),'" target="_blank" >',  "WDR_OMG</a> <br>'"
!      write(51, *) "],"
!      write(51, *)

      write(52, fmt=95) lat, lon, qflag, station_id
      write(52, fmt=91) adjustl(png_spd_obs_30mb)
      write(52, fmt=92) adjustl(png_spd_omg_30mb)
      write(52, fmt=93) adjustl(png_wdr_obs_30mb)
      write(52, fmt=94) adjustl(png_wdr_omg_30mb)
      write(52, *) "],"
      write(52, *)

      write(53, fmt=95) lat, lon, qflag, station_id
      write(53, fmt=91) adjustl(png_spd_obs_125mb)
      write(53, fmt=92) adjustl(png_spd_omg_125mb)
      write(53, fmt=93) adjustl(png_wdr_obs_125mb)
      write(53, fmt=94) adjustl(png_wdr_omg_125mb)
      write(53, *) "],"
      write(53, *)

      write(54, fmt=95) lat, lon, qflag, station_id
      write(54, fmt=91) adjustl(png_spd_obs_225mb)
      write(54, fmt=92) adjustl(png_spd_omg_225mb)
      write(54, fmt=93) adjustl(png_wdr_obs_225mb)
      write(54, fmt=94) adjustl(png_wdr_omg_225mb)
      write(54, *) "],"
      write(54, *)

      write(55, fmt=95) lat, lon, qflag, station_id
      write(55, fmt=91) adjustl(png_spd_obs_325mb)
      write(55, fmt=92) adjustl(png_spd_omg_325mb)
      write(55, fmt=93) adjustl(png_wdr_obs_325mb)
      write(55, fmt=94) adjustl(png_wdr_omg_325mb)
      write(55, *) "],"
      write(55, *)

      write(56, fmt=95) lat, lon, qflag, station_id
      write(56, fmt=91) adjustl(png_spd_obs_425mb)
      write(56, fmt=92) adjustl(png_spd_omg_425mb)
      write(56, fmt=93) adjustl(png_wdr_obs_425mb)
      write(56, fmt=94) adjustl(png_wdr_omg_425mb)
      write(56, *) "],"
      write(56, *)

      write(57, fmt=95) lat, lon, qflag, station_id
      write(57, fmt=91) adjustl(png_spd_obs_525mb)
      write(57, fmt=92) adjustl(png_spd_omg_525mb)
      write(57, fmt=93) adjustl(png_wdr_obs_525mb)
      write(57, fmt=94) adjustl(png_wdr_omg_525mb)
      write(57, *) "],"
      write(57, *)

      write(58, fmt=95) lat, lon, qflag, station_id
      write(58, fmt=91) adjustl(png_spd_obs_625mb)
      write(58, fmt=92) adjustl(png_spd_omg_625mb)
      write(58, fmt=93) adjustl(png_wdr_obs_625mb)
      write(58, fmt=94) adjustl(png_wdr_omg_625mb)
      write(58, *) "],"
      write(58, *)

      write(59, fmt=95) lat, lon, qflag, station_id
      write(59, fmt=91) adjustl(png_spd_obs_725mb)
      write(59, fmt=92) adjustl(png_spd_omg_725mb)
      write(59, fmt=93) adjustl(png_wdr_obs_725mb)
      write(59, fmt=94) adjustl(png_wdr_omg_725mb)
      write(59, *) "],"
      write(59, *)

      write(60, fmt=95) lat, lon, qflag, station_id
      write(60, fmt=91) adjustl(png_spd_obs_825mb)
      write(60, fmt=92) adjustl(png_spd_omg_825mb)
      write(60, fmt=93) adjustl(png_wdr_obs_825mb)
      write(60, fmt=94) adjustl(png_wdr_omg_825mb)
      write(60, *) "],"
      write(60, *)

      write(61, fmt=95) lat, lon, qflag, station_id
      write(61, fmt=91) adjustl(png_spd_obs_925mb)
      write(61, fmt=92) adjustl(png_spd_omg_925mb)
      write(61, fmt=93) adjustl(png_wdr_obs_925mb)
      write(61, fmt=94) adjustl(png_wdr_omg_925mb)
      write(61, *) "],"
      write(61, *)

!      print *, color_key 
!      select case(color_key)
!        case(1) 
!          write(51, *) "icon = ", "'", "red", "'"
!        case(2)
!          write(51, *) "icon = ", "'", "blue", "'"
!        case(3)
!          write(51, *) "icon = ", "'", "green", "'" 
!      end select


   else
     cycle

   end if!num_tokens .ge. 2 

 end do

 write(51, *)"];"
 write(52, *)"];"
 write(53, *)"];"
 write(54, *)"];"
 write(55, *)"];"
 write(56, *)"];"
 write(57, *)"];"
 write(58, *)"];"
 write(59, *)"];"
 write(60, *)"];"
 write(61, *)"];"

 close (21)
 close (51)
 close (52)
 close (53)
 close (54)
 close (55)
 close (56)
 close (57)
 close (58)
 close (59)
 close (60)
 close (61)

 print *, "Total number of stations: ", num_stations

end program create_markes_multilevel
