
cat <<EOF > enkf.nml

 &nam_enkf
  datestring          = '$ANAL_TIME',
  datapath            = './',
  analpertwtnh        = 0.9,
  analpertwtsh        = 0.9,
  analpertwttr        = 0.9,
  lupd_satbiasc       = .false.,
  zhuberleft          = 1.e10,
  zhuberright         = 1.e10,
  huber               = .false.,
  varqc               = .false.,
  covinflatemax       = 1.e2,
  covinflatemin       = 1.0,
  pseudo_rh           = .true.,
  corrlengthnh        = 500,
  corrlengthsh        = 500,
  corrlengthtr        = 500,
  obtimelnh           = 1.e30,
  obtimelsh           = 1.e30,
  obtimeltr           = 1.e30,
  iassim_order        = 0,
  lnsigcutoffnh       = 0.4,
  lnsigcutoffsh       = 0.4,
  lnsigcutofftr       = 0.4,
  lnsigcutoffsatnh    = 0.4,
  lnsigcutoffsatsh    = 0.4,
  lnsigcutoffsattr    = 0.4,
  lnsigcutoffpsnh     = 0.4,
  lnsigcutoffpssh     = 0.4,
  lnsigcutoffpstr     = 0.4,
  simple_partition    = .true.,
  nlons               = $NLONS,
  nlats               = $NLATS,
  smoothparm          = -1,
  readin_localization = .false.,
  saterrfact          = 1.0,
  numiter             = 6,
  sprd_tol            = 1.e30,
  paoverpb_thresh     = 0.99,
  reducedgrid         = .false.,
  nlevs               = $NLEVS,
  nanals              = $NMEM_ENKF,
  nvars               = 5,
  deterministic       = .true.,
  sortinc             = .true.,
  univaroz            = .true.,
  regional            = .true., 
  adp_anglebc         = .true.,
  angord              = 4,
  use_edges           = .false.,
  emiss_bc            = .true.,
  biasvar             = -500 
/
 &satobs_enkf
  sattypes_rad(1)     = 'amsua_n15',     dsis(1) = 'amsua_n15',
  sattypes_rad(2)     = 'amsua_n18',     dsis(2) = 'amsua_n18',
  sattypes_rad(3)     = 'amsua_n19',     dsis(3) = 'amsua_n19',
  sattypes_rad(4)     = 'amsub_n16',     dsis(4) = 'amsub_n16',
  sattypes_rad(5)     = 'amsub_n17',     dsis(5) = 'amsub_n17',
  sattypes_rad(6)     = 'amsua_aqua',    dsis(6) = 'amsua_aqua',
  sattypes_rad(7)     = 'amsua_metop-a', dsis(7) = 'amsua_metop-a',
  sattypes_rad(8)     = 'airs_aqua',     dsis(8) = 'airs281SUBSET_aqua',
  sattypes_rad(9)     = 'hirs3_n17',     dsis(9) = 'hirs3_n17',
  sattypes_rad(10)    = 'hirs4_n19',     dsis(10)= 'hirs4_n19',
  sattypes_rad(11)    = 'hirs4_metop-a', dsis(11)= 'hirs4_metop-a',
  sattypes_rad(12)    = 'mhs_n18',       dsis(12)= 'mhs_n18',
  sattypes_rad(13)    = 'mhs_n19',       dsis(13)= 'mhs_n19',
  sattypes_rad(14)    = 'mhs_metop-a',   dsis(14)= 'mhs_metop-a',
  sattypes_rad(15)    = 'goes_img_g11',  dsis(15)= 'imgr_g11',
  sattypes_rad(16)    = 'goes_img_g12',  dsis(16)= 'imgr_g12',
  sattypes_rad(17)    = 'goes_img_g13',  dsis(17)= 'imgr_g13',
  sattypes_rad(18)    = 'goes_img_g14',  dsis(18)= 'imgr_g14',
  sattypes_rad(19)    = 'goes_img_g15',  dsis(19)= 'imgr_g15',
  sattypes_rad(20)    = 'avhrr3_n18',    dsis(20)= 'avhrr3_n18',
  sattypes_rad(21)    = 'avhrr3_metop-a',dsis(21)= 'avhrr3_metop-a',
  sattypes_rad(22)    = 'avhrr3_n19',    dsis(22)= 'avhrr3_n19',
  sattypes_rad(23)    = 'amsre_aqua',    dsis(23)= 'amsre_aqua',
  sattypes_rad(24)    = 'ssmis_f16',     dsis(24)= 'ssmis_f16',
  sattypes_rad(25)    = 'ssmis_f17',     dsis(25)= 'ssmis_f17',
  sattypes_rad(26)    = 'ssmis_f18',     dsis(26)= 'ssmis_f18',
  sattypes_rad(27)    = 'ssmis_f19',     dsis(27)= 'ssmis_f19',
  sattypes_rad(28)    = 'ssmis_f20',     dsis(28)= 'ssmis_f20',
  sattypes_rad(29)    = 'sndrd1_g11',    dsis(29)= 'sndrD1_g11',
  sattypes_rad(30)    = 'sndrd2_g11',    dsis(30)= 'sndrD2_g11',
  sattypes_rad(31)    = 'sndrd3_g11',    dsis(31)= 'sndrD3_g11',
  sattypes_rad(32)    = 'sndrd4_g11',    dsis(32)= 'sndrD4_g11',
  sattypes_rad(33)    = 'sndrd1_g12',    dsis(33)= 'sndrD1_g12',
  sattypes_rad(34)    = 'sndrd2_g12',    dsis(34)= 'sndrD2_g12',
  sattypes_rad(35)    = 'sndrd3_g12',    dsis(35)= 'sndrD3_g12',
  sattypes_rad(36)    = 'sndrd4_g12',    dsis(36)= 'sndrD4_g12',
  sattypes_rad(37)    = 'sndrd1_g13',    dsis(37)= 'sndrD1_g13',
  sattypes_rad(38)    = 'sndrd2_g13',    dsis(38)= 'sndrD2_g13',
  sattypes_rad(39)    = 'sndrd3_g13',    dsis(39)= 'sndrD3_g13',
  sattypes_rad(40)    = 'sndrd4_g13',    dsis(40)= 'sndrD4_g13',
  sattypes_rad(41)    = 'sndrd1_g14',    dsis(41)= 'sndrD1_g14',
  sattypes_rad(42)    = 'sndrd2_g14',    dsis(42)= 'sndrD2_g14',
  sattypes_rad(43)    = 'sndrd3_g14',    dsis(43)= 'sndrD3_g14',
  sattypes_rad(44)    = 'sndrd4_g14',    dsis(44)= 'sndrD4_g14',
  sattypes_rad(45)    = 'sndrd1_g15',    dsis(45)= 'sndrD1_g15',
  sattypes_rad(46)    = 'sndrd2_g15',    dsis(46)= 'sndrD2_g15',
  sattypes_rad(47)    = 'sndrd3_g15',    dsis(47)= 'sndrD3_g15',
  sattypes_rad(48)    = 'sndrd4_g15',    dsis(48)= 'sndrD4_g15',
  sattypes_rad(49)    = 'iasi_metop-a',  dsis(49)= 'iasi616_metop-a',
  sattypes_rad(50)    = 'seviri_m08',    dsis(50)= 'seviri_m08',
  sattypes_rad(51)    = 'seviri_m09',    dsis(51)= 'seviri_m09',
  sattypes_rad(52)    = 'seviri_m10',    dsis(52)= 'seviri_m10',
  sattypes_rad(53)    = 'amsua_metop-b', dsis(53)= 'amsua_metop-b',
  sattypes_rad(54)    = 'hirs4_metop-b', dsis(54)= 'hirs4_metop-b',
  sattypes_rad(55)    = 'mhs_metop-b',   dsis(15)= 'mhs_metop-b',
  sattypes_rad(56)    = 'iasi_metop-b',  dsis(56)= 'iasi616_metop-b',
  sattypes_rad(57)    = 'avhrr3_metop-b',dsis(56)= 'avhrr3_metop-b',
  sattypes_rad(58)    = 'atms_npp',      dsis(58)= 'atms_npp',
  sattypes_rad(59)    = 'cris_npp',      dsis(59)= 'cris_npp',
 /
 &ozobs_enkf
  sattypes_oz(1)      = 'sbuv2_n16',
  sattypes_oz(2)      = 'sbuv2_n17',
  sattypes_oz(3)      = 'sbuv2_n18',
  sattypes_oz(4)      = 'sbuv2_n19',
  sattypes_oz(5)      = 'omi_aura',
  sattypes_oz(6)      = 'gome_metop-a',
  sattypes_oz(7)      = 'gome_metop-b',
 /
&nam_wrf
  arw                 = $IF_ARW,
  nmm                 = $IF_NMM,
 / 
EOF
