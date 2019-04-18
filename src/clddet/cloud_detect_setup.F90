SUBROUTINE Cloud_Detect_Setup

!    This software was developed within the context of
!    the EUMETSAT Satellite Application Facility on
!    Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 25 November 1998, between
!    EUMETSAT and the Met Office, UK, by one or more partners
!    within the NWP SAF. The partners in the NWP SAF are
!    the Met Office, ECMWF, KNMI and MeteoFrance.
!
!    Copyright 2013, EUMETSAT, All Rights Reserved.


!**** Cloud detection setup
!        A. Collard  ECMWF 01/02/06

!     PURPOSE
!     -------
!     Initialise cloud detection parameters for advanced infrared sounders.

!**   INTERFACE
!     ---------
!     Cloud_detect_setup is called from cloud_detect_wrapper.

!     METHOD
!     ------
!     Default values are assigned to the cloud detections setup structure.

!     MODIFICATIONS
!     -------------
!     01/02/06   A.Collard   1.0   Original code.
!     19/10/06   A.Collard   1.1   Use IASI 300 Subset Channels.
!     17/11/09   R.Eresmaa   1.2   Use IASI 366 Subset Channels.
!                                  Include parameters of the Quick Exit /
!                                  long-wave window gradient check parameters.
!     11/11/11   R.Eresmaa   1.3   Default channel list for AIRS bands 3-5
!                                  modified.
!                                  Processing capability for CrIS added
!                                  assuming a selection of 320 channels.
!     03/12/13   R,Eresmaa   2.0   Imager-assisted cloud detection added for
!                                  IASI.
!                                  Updated setup for CrIS.
!     19/01/15   R.Eresmaa   2.1   Remove unused variable specifications and
!                                  switch aerosol detection on by default for
!                                  AIRS and IASI.
!     10/11/15   R.Eresmaa   2.2   Changed instrument ID naming convention.
!                                  Changed parameters of aerosol detection.
!     20/12/16   R.Eresmaa   2.3   Remove settings for aerosol detection.

USE YOMIASI, ONLY   : S__Cloud_Detect_Setup, &
&                     JP__Min_Sensor_Index,  &
&                     JP__Max_Sensor_Index,  &
&                     INST_ID_AIRS,          &
&                     INST_ID_IASI,          &
&                     INST_ID_CRIS,          &
&                     INST_ID_IRS,           &
&                     INST_ID_IASING,        &
&                     JP__Digital_Filter

IMPLICIT NONE

! Local variables

CHARACTER(LEN=6)   :: CL__InstrumentName
CHARACTER(LEN=16)  :: CL__Cloud_Detection_File

INTEGER  :: J, J__Sensor      ! Loop variables
INTEGER  :: INIU1, IOS
INTEGER  :: N__Filter_Method

!-----------------------
! Namelist variables
!-----------------------

! N.B. Max_Bands must be greater than 5
INTEGER, PARAMETER :: I__Max_Bands    =    8
INTEGER, PARAMETER :: I__Max_Channels = 8461

INTEGER  :: M__Sensor
INTEGER  :: N__Num_Bands
INTEGER  :: N__GradChkInterval(I__Max_Bands)
INTEGER  :: N__Band_Size(I__Max_Bands)
INTEGER  :: N__Bands(I__Max_Channels,I__Max_Bands)
INTEGER  :: N__Window_Width(I__Max_Bands)
INTEGER  :: N__Window_Bounds(I__Max_Bands,2)
REAL     :: R__BT_Threshold(I__Max_Bands)
REAL     :: R__Grad_Threshold(I__Max_Bands)
REAL     :: R__Window_Grad_Threshold(I__Max_Bands)
LOGICAL  :: L__Do_Quick_Exit
LOGICAL  :: L__Do_CrossBand
INTEGER  :: N__BandToUse(I__Max_Bands)

! Imager cloud detection
LOGICAL  :: L__Do_Imager_Cloud_Detection
INTEGER  :: N__Num_Imager_Chans
INTEGER  :: N__Num_Imager_Clusters
INTEGER  :: N__Imager_Chans(I__Max_Bands)
REAL     :: R__Stddev_Threshold(I__Max_Bands)
REAL     :: R__Coverage_Threshold
REAL     :: R__FG_Departure_Threshold

! Namelist

NAMELIST / Cloud_Detect_Coeffs / M__Sensor, N__Num_Bands,                    &
&        N__GradChkInterval, N__Band_Size, N__Bands, N__Window_Width,        &
&        N__Window_Bounds, R__Window_Grad_Threshold,                         &
&        R__BT_Threshold, R__Grad_Threshold, L__Do_Quick_Exit,               &
&        L__Do_CrossBand, N__BandToUse,                                      &
&        L__Do_Imager_Cloud_Detection, N__Num_Imager_Chans,                  &
&        N__Num_Imager_Clusters, N__Imager_Chans,                            &
&        R__Stddev_Threshold, R__Coverage_Threshold,                         &
&        R__FG_Departure_Threshold

INCLUDE 'abor1.intfb'


!============================================================================

IF (I__Max_Bands < 5) CALL ABOR1 ('I__Max_Bands must be greater than 5')

!============================================================================
!   Loop through sensors setting up cloud detection
!============================================================================

SensorLoop : DO J__Sensor = JP__Min_Sensor_Index, JP__Max_Sensor_Index


   SELECT CASE (J__Sensor)

   CASE(INST_ID_AIRS)
      !====================
      ! Set up AIRS
      !====================

      CL__InstrumentName='AIRS'
      CL__Cloud_Detection_File = 'AIRS_CLDDET.NL'

      N__Filter_Method = JP__Digital_Filter

      N__Num_Bands = 5

      N__Band_Size(:) = 0
      N__Band_Size(1:N__Num_Bands) =(/141, 36, 54, 23, 65 /)  !orig
      N__Band_Size(1:N__Num_Bands) =(/127, 35, 49, 15, 51 /)  !emily

      IF (MAXVAL(N__Band_Size(:)) > I__Max_Channels) &
&              CALL ABOR1('Too many channels specified in cloud '//&
&                         'detection - increase I__Max_Channels')

      N__Bands(:,:)= 0
!>>orig
!      N__Bands(1:N__Band_Size(1),1) = &
!&    (/    1,    6,    7,   10,   11,   15,   16,   17,   20,   21, &
!&         22,   24,   27,   28,   30,   36,   39,   40,   42,   51, &
!&         52,   54,   55,   56,   59,   62,   63,   68,   69,   71, &
!&         72,   73,   74,   75,   76,   77,   78,   79,   80,   82, &
!&         83,   84,   86,   92,   93,   98,   99,  101,  104,  105, &
!&        108,  110,  111,  113,  116,  117,  123,  124,  128,  129, &
!&        138,  139,  144,  145,  150,  151,  156,  157,  159,  162, &
!&        165,  168,  169,  170,  172,  173,  174,  175,  177,  179, &
!&        180,  182,  185,  186,  190,  192,  193,  198,  201,  204, &
!&        207,  210,  213,  215,  216,  218,  221,  224,  226,  227, &
!&        232,  239,  248,  250,  251,  252,  253,  256,  257,  261, &
!&        262,  267,  272,  295,  299,  300,  305,  308,  309,  310, &
!&        318,  321,  325,  333,  338,  355,  362,  375,  453,  475, &
!&        484,  497,  528,  587,  672,  787,  791,  843,  870,  914, &
!&        950 /)
!
!      N__Bands(1:N__Band_Size(2),2) = &
!&    (/ 1003, 1012, 1019, 1024, 1030, 1038, 1048, 1069, 1079, 1082, &
!&       1083, 1088, 1090, 1092, 1095, 1104, 1111, 1115, 1116, 1119, &
!&       1120, 1123, 1130, 1138, 1142, 1178, 1199, 1206, 1221, 1237, &
!&       1252, 1260, 1263, 1266, 1278, 1285 /)
!
!      N__Bands(1:N__Band_Size(3),3) = &
!&    (/ 1290, 1301, 1304, 1329, 1371, 1382, 1415, 1424, 1449, 1455, &
!&       1466, 1471, 1477, 1479, 1488, 1500, 1519, 1520, 1538, 1545, &
!&       1565, 1574, 1583, 1593, 1614, 1627, 1636, 1644, 1652, 1669, &
!&       1674, 1681, 1694, 1708, 1717, 1723, 1740, 1748, 1751, 1756, &
!&       1763, 1766, 1771, 1777, 1780, 1783, 1794, 1800, 1803, 1806, &
!&       1812, 1826, 1843, 1852 /)
!
!      N__Bands(1:N__Band_Size(4),4) = &
!&    (/ 1865, 1866, 1867, 1868, 1869, 1872, 1873, 1875, 1876, 1877, &
!&       1881, 1882, 1883, 1884, 1897, 1901, 1911, 1917, 1918, 1921, &
!&       1923, 1924, 1928 /)
!
!      N__Bands(1:N__Band_Size(5),5) = &
!&    (/ 1937, 1938, 1939, 1941, 1946, 1947, 1948, 1958, 1971, 1973, &
!&       1988, 1995, 2084, 2085, 2097, 2098, 2099, 2100, 2101, 2103, &
!&       2104, 2106, 2107, 2108, 2109, 2110, 2111, 2112, 2113, 2114, &
!&       2115, 2116, 2117, 2118, 2119, 2120, 2121, 2122, 2123, 2128, &
!&       2134, 2141, 2145, 2149, 2153, 2164, 2189, 2197, 2209, 2226, &
!&       2234, 2280, 2318, 2321, 2325, 2328, 2333, 2339, 2348, 2353, &
!&       2355, 2363, 2370, 2371, 2377 /)
!<<orig
!>>emily
      N__Bands(1:N__Band_Size(1),1) = &
&    (/    1,   6,   7,  10,  11,  15,  16,  17,  20,  21, &
&         22,  24,  27,  28,  30,  36,  39,  40,  42,  51, &
&         52,  54,  55,  56,  59,  62,  63,  68,  69,  71, &
&         72,  73,  74,  75,  76,  77,  78,  79,  80,  82, &
&         83,  84,  86,  92,  93,  98,  99, 101, 104, 105, &
&        108, 110, 111, 113, 116, 117, 123, 124, 128, 129, &
&        138, 139, 144, 145, 150, 151, 156, 157, 159, 162, &
&        165, 168, 169, 170, 172, 173, 174, 175, 177, 179, &
&        180, 182, 185, 186, 190, 192, 198, 201, 204, 207, & 
&        210, 215, 216, 221, 226, 227, 232, 252, 253, 256, &
&        257, 261, 262, 267, 272, 295, 299, 305, 310, 321, &
&        333, 338, 355, 362, 375, 475, 484, 497, 528, 587, &
&        672, 787, 791, 843, 870, 914, 950 /)

      N__Bands(1:N__Band_Size(2),2) = &
&    (/ 1003, 1012, 1019, 1024, 1030, 1038, 1048, 1069, 1079, 1082, &
&       1083, 1088, 1090, 1092, 1095, 1104, 1111, 1115, 1116, 1119, &
&       1120, 1123, 1130, 1138, 1142, 1178, 1199, 1206, 1221, 1237, &
&       1252, 1260, 1263, 1266, 1285 /)

      N__Bands(1:N__Band_Size(3),3) = &
&    (/ 1301, 1304, 1329, 1371, 1382, 1415, 1424, 1449, 1455, 1466, &
&       1477, 1500, 1519, 1538, 1545, 1565, 1574, 1583, 1593, 1614, &
&       1627, 1636, 1644, 1652, 1669, 1674, 1681, 1694, 1708, 1717, &
&       1723, 1740, 1748, 1751, 1756, 1763, 1766, 1771, 1777, 1780, &
&       1783, 1794, 1800, 1803, 1806, 1812, 1826, 1843, 1852 /)

      N__Bands(1:N__Band_Size(4),4) = &
&    (/ 1865, 1866, 1868, 1869, 1872, 1873, 1876, 1881, 1882, 1883, &
&       1911, 1917, 1918, 1924, 1928 /)

      N__Bands(1:N__Band_Size(5),5) = &
&    (/ 1937, 1941, 2099, 2100, 2101, 2103, 2104, 2106, 2107, 2108, &
&       2109, 2110, 2111, 2112, 2113, 2114, 2115, 2116, 2117, 2118, &
&       2119, 2120, 2121, 2122, 2123, 2128, 2134, 2141, 2145, 2149, &
&       2153, 2164, 2189, 2197, 2209, 2226, 2234, 2280, 2318, 2321, &
&       2325, 2328, 2333, 2339, 2348, 2353, 2355, 2363, 2370, 2371, &
&       2377 /)
!<<emily

      N__GradChkInterval(:) = 0
      N__GradChkInterval(1:N__Num_Bands) = (/ 5,5,5,5,5 /)

      N__Window_Width(:) = 0
      N__Window_Width(1:N__Num_Bands) = (/ 10,6,8,5,8 /)  !orig
      N__Window_Width(1:N__Num_Bands) = (/ 14,6,8,5,8 /)  !emily

      N__Window_Bounds(:,:) = 0
      N__Window_Bounds(1,1) = 475
      N__Window_Bounds(1,2) = 950

      R__BT_Threshold(:) = 0.
      R__BT_Threshold(1:N__Num_Bands) = (/ 0.5, 0.5, 0.5, 0.5, 0.5/)   !orig
      R__BT_Threshold(1:N__Num_Bands) = (/ 0.43, 0.5, 0.5, 0.5, 0.5/)  !emily

      R__Grad_Threshold(:) = 0.
      R__Grad_Threshold(1:N__Num_Bands) = (/ 0.02, 0.02, 0.02, 0.02, 0.02 /)

      R__Window_Grad_Threshold(:) = 0.
      R__Window_Grad_Threshold(1) = 0.4

      L__Do_Quick_Exit = .TRUE.


      ! This is cross-band:

      L__Do_CrossBand = .TRUE.

      N__BandToUse(:) = 0
      N__BandToUse(1:N__Num_Bands) = (/ 1,1,1,4,5 /)  !orig
      N__BandToUse(1:N__Num_Bands) = (/ 1,1,1,4,1 /)  !emily


      ! This is the setup for imager cloud detection

      L__Do_Imager_Cloud_Detection = .FALSE.

      N__Num_Imager_Chans = 0
      N__Num_Imager_Clusters = 0
      N__Imager_Chans(:) = 0

      R__Stddev_Threshold(:) = 0.0
      R__Coverage_Threshold = 0.0
      R__FG_Departure_Threshold = 0.0


   CASE(INST_ID_IASI)
      !====================
      ! Set up IASI
      !====================

      CL__InstrumentName='IASI'
      CL__Cloud_Detection_File = 'IASI_CLDDET.NL'

      N__Filter_Method = JP__Digital_Filter

      N__Num_Bands = 5

      N__Band_Size(:) = 0
!     N__Band_Size(1:N__Num_Bands) =(/ 193, 15, 116, 4, 15 /)    !orig
      N__Band_Size(1:N__Num_Bands) =(/ 149, 15, 116, 4, 15 /)    !emily:assimilated
      N__Band_Size(1:N__Num_Bands) =(/ 164, 15, 116, 4, 15 /)    !emily

      IF (MAXVAL(N__Band_Size(:)) > I__Max_Channels) &
&              CALL ABOR1('Too many channels specified in cloud '//&
&                         'detection - increase I__Max_Channels')

      N__Bands(:,:)= 0

      ! Use the "IASI 366" Subset
!>>orig
!      N__Bands(1:N__Band_Size(1),1) = &
!&    (/   16,   38,   49,   51,   55,   57,   59,   61,   63,   66, &
!&         70,   72,   74,   79,   81,   83,   85,   87,   89,   92, &
!&         95,   97,   99,  101,  104,  106,  109,  111,  113,  116, &
!&        119,  122,  125,  128,  131,  133,  135,  138,  141,  144, &
!&        146,  148,  151,  154,  157,  159,  161,  163,  165,  167, &
!&        170,  173,  176,  178,  179,  180,  183,  185,  187,  189, &
!&        191,  193,  195,  197,  199,  201,  203,  205,  207,  210, &
!&        212,  214,  217,  219,  222,  224,  226,  228,  230,  232, &
!&        234,  236,  239,  241,  242,  243,  246,  249,  252,  254, &
!&        256,  258,  260,  262,  265,  267,  269,  271,  272,  273, &
!&        275,  278,  280,  282,  284,  286,  288,  290,  292,  294, &
!&        296,  299,  301,  303,  306,  308,  310,  312,  314,  316, &
!&        318,  320,  323,  325,  327,  329,  331,  333,  335,  337, &
!&        339,  341,  343,  345,  347,  350,  352,  354,  356,  358, &
!&        360,  362,  364,  366,  369,  371,  373,  375,  377,  379, &
!&        381,  383,  386,  389,  398,  401,  404,  407,  410,  414, &
!&        416,  426,  428,  432,  434,  439,  445,  457,  515,  546, &
!&        552,  559,  566,  571,  573,  646,  662,  668,  756,  867, &
!&        921, 1027, 1090, 1133, 1191, 1194, 1271, 1805, 1884, 1946, &
!&       1991, 2094, 2239 /)
!<<orig
!>>emily:assiilated channels
!      N__Bands(1:N__Band_Size(1),1) = &
!&    (/   16,   38,   49,   51,   55,   57,   59,   61,   63,   66, &
!&         70,   72,   74,   79,   81,   83,   85,   87,  104,  106, &
!&        109,  111,  113,  116,  119,  122,  125,  128,  131,  133, &
!&        135,  138,  141,  144,  146,  148,  151,  154,  157,  159, &
!&        161,  163,  167,  170,  173,  176,  180,  185,  187,  193, &
!&        199,  205,  207,  210,  212,  214,  217,  219,  222,  224, &
!&        226,  230,  232,  236,  239,  243,  246,  249,  252,  254, &
!&        260,  262,  265,  267,  275,  282,  294,  296,  299,  303, &
!&        306,  323,  327,  329,  335,  345,  347,  350,  354,  356, &
!&        360,  366,  371,  373,  375,  377,  379,  381,  383,  386, &
!&        389,  398,  401,  404,  407,  410,  414,  416,  426,  428, &
!&        432,  434,  439,  445,  457,  515,  546,  552,  559,  566, &
!&        571,  573,  646,  662,  668,  756,  867,  906,  921, 1027, &
!&       1046, 1121, 1133, 1191, 1194, 1271, 1786, 1805, 1884, 1991, &
!&       2019, 2094, 2119, 2213, 2239, 2271, 2321, 2398, 2701 /)
!<<emily
!>>emily
      N__Bands(1:N__Band_Size(1),1) = &
&    (/   16,   38,   49,   51,   55,   57,   59,   61,   63,   66, &
&         70,   72,   74,   79,   81,   83,   85,   87,   89,   92, &
&         95,   97,   99,  101,  104,  106,  109,  111,  113,  116, &
&        119,  122,  125,  128,  131,  133,  135,  138,  141,  144, & 
&        146,  148,  151,  154,  157,  159,  161,  163,  167,  170, & 
&        173,  176,  179,  180,  185,  187,  191,  193,  197,  199, &
&        202,  203,  205,  207,  210,  212,  214,  217,  219,  222, & 
&        224,  226,  228,  230,  232,  236,  239,  243,  246,  249, & 
&        252,  254,  260,  262,  265,  267,  269,  275,  282,  285, &
&        294,  296,  299,  303,  306,  309,  313,  320,  323,  323, &
&        327,  329,  332,  335,  345,  347,  350,  354,  356,  360, &
&        363,  366,  371,  373,  375,  377,  379,  381,  383,  386, &
&        389,  398,  401,  404,  407,  410,  414,  416,  426,  428, &
&        432,  434,  439,  445,  457,  515,  546,  552,  559,  566, &
&        571,  573,  574,  646,  662,  668,  756,  867,  921, 1027, &
&       1046, 1090, 1121, 1133, 1191, 1194, 1271, 1786, 1805, 1884, &
&       1991, 2019, 2094, 2239 /)
!<<emily

      N__Bands(1:N__Band_Size(2),2) = &
&    (/ 1479, 1509, 1513, 1521, 1536, 1574, 1579, 1585, 1587, 1626, &
&       1639, 1643, 1652, 1658, 1671 /)

      N__Bands(1:N__Band_Size(3),3) = &
&    (/ 2119, 2213, 2271, 2321, 2398, 2701, 2741, 2819, 2889, 2907, &
&       2910, 2919, 2939, 2944, 2948, 2951, 2958, 2977, 2985, 2988, &
&       2991, 2993, 3002, 3008, 3014, 3027, 3029, 3036, 3047, 3049, &
&       3053, 3058, 3064, 3069, 3087, 3093, 3098, 3105, 3107, 3110, &
&       3127, 3136, 3151, 3160, 3165, 3168, 3175, 3178, 3207, 3228, &
&       3244, 3248, 3252, 3256, 3263, 3281, 3303, 3309, 3312, 3322, &
&       3375, 3378, 3411, 3438, 3440, 3442, 3444, 3446, 3448, 3450, &
&       3452, 3454, 3458, 3467, 3476, 3484, 3491, 3497, 3499, 3504, &
&       3506, 3509, 3518, 3527, 3555, 3575, 3577, 3580, 3582, 3586, &
&       3589, 3599, 3653, 3658, 3661, 4032, 5368, 5371, 5379, 5381, &
&       5383, 5397, 5399, 5401, 5403, 5405, 5455, 5480, 5483, 5485, &
&       5492, 5502, 5507, 5509, 5517, 5558 /)

      N__Bands(1:N__Band_Size(4),4) = &
&    (/ 5988, 5992, 5994, 6003 /)

      N__Bands(1:N__Band_Size(5),5) = &
&    (/ 6982, 6985, 6987, 6989, 6991, 6993, 6995, 6997, 7267, 7269, &
&       7424, 7426, 7428, 7885, 8007 /)


      N__GradChkInterval(:) = 0
      N__GradChkInterval(1:N__Num_Bands) = (/ 5,5,5,5,5 /)  

      N__Window_Width(:) = 0
      N__Window_Width(1:N__Num_Bands) = (/ 10,6,8,5,8 /)   

      N__Window_Bounds(:,:) = 0
      N__Window_Bounds(1,1) = 573
      N__Window_Bounds(1,2) = 2239

      R__BT_Threshold(:) = 0.
      R__BT_Threshold(1:N__Num_Bands) = (/ 0.5, 0.5, 0.5, 0.5, 0.5/) 

      R__Grad_Threshold(:) = 0.
      R__Grad_Threshold(1:N__Num_Bands) = (/ 0.02, 0.02, 0.02, 0.02, 0.02 /)

      R__Window_Grad_Threshold(:) = 0.
      R__Window_Grad_Threshold(1) = 0.4

      L__Do_Quick_Exit = .TRUE.


      ! This is cross-band:

      L__Do_CrossBand = .TRUE.

      N__BandToUse(:) = 0
      N__BandToUse(1:N__Num_Bands) = (/ 1,1,1,1,1 /)


      ! This is the setup for imager cloud detection

      L__Do_Imager_Cloud_Detection = .TRUE.

      N__Num_Imager_Chans = 2
      N__Num_Imager_Clusters = 7

      N__Imager_Chans(:) = 0
      N__Imager_Chans(1:N__Num_Imager_Chans) = (/ 2, 3 /)

      R__Stddev_Threshold(:) = 0.0
      R__Stddev_Threshold(1:N__Num_Imager_Chans) = (/ 0.75, 0.80 /)

      R__Coverage_Threshold = 0.03
      R__FG_Departure_Threshold = 1.0


   CASE(INST_ID_CRIS)
      !====================
      ! Set up CRIS
      !====================

      CL__InstrumentName='CRIS'
      CL__Cloud_Detection_File = 'CRIS_CLDDET.NL'

      N__Filter_Method = JP__Digital_Filter

      N__Num_Bands = 5

      N__Band_Size(:) = 0

!     N__Band_Size(1:N__Num_Bands) =(/ 143, 92, 54, 5, 6 /)   !orig
      N__Band_Size(1:N__Num_Bands) =(/  91, 92, 54, 5, 6 /)   !emily

      IF (MAXVAL(N__Band_Size(:)) > I__Max_Channels) &
&              CALL ABOR1('Too many channels specified in cloud '//&
&                      'detection - increase I__Max_Channels')

      N__Bands(:,:)= 0

!>>orig
!      ! Use the "CRIS 300" Subset
!      N__Bands(1:N__Band_Size(1),1) = &
!&    (/    1,    5,    9,   13,   17,   18,   19,   20,   21,   22, &
!&         23,   24,   25,   26,   27,   28,   29,   30,   31,   32, &
!&         33,   34,   35,   36,   37,   38,   39,   40,   41,   42, &
!&         43,   44,   45,   46,   47,   48,   49,   50,   51,   52, &
!&         53,   54,   55,   56,   57,   58,   59,   60,   61,   62, &
!&         63,   64,   65,   66,   67,   68,   69,   70,   71,   72, &
!&         73,   74,   75,   76,   77,   78,   79,   80,   81,   82, &
!&         83,   84,   85,   86,   87,   88,   89,   90,   91,   92, &
!&         93,   94,   95,   96,   97,   99,  101,  103,  105,  107, &
!&        109,  111,  113,  115,  116,  117,  118,  119,  120,  121, &
!&        122,  123,  124,  125,  133,  135,  137,  139,  141,  145, &
!&        149,  157,  161,  165,  169,  173,  177,  181,  185,  205, &
!&        221,  225,  229,  249,  257,  269,  273,  293,  301,  317, &
!&        333,  349,  369,  409,  433,  457,  481,  501,  549,  701, &
!&        705,  709,  713  /)
!
!      N__Bands(1:N__Band_Size(2),2) = &
!&    (/    3,    6,    7,    8,   10,   12,   14,   15,   16,  102, &
!&        104,  106,  108,  110,  114,  126,  127,  129,  132,  134, &
!&        138,  140,  143,  144,  146,  147,  148,  150,  151,  153, &
!&        155,  156,  158,  159,  162,  163,  164,  166,  170,  171, &
!&        172,  175,  180,  189,  195,  200,  201,  206,  210,  214, &
!&        217,  218,  226,  228,  230,  231,  233,  236,  240,  241, &
!&        245,  248,  252,  264,  265,  281,  285,  297,  324,  327, &
!&        473,  493,  500,  503,  511,  529,  534,  538,  542,  544, &
!&        545,  547,  550,  553,  555,  589,  605,  621,  637,  653, &
!&        669,  685  /)
!
!      N__Bands(1:N__Band_Size(3),3) = &
!&    (/  717,  725,  733,  741,  749,  757,  765,  773,  781,  789, &
!&        797,  805,  813,  821,  829,  837,  845,  853,  861,  869, &
!&        877,  885,  893,  901,  909,  917,  925,  933,  941,  949, &
!&        957,  965,  973,  981,  989,  997, 1005, 1013, 1021, 1029, &
!&       1037, 1045, 1053, 1061, 1069, 1077, 1085, 1093, 1101, 1109, &
!&       1117, 1125, 1133, 1141  /)
!
!      N__Bands(1:N__Band_Size(4),4) = &
!&    (/ 1149, 1157, 1165, 1173, 1181 /)
!
!      N__Bands(1:N__Band_Size(5),5) = &
!&    (/ 1189, 1197, 1205, 1213, 1221, 1251  /)
!<<orig
!>>emily

!       N__Bands(1:N__Band_Size(1),1) = &
!&     (/    24,   26,   28,   32,   37,   39,   42,   44,   47,   49, &
!&           51,   53,   55,   57,   59,   61,   63,   65,   67,   69, &
!&           71,   73,   75,   77,   79,   81,   83,   85,   87,   89, &
!&           91,   93,   95,   97,   99,  103,  105,  107,  109,  111, &
!&          113,  115,  117,  119,  121,  123,  125,  127,  129,  131, &
!&          133,  135,  137,  139,  141,  143,  145,  147,  149,  151, &
!&          153,  155,  157,  159,  163,  167,  171,  175,  179,  183, &
!&          187,  190,  194,  197,  200,  211,  224,  275,  279,  291, &
!&          311,  332,  342,  389,  410,  427,  464,  482,  501,  529, &
!&          710,  713,  742,  882,  890,  937,  995, 1008, 1022, 1058  /)

      N__Bands(1:N__Band_Size(1),1) = &
&     (/  24,   26,   28,   32,   37,   39,   42,   44,   47,   49, &
&         51,   53,   55,   57,   59,   61,   63,   65,   67,   69, &
&         71,   73,   75,   77,   79,   81,   83,   85,   87,   89, &
&         91,   93,   95,   97,   99,  103,  105,  107,  109,  111, &
&        113,  115,  117,  119,  121,  123,  125,  127,  129,  131, &
&        133,  135,  137,  139,  141,  143,  145,  147,  149,  151, &
&        153,  155,  157,  159,  163,  167,  171,  175,  179,  183, &
&        187,  190,  194,  197,  200,  211,  224,  275,  279,  291, &
&        311,  332,  342,  389,  410,  427,  464,  482,  501,  529, &
&        710 /)

      N__Bands(1:N__Band_Size(2),2) = &
&    (/    3,    6,    7,    8,   10,   12,   14,   15,   16,  102, &
&        104,  106,  108,  110,  114,  126,  127,  129,  132,  134, &
&        138,  140,  143,  144,  146,  147,  148,  150,  151,  153, &
&        155,  156,  158,  159,  162,  163,  164,  166,  170,  171, &
&        172,  175,  180,  189,  195,  200,  201,  206,  210,  214, &
&        217,  218,  226,  228,  230,  231,  233,  236,  240,  241, &
&        245,  248,  252,  264,  265,  281,  285,  297,  324,  327, &
&        473,  493,  500,  503,  511,  529,  534,  538,  542,  544, &
&        545,  547,  550,  553,  555,  589,  605,  621,  637,  653, &
&        669,  685  /)

      N__Bands(1:N__Band_Size(3),3) = &
&    (/  717,  725,  733,  741,  749,  757,  765,  773,  781,  789, &
&        797,  805,  813,  821,  829,  837,  845,  853,  861,  869, &
&        877,  885,  893,  901,  909,  917,  925,  933,  941,  949, &
&        957,  965,  973,  981,  989,  997, 1005, 1013, 1021, 1029, &
&       1037, 1045, 1053, 1061, 1069, 1077, 1085, 1093, 1101, 1109, &
&       1117, 1125, 1133, 1141  /)

      N__Bands(1:N__Band_Size(4),4) = &
&    (/ 1149, 1157, 1165, 1173, 1181 /)

      N__Bands(1:N__Band_Size(5),5) = &
&    (/ 1189, 1197, 1205, 1213, 1221, 1251  /)
!>>emily

      N__GradChkInterval(:) = 0
      N__GradChkInterval(1:N__Num_Bands) = (/ 5,5,5,3,3 /)

      N__Window_Width(:) = 0
      N__Window_Width(1:N__Num_Bands) = (/ 10,6,8,3,3 /)
      N__Window_Width(1) = 6    ! emily

      N__Window_Bounds(:,:) = 0
    ! N__Window_Bounds(1,1) = 229  !orig
    ! N__Window_Bounds(1,2) = 549  !orig
      N__Window_Bounds(1,1) = 224  !emily
      N__Window_Bounds(1,2) = 529  !emily

      R__BT_Threshold(:) = 0.
      R__BT_Threshold(1:N__Num_Bands) = (/ 0.5, 0.5, 0.5, 0.5, 0.5 /)
      R__BT_Threshold(1) = 0.3  ! emily

      R__Grad_Threshold(:) = 0.
      R__Grad_Threshold(1:N__Num_Bands) = (/ 0.02, 0.02, 0.02, 0.02, 0.02 /)

      R__Window_Grad_Threshold(:) = 0.
      R__Window_Grad_Threshold(1) = 0.4

      L__Do_Quick_Exit = .TRUE.


      ! This is cross-band:

      L__Do_CrossBand = .TRUE.

      N__BandToUse(:) = 0
      N__BandToUse(1:N__Num_Bands) = (/ 1,1,1,1,1 /)


      ! This is the setup for imager cloud detection

      L__Do_Imager_Cloud_Detection = .FALSE.

      N__Num_Imager_Chans = 0
      N__Num_Imager_Clusters = 0
      N__Imager_Chans(:) = 0

      R__Stddev_Threshold(:) = 0.0
      R__Coverage_Threshold = 0.0
      R__FG_Departure_Threshold = 0.0


   CASE(INST_ID_IRS)
      !====================
      ! Set up IRS
      !====================

      CL__InstrumentName='IRS'
      CL__Cloud_Detection_File = 'IRS_CLDDET.NL'

      N__Filter_Method = JP__Digital_Filter

      N__Num_Bands = 1

      N__Band_Size(:) = 0

      N__Band_Size(1:N__Num_Bands) =(/ 138 /)

      IF (MAXVAL(N__Band_Size(:)) > I__Max_Channels) &
&              CALL ABOR1('Too many channels specified in cloud '//&
&                      'detection - increase I__Max_Channels')

      N__Bands(:,:)= 0

      N__Bands(1:N__Band_Size(1),1) = &
&    (/    1,    2,    3,    4,    5,    6,    7,    8,    9,   10, &
&         11,   12,   13,   14,   15,   16,   17,   18,   19,   20, &
&         21,   22,   23,   24,   25,   26,   27,   28,   29,   30, &
&         31,   32,   33,   34,   35,   36,   37,   38,   39,   40, &
&         41,   42,   43,   44,   45,   46,   48,   53,   54,   55, &
&         56,   57,   58,   60,   61,   62,   63,   65,   70,   74, &
&         75,   76,   77,   78,   79,   80,   81,   82,   83,   84, &
&         85,   86,   87,   89,   90,   91,   92,   93,   94,   95, &
&         96,   97,   98,   99,  100,  101,  102,  103,  104,  105, &
&        106,  107,  108,  109,  118,  119,  131,  145,  163,  169, &
&        177,  180,  190,  195,  199,  209,  215,  221,  231,  237, &
&        252,  262,  268,  281,  289,  298,  312,  322,  328,  341, &
&        347,  359,  375,  384,  390,  404,  412,  421,  648,  656, &
&        667,  678,  686,  692,  709,  750,  792,  808 /)

      N__GradChkInterval(:) = 0
      N__GradChkInterval(1:N__Num_Bands) = (/ 12 /)

      N__Window_Width(:) = 0
      N__Window_Width(1:N__Num_Bands) = (/ 10 /)

      N__Window_Bounds(:,:) = 0
      N__Window_Bounds(1,1) = 131
      N__Window_Bounds(1,2) = 808

      R__BT_Threshold(:) = 0.
      R__BT_Threshold(1:N__Num_Bands) = (/ 0.4 /)

      R__Grad_Threshold(:) = 0.
      R__Grad_Threshold(1:N__Num_Bands) = (/ 0.02 /)

      R__Window_Grad_Threshold(:) = 0.
      R__Window_Grad_Threshold(1) = 0.4

      L__Do_Quick_Exit = .TRUE.


      ! This is cross-band:

      L__Do_CrossBand = .TRUE.

      N__BandToUse(:) = 0
      N__BandToUse(1:N__Num_Bands) = (/ 1 /)


      ! This is the setup for imager cloud detection

      L__Do_Imager_Cloud_Detection = .FALSE.

      N__Num_Imager_Chans = 0
      N__Num_Imager_Clusters = 0
      N__Imager_Chans(:) = 0

      R__Stddev_Threshold(:) = 0.0
      R__Coverage_Threshold = 0.0
      R__FG_Departure_Threshold = 0.0


   CASE(INST_ID_IASING)
      !====================
      ! Set up IASING
      !====================

      CL__InstrumentName='IASING'
      CL__Cloud_Detection_File = 'IASING_CLDDET.NL'

      N__Filter_Method = JP__Digital_Filter

      N__Num_Bands = 1

      N__Band_Size(:) = 0

      N__Band_Size(1:N__Num_Bands) =(/ 254 /)

      IF (MAXVAL(N__Band_Size(:)) > I__Max_Channels) &
&              CALL ABOR1('Too many channels specified in cloud '//&
&                      'detection - increase I__Max_Channels')

      N__Bands(:,:)= 0

      N__Bands(1:N__Band_Size(1),1) = &
&    (/   31,   75,   97,  101,  109,  113,  117,  121,  125,  131, &
&        139,  143,  147,  157,  161,  165,  169,  173,  177,  183, &
&        189,  193,  197,  201,  207,  211,  217,  221,  225,  231, &
&        237,  243,  249,  255,  261,  265,  269,  275,  281,  287, &
&        291,  295,  301,  307,  313,  317,  321,  325,  329,  333, &
&        339,  345,  351,  355,  357,  359,  365,  369,  373,  377, &
&        381,  385,  389,  393,  397,  401,  403,  405,  407,  409, &
&        411,  413,  415,  417,  419,  421,  423,  425,  427,  429, &
&        431,  433,  435,  437,  439,  441,  443,  445,  447,  449, &
&        451,  453,  455,  457,  459,  461,  463,  465,  467,  469, &
&        471,  473,  475,  477,  479,  481,  483,  485,  487,  489, &
&        491,  493,  495,  497,  499,  501,  503,  505,  507,  509, &
&        511,  513,  515,  517,  519,  521,  523,  525,  527,  529, &
&        531,  533,  535,  537,  539,  541,  543,  545,  547,  549, &
&        551,  553,  555,  557,  559,  561,  563,  565,  567,  569, &
&        571,  573,  575,  577,  579,  581,  583,  585,  587,  589, &
&        591,  593,  595,  597,  601,  603,  605,  607,  609,  611, &
&        613,  615,  617,  619,  621,  623,  625,  627,  629,  631, &
&        633,  635,  637,  639,  641,  643,  645,  647,  649,  651, &
&        653,  655,  657,  659,  661,  663,  665,  667,  669,  681, &
&        693,  699,  703,  707,  711,  715,  719,  723,  727,  731, &
&        737,  741,  745,  749,  753,  757,  761,  771,  777,  807, &
&        813,  819,  827,  831,  851,  855,  863,  867,  889,  913, &
&       1029, 1091, 1103, 1131, 1141, 1145, 1291, 1323, 1335, 1511, &
&       1733, 1841, 2053, 2179, 2265, 2381, 2387, 2541, 3609, 3767, &
&       3891, 3981, 4187, 4477 /)

      N__GradChkInterval(:) = 0
      N__GradChkInterval(1:N__Num_Bands) = (/ 25 /)

      N__Window_Width(:) = 0
      N__Window_Width(1:N__Num_Bands) = (/ 20 /)

      N__Window_Bounds(:,:) = 0
      N__Window_Bounds(1,1) = 1145
      N__Window_Bounds(1,2) = 4477

      R__BT_Threshold(:) = 0.
      R__BT_Threshold(1:N__Num_Bands) = (/ 0.27 /)

      R__Grad_Threshold(:) = 0.
      R__Grad_Threshold(1:N__Num_Bands) = (/ 0.02 /)

      R__Window_Grad_Threshold(:) = 0.
      R__Window_Grad_Threshold(1) = 0.4

      L__Do_Quick_Exit = .TRUE.


      ! This is cross-band:

      L__Do_CrossBand = .TRUE.

      N__BandToUse(:) = 0
      N__BandToUse(1:N__Num_Bands) = (/ 1 /)


      ! This is the setup for imager cloud detection

      L__Do_Imager_Cloud_Detection = .FALSE.

      N__Num_Imager_Chans = 0
      N__Num_Imager_Clusters = 0
      N__Imager_Chans(:) = 0

      R__Stddev_Threshold(:) = 0.0
      R__Coverage_Threshold = 0.0
      R__FG_Departure_Threshold = 0.0

   CASE DEFAULT
      CYCLE
   END SELECT

   !------------------------------------------------------------------
   ! Open and read file containing cloud detection setup for the
   ! current instrument
   !------------------------------------------------------------------
!>>orig:commented out
!   INIU1=10
!   WRITE(*,*)'READING CLOUD DETECTION FILE FOR ',TRIM(CL__InstrumentName)
!   OPEN(INIU1,STATUS='OLD',FORM='FORMATTED', &
!        FILE=TRIM(CL__Cloud_Detection_File), IOSTAT=IOS)
!   IF (IOS == 0) THEN
!      READ(INIU1,nml=Cloud_Detect_Coeffs,IOSTAT=IOS)
!      IF (IOS == 0) THEN
!         WRITE(*,*) TRIM(CL__InstrumentName),' CLOUD DETECTION FILE READ OK'
!      ELSE
!        CALL ABOR1('PROBLEM READING '//TRIM(CL__InstrumentName)//&
!&                'CLOUD DETECTION FILE')
!      ENDIF
!      CLOSE(INIU1)
!   ELSE
!      WRITE(*,*)'NO '//TRIM(CL__InstrumentName)//&
!&              ' CLOUD DETECTION FILE : Using Default Values'
!   ENDIF
!<<orig

   IF (MAXVAL(N__Band_Size(:)) > I__Max_Channels) &
&              CALL ABOR1('Too many channels specified in cloud '//&
&                      'detection - increase I__Max_Channels')


   M__Sensor = J__Sensor

   !------------------------------------------------------------------
   ! Set up the S__Cloud_Detect_Setup structure for current sensor
   !------------------------------------------------------------------

   S__Cloud_Detect_Setup(J__SENSOR) % M__SENSOR = M__Sensor

   S__Cloud_Detect_Setup(J__SENSOR) % N__Filter_Method = N__Filter_Method

   S__Cloud_Detect_Setup(J__SENSOR) % N__Num_Bands = N__Num_Bands

   ALLOCATE( S__Cloud_Detect_Setup(J__SENSOR) % N__Band_Size(N__Num_Bands) )

   S__Cloud_Detect_Setup(J__SENSOR) % N__Band_Size(:) = &
&             N__Band_Size(1:N__Num_Bands)

   ALLOCATE(S__Cloud_Detect_Setup(J__SENSOR) % N__Bands &
&            (MAXVAL(N__Band_Size(:)), N__Num_Bands))

   S__Cloud_Detect_Setup(J__SENSOR) % N__Bands(:,:) = 0

   DO J = 1, N__Num_Bands
      S__Cloud_Detect_Setup(J__SENSOR) % N__Bands(1:N__Band_Size(J),J) = &
&          N__Bands(1:N__Band_Size(J),J)
   ENDDO

   ALLOCATE( S__Cloud_Detect_Setup(J__SENSOR) % N__Window_Width(N__Num_Bands) )

   S__Cloud_Detect_Setup(J__SENSOR) % N__Window_Width(:) = &
&        N__Window_Width(1:N__Num_Bands)

   ALLOCATE( S__Cloud_Detect_Setup(J__SENSOR) % R__BT_Threshold(N__Num_Bands) )
   S__Cloud_Detect_Setup(J__SENSOR) % R__BT_Threshold(:) = &
&        R__BT_Threshold(1:N__Num_Bands)

   ALLOCATE(S__Cloud_Detect_Setup(J__SENSOR) % R__Grad_Threshold(N__Num_Bands))
   S__Cloud_Detect_Setup(J__SENSOR) % R__Grad_Threshold(:) = &
&        R__Grad_Threshold(1:N__Num_Bands)

   ALLOCATE(S__Cloud_Detect_Setup(J__SENSOR) % &
&        R__Window_Grad_Threshold(N__Num_Bands))
   S__Cloud_Detect_Setup(J__SENSOR) % R__Window_Grad_Threshold(:) = &
&        R__Window_Grad_Threshold(1:N__Num_Bands)

   ALLOCATE(S__Cloud_Detect_Setup(J__SENSOR) % N__GradChkInterval(N__Num_Bands))
   S__Cloud_Detect_Setup(J__SENSOR) % N__GradChkInterval(:) = &
&        N__GradChkInterval(1:N__Num_Bands)

   ALLOCATE(S__Cloud_Detect_Setup(J__SENSOR) % N__Window_Bounds(N__Num_Bands,2))
   S__Cloud_Detect_Setup(J__SENSOR) % N__Window_Bounds(:,:) = &
&        N__Window_Bounds(1:N__Num_Bands,:)

   S__Cloud_Detect_Setup(J__SENSOR) % L__Do_Quick_Exit = L__Do_Quick_Exit


   !-------------
   ! Cross Band
   !-------------

   S__Cloud_Detect_Setup(J__SENSOR) % L__Do_CrossBand = L__Do_CrossBand

   ALLOCATE( S__Cloud_Detect_Setup(J__SENSOR) % N__BandToUse(N__Num_Bands) )
   S__Cloud_Detect_Setup(J__SENSOR) % N__BandToUse(:) = &
&        N__BandToUse(1:N__Num_Bands)


   !-------------
   ! Imager cloud detection
   !-------------

   S__Cloud_Detect_Setup(J__SENSOR) % L__Do_Imager_Cloud_Detection = &
&        L__Do_Imager_Cloud_Detection

   S__Cloud_Detect_Setup(J__SENSOR) % N__Num_Imager_Chans = &
&        N__Num_Imager_Chans

   S__Cloud_Detect_Setup(J__SENSOR) % N__Num_Imager_Clusters = &
&        N__Num_Imager_Clusters

   ALLOCATE( S__Cloud_Detect_Setup(J__SENSOR) % &
&        N__Imager_Chans(N__Num_Imager_Chans))
   S__Cloud_Detect_Setup(J__SENSOR) % N__Imager_Chans(:) = &
&        N__Imager_Chans(1:N__Num_Imager_Chans)

   ALLOCATE( S__Cloud_Detect_Setup(J__SENSOR) % &
&        R__Stddev_Threshold(N__Num_Imager_Chans))
   S__Cloud_Detect_Setup(J__SENSOR) % R__Stddev_Threshold(:) = &
&        R__Stddev_Threshold(1:N__Num_Imager_Chans)

   S__Cloud_Detect_Setup(J__SENSOR) % R__Coverage_Threshold = &
&        R__Coverage_Threshold

   S__Cloud_Detect_Setup(J__SENSOR) % R__FG_Departure_Threshold = &
&        R__FG_Departure_Threshold

ENDDO SensorLoop

END SUBROUTINE CLOUD_DETECT_SETUP
