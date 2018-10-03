# CMake generated Testfile for 
# Source directory: /scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI
# Build directory: /scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/build_efsoi
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
ADD_TEST(global_T62 "regression_driver.sh" "global_T62" "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/build_efsoi")
SET_TESTS_PROPERTIES(global_T62 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/regression")
ADD_TEST(global_T62_ozonly "regression_driver.sh" "global_T62_ozonly" "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/build_efsoi")
SET_TESTS_PROPERTIES(global_T62_ozonly PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/regression")
ADD_TEST(global_4dvar_T62 "regression_driver.sh" "global_4dvar_T62" "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/build_efsoi")
SET_TESTS_PROPERTIES(global_4dvar_T62 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/regression")
ADD_TEST(global_4denvar_T126 "regression_driver.sh" "global_4denvar_T126" "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/build_efsoi")
SET_TESTS_PROPERTIES(global_4denvar_T126 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/regression")
ADD_TEST(global_lanczos_T62 "regression_driver.sh" "global_lanczos_T62" "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/build_efsoi")
SET_TESTS_PROPERTIES(global_lanczos_T62 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/regression")
ADD_TEST(arw_netcdf "regression_driver.sh" "arw_netcdf" "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/build_efsoi")
SET_TESTS_PROPERTIES(arw_netcdf PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/regression")
ADD_TEST(
          arw_binary "regression_driver.sh" "
          arw_binary" "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/build_efsoi")
SET_TESTS_PROPERTIES(
          arw_binary PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/regression")
ADD_TEST(nmm_binary "regression_driver.sh" "nmm_binary" "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/build_efsoi")
SET_TESTS_PROPERTIES(nmm_binary PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/regression")
ADD_TEST(nmm_netcdf "regression_driver.sh" "nmm_netcdf" "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/build_efsoi")
SET_TESTS_PROPERTIES(nmm_netcdf PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/regression")
ADD_TEST(nmmb_nems_4denvar "regression_driver.sh" "nmmb_nems_4denvar" "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/build_efsoi")
SET_TESTS_PROPERTIES(nmmb_nems_4denvar PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/regression")
ADD_TEST(hwrf_nmm_d2 "regression_driver.sh" "hwrf_nmm_d2" "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/build_efsoi")
SET_TESTS_PROPERTIES(hwrf_nmm_d2 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/regression")
ADD_TEST(hwrf_nmm_d3 "regression_driver.sh" "hwrf_nmm_d3" "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/build_efsoi")
SET_TESTS_PROPERTIES(hwrf_nmm_d3 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/regression")
ADD_TEST(rtma "regression_driver.sh" "rtma" "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/build_efsoi")
SET_TESTS_PROPERTIES(rtma PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/regression")
ADD_TEST(global_enkf_T62 "regression_driver.sh" "global_enkf_T62" "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/build_efsoi")
SET_TESTS_PROPERTIES(global_enkf_T62 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/regression")
ADD_TEST(netcdf_fv3_regional "regression_driver.sh" "netcdf_fv3_regional" "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/build_efsoi")
SET_TESTS_PROPERTIES(netcdf_fv3_regional PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/version_revised_fv3efsoi/ProdGSI/regression")
SUBDIRS(libsrc/wrflib)
SUBDIRS(libsrc/ncdiag)
SUBDIRS(src)
SUBDIRS(src/enkf)
SUBDIRS(util/EFSOI_Utilities/src)
SUBDIRS(util/ndate)
