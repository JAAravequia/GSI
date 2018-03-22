# This extends CMake's FindHDF5.cmake to add support to include MPI include
# paths and libraries in the HDF5 ones if HDF5_IS_PARALLEL is ON
# (BUG #0014363).

# include the default FindHDF5.cmake.
if(CMAKE_VERSION VERSION_LESS 3.1)
  include(${CMAKE_ROOT}/Modules/FindMPI.cmake)
elseif(CMAKE_VERSION VERSION_LESS 3.10)
  message("hey, including new FindMPI")
  message("hey, cmake version is ${CMAKE_VERSION}")
  include(${CMAKE_CURRENT_LIST_DIR}/NewCMake/FindMPI.cmake)
else()
  include(${CMAKE_ROOT}/Modules/FindMPI.cmake)
endif()

