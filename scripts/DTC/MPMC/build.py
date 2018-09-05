#!/usr/bin/python3
#the first line does not matter, it will be replaced by running "python initMPMC.py"
#########################################################
import os, sys, stat
import os.path
import shutil
import subprocess
import time

#------------------BEGINNING OF FUNCTION CONFIRM SUCCESSFUL BUILD------------------------------
def confirm_successful_build( build_directory_name ):
# ---------------------------------------------------------------------------------------------
# Checks the bin directory in the specified build directory for executables 'enkf_gfs.x' & 'gsi.x'
# NCEP library files, and reports back if successful/fails
# Arguments
#    build_directory_name: full path of each build directory. 
# Returns 
#    String with either success or failure
# ---------------------------------------------------------------------------------------------

   print('#############################################')
   print('Test directory name >' + build_directory_name )
   executables = [ '/bin/enkf_gfs.x', '/bin/gsi.x', '/bin/nc_diag_cat.x',    \
                   '/bin/test_nc_unlimdims.x' ]
   return_code = ""

   path = build_directory_name + "/Makefile"
   if os.path.isfile(path) and os.access(path, os.R_OK):
      print(path + " exists and is readable")
   else:
      print("Either "+ path + " is missing or not readable")
      return_code = "CMKFAIL"
      return return_code

   count = 0
   for exe in executables:
      path = build_directory_name + exe
      if os.path.isfile(path) and os.access(path, os.R_OK):
         print("File " + exe + " exists and is readable")
         count = count + 1
      else:
         print("Either the file " + exe + " is missing or not readable")
         return_code = "EXEFAIL"

   if count == len( executables ):
      return_code = "PASS   "

   return return_code

#------------------END OF FUNCTION CONFIRM SUCCESSFUL BUILD------------------------------------

#------------------BEGINNING OF FUNCTION FORWARD SLASH-----------------------------------------
def remove_forwardslash( module_name_string ):
# ---------------------------------------------------------------------------------------------
# Remove a single forward-slash from the module name, fails if there is more than one.
# For Example: 'impi/2017.1.132' becomes 'impi2017.1.132'
# Arguments
#    module_name_string: original string with forward-slash
# Return
#    string without forward-slash
# ---------------------------------------------------------------------------------------------
   indx=module_name_string.find('/')
   inde=len( module_name_string )
   return  module_name_string[0:indx] +'_'+ module_name_string[indx+1:inde]

#------------------END OF FUNCTION FORWARD SLASH-----------------------------------------------

#------------------BEGINNING OF FUNCTION BUILD COMPILE SCRIPT----------------------------------
def build_compile_script (configstate, gsi_src_directory, build_directory_location,           \
                          build_directory_root, batch_queue_pre, batch_queue_varaibles,       \
                          batch_queue_module_pre, cmake_version ):
# ---------------------------------------------------------------------------------------------
# For each compile configuration in the setup file pyrunconfig
# 1. Create a build directory, named according to the compiler and version of mpi, at location
#    build_directory_location.
# 2. In each directory, generate a batch queue shell script that loads the appropriate modules 
#    calls CMAKE, and then runs a parallel MAKE
# 3. Submit the build script to the computer's batch queue
# Arguments
#    configstate: Object with the compile environmental state as read from the file pyrunconfig
#    gsi_src_directory: Location of the GSI source code
#    build_directory_location: Base path of the build directory, default is in current directory
#    build_directory_root: Location of these python scripts
#    batch_queue_pre, batch_queue_varaibles, batch_queue_module_pre: Strings with batch queue
#           commands specific to the current computing platform
#    cmake_version: Module name for the version of cmake to be used
# Return 
#    build_directory_name
# ---------------------------------------------------------------------------------------------

# Default location for source and build directory
   current_directory = os.getcwd() 
# Extract module information from configure file
   
   config_id = configstate['id']

   compiler_module_name = configstate['comp']
   compiler_name = remove_forwardslash( compiler_module_name )

   compiler_post_module_name = configstate['comp_post']

   mpi_module_name = configstate['mpi']
   mpi_name = remove_forwardslash( mpi_module_name )

   lapack_module_name = configstate['lapack']
   lapack_name = remove_forwardslash( lapack_module_name )

   netcdf_module_name = configstate['netcdf']
   netcdf_name = remove_forwardslash( netcdf_module_name )

   modules_to_load_compiler = cmake_version + ' ' + compiler_module_name + ' ' +              \
                              compiler_post_module_name + ' ' + lapack_module_name
   modules_to_load_mpi = mpi_module_name
   modules_to_load_netcdf = netcdf_module_name

   build_script_name = 'compile.sh'
# If these are not defined, use current working directory.  
   if not build_directory_root:
      build_directory_root = current_directory

   if not build_directory_location:
      build_directory_location = current_directory

   if not gsi_src_directory:
      gsi_src_directory = current_directory

# If the build directory path is nontrivial, add a trailing forward slash if not already there. 
   if build_directory_location[ len(build_directory_location)-1 ] != '/':
      build_directory_location = build_directory_location + '/'

# Determine if the root and source code directory paths are valid
   returncode = os.path.isdir( gsi_src_directory )
   if not returncode:
      print("Log Error: GSI source code directory is not valid")
      return 1

   returncode = os.path.isdir( build_directory_root )
   if not returncode:
      print("Log Error: Root directory location " + build_directory_root + " is not valid")
      return 1

# Determine if the build location path is valid
   returncode = os.path.isdir( build_directory_location )
   if not returncode:
      print("WARNING: Build directory location " + build_directory_location +                  \
                  " does not exist, lets try to make it") 
      retcode = subprocess.call(["mkdir", build_directory_location ])
      if retcode !=0:
         print ("Log Error: Build directory "+ build_directory_location +" failed to be created")
         return 1

# If the base build directory is a valid location, use it
   build_directory_name = build_directory_location + compiler_name + '_' + mpi_name + '_' + config_id

# Check to see if the new build directory name already exists. If it does, rename it and create a fresh one
   returncode = os.path.isdir(build_directory_name)
   if returncode != 0:
      if (not force):
         print("Name conflict, directory " + build_directory_name +'\nalready exists, please rename it or build to another directory\n'+
         "OR you may use 'build.py force' to force to remove the old directory\n")
         exit()
         old_build_directory_name = build_directory_name+".old"

         returncode_old = os.path.isdir(old_build_directory_name)
         if returncode_old != 0:
            shutil.rmtree(old_build_directory_name)

         os.rename(build_directory_name, old_build_directory_name)
      else:
         shutil.rmtree(build_directory_name)

      returncode = os.path.isdir(build_directory_name)
      if returncode !=0:
         print ("DEBUG: humm, the rename failed")
         print ("Log Error: Unable to rename old build directory:" +build_directory_name)
         return 1

   if returncode == 0:
      retcode = subprocess.call(["mkdir", build_directory_name ])
      if retcode !=0:
         print ("Log Error: Directory "+ build_directory_name +" failed to be created")
         return 1

   if lapack_name:
      report_config.append( ' ==>| ' + config_id.ljust(4) + ' | ' + compiler_name.ljust(18) + ' | ' + mpi_name.ljust(16) \
                          + ' | ' + netcdf_name.ljust(15) + ' | ' + lapack_name.ljust(17) + ' |')
   else:
      report_config.append( ' ==>| ' + config_id.ljust(4) + ' | ' + compiler_name.ljust(18) + ' | ' + mpi_name.ljust(16) \
                          + ' | ' + netcdf_name.ljust(15) + ' | NA |')


# Construct a batch queue script, in the build directory, for completing the compile
   file = open(build_directory_name + '/' + build_script_name, "w")
   file.write('#!/bin/sh \n#----------------BUILD SCRIPT-----------------\n' )
   if lapack_name:
      local_lapack_name = ' and ' + lapack_name
   else:
      local_lapack_name = ''
   file.write('# Compiler='+compiler_name+' with '+mpi_name+ ' and ' + netcdf_name +           \
               local_lapack_name+'\n')
   job_name = compiler_name + '_' + mpi_name
   file.write( batch_queue_pre )
   file.write(' job_' + job_name + '\n')
   file.write( batch_queue_varaibles )
   file.write( batch_queue_module_pre )
#  file.write('module purge \n') ## put it in pyrunconfig.py_* to accomodate Jet's special need to load newdefaults first
   file.write('module load ' + modules_to_load_compiler +'\n')
   file.write('module load ' + modules_to_load_mpi +'\n')
   file.write('module load ' + modules_to_load_netcdf +'\n\n')
   file.write('cd ' + build_directory_name + '\n\n')
   CMakeExtras=''
   file.write('cmake '+CMakeExtras+'-DBUILD_CORELIBS=ON -DCMAKE_C_COMPILER='+ configstate['cc'] +            \
              ' -DCMAKE_CXX_COMPILER=' + configstate['cxx'] + ' ' + gsi_src_directory + ' \n')
   file.write('make -j8 \n')
   file.write('make -j2 \n\n')  #
   file.write('rm -rf CMakeCache.txt CMakeFiles \n')
#  file.write('rm -rf CMakeCache.txt CMakeFiles DartConfiguration.tcl include lib libsrc src Makefile util \n')
   file.write('cmake '+CMakeExtras+'-DBUILD_WRF=ON -DBUILD_CORELIBS=ON -DCMAKE_C_COMPILER='+ configstate['cc'] + \
              ' -DCMAKE_CXX_COMPILER=' + configstate['cxx'] + ' ' + gsi_src_directory + ' \n')
   file.write('make -j8 \n')
   file.write('make -j2 \n\n')  #

   file.close() 

# Make the script executable
   os.system("chmod +x "+build_directory_name + '/' + build_script_name)

# Change into build_directory_location and submit the build script to the batch queue
   if not dry_mode:
      subprocess.call("cd " + build_directory_name + "; qsub " + build_directory_name + "/" + \
                       build_script_name,shell=True)
#
   return build_directory_name 

#------------------------END OF FUNCTION BUILD COMPILE SCRIPT----------------------------------

#--------------------------------BEGIN MAIN PROGRAM--------------------------------------------
import socket
hostname=socket.gethostname()
if hostname.find("tfe")>=0:  ## THEIA
  os.system("ln -sf pyrunconfig.py_theia pyrunconfig.py")
elif hostname.find("fe")>=0: ## Jet
  os.system("ln -sf pyrunconfig.py_jet pyrunconfig.py")
elif hostname.find("cheyenne")>=0:  ## Cheyenne
  os.system("ln -sf pyrunconfig.py_cheyenne pyrunconfig.py")
else:
  print("I'm new to Host: "+hostname+". Please create a pyrunconfig.py file for me")
  exit()

from datetime import datetime
from pyrunconfig import cfg, machine_name,platform_username, cmake_version, gsi_src_directory, \
                 build_directory_location, build_directory_root, batch_queue_pre,              \
                 batch_queue_varaibles, batch_queue_module_pre, create_build_report, dry_mode

report_config = [ ]
if (sys.version_info < (3, 0)):
   print('You are trying to run this program with python2')
   print('You must run this program with python v3')
   print('Current python version is:'+str(sys.version_info.major)+'.'+str(sys.version_info.minor))
   sys.exit()

arguments = len(sys.argv) - 1
force=False
if (arguments >=1):
   if (sys.argv[1] == "force" ):
      force=True

def main():
#
   test_name = [ ]
   test_state = [ ]

   for configstate in cfg:
      test_name.append( build_compile_script ( configstate, gsi_src_directory,               \
                                      build_directory_location, build_directory_root,        \
                                      batch_queue_pre, batch_queue_varaibles,                \
                                      batch_queue_module_pre, cmake_version ) )

   test_directory_list_filename = "list." + datetime.now().strftime("%Y%m%d_%H%M%S")
   file_report = open(test_directory_list_filename, "w")
   for position in range(len(test_name)):
      file_report.write( test_name[position] + '\n')
   file_report.close() 

main()
