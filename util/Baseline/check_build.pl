#!/usr/bin/perl

  $baselineDir = "$NS/GSI-Baseline";
  chdir ($baselineDir);
  $hashcmd = 'git log --pretty=oneline | head -1 | awk \'{print $1}\'';
  $lasthash = `$hashcmd`;
  chop($lasthash);
  $updatecmd = "git remote update; git pull origin DA-FV3-IMPL-merged";
  system("$updatecmd");
  $newhash = `$hashcmd`;
  chop($newhash);

# if($newhash ne $lasthash) { 
  if($newhash eq $lasthash) { 
    # rebuild new master    
    # this is just for mars until it gets added to the master
#   $sedcmd = 'sed -i \'s/v\\[0-9/m\\[0-9/g\' CMakeLists.txt';
#   system($sedcmd);
    $buildcmd = "./ush/build_all_cmake.sh $baselineDir 0";
    system($buildcmd); 
    chdir ("$baselineDir/build");
    system("REND=2 ctest -j 14");
  }


