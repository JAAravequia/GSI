#!/usr/bin/perl

  @list = split(/\n/,`cat @ARGV`);
  foreach(@list) {
    $file = $_;
    $outfilename = "new/".$file;
    open($outfile, ">$outfilename");
    @words = split(/\./,$file);
    $subname = @words[0];
    @words = split(/setup/,$subname);
    $tailname = @words[1];
    @lines = split(/\n/,`cat $file`);
    $class_in = "      class($subname"."_class)                              , intent(inout) :: this"; 
    $outline1 = "module ".$subname."_mod\n"; 
    print $outfile $outline1;
    $outline1 = "use abstract_setup_mod\n";
    print $outfile $outline1;
    $outline1 = "  type, extends(abstract_setup_class) :: ".$subname."_class\n";
    print $outfile $outline1;
    print $outfile "  contains\n";
    print $outfile "    procedure, pass(this) :: setup => $subname\n";
    print $outfile "    procedure, pass(this) :: init_vars_derived => init_vars_$tailname\n";
    print $outfile "    procedure, pass(this) :: final_vars_$tailname\n";
    print $outfile "    procedure, pass(this) :: check_vars_$tailname\n";
    $outline1 = "  end type ".$subname."_class\n";
    print $outfile $outline1;
    print $outfile "contains\n";
    
    $space = " ";
    foreach(@lines) {
      $line = $_;
      if(($line =~ /lunin,mype/)&&($line =~ /subroutine/)) {
        $line =~ s/lunin/this,lunin/;
        print $outfile "$space $line\n"
      } elsif($line =~ /passed variables/) {
        $line = $line."\n".$class_in;
        print $outfile "$space $line\n"
      } elsif($line =~ /contains/) {
        $space = "";
        print $outfile "end subroutine $subname\n";
      } else {
        if($line =~ /end subroutine $subname/) {
          $outline1 = "end module ".$subname."_mod\n"; 
          print $outfile $outline1;
        } elsif(($line =~ /_vars_/)&&($line =~ /  subroutine/)) {
          if($line =~ /proceed/) {
            $line =~ s/proceed/this,proceed/;
            $line =~ s/ \(/\(/;
            $line =~ s/\(/$tailname\(/; 
            $line = $line."\n      implicit none\n$class_in";
          } else {
            $line = $line.$tailname."(this)\n      implicit none\n$class_in";
          }
          print $outfile "$space $line\n";
        } else {
          print $outfile "$space $line\n"
        }
      }
    }
    close($outfile);
  }

