regtest=$1

case $machine in

	Theia)
	   sub_cmd="sub_zeus"
    ;;
	WCOSS)
	   sub_cmd="sub_wcoss -a GDAS-T2O -d $PWD"
    ;;
	WCOSS_C)
	   sub_cmd="sub_wcoss_c -a GDAS-T2O -d $PWD"
    ;;
	WCOSS_D)
	   sub_cmd="sub_wcoss_d -a ibm -d $PWD"
    ;;
	discover)
	   sub_cmd="sub_discover"
    ;;
	s4)
	   sub_cmd="sub_s4"
    ;;
	Cheyenne)
	   sub_cmd="sub_ncar -a p48503002 -q economy -d $PWD"
    ;;
    *) # EXIT out for unresolved machine
        echo "unknown $machine"
        exit 1

esac

case $regtest in

    global_T62)

        if [[ "$machine" = "Theia" ]]; then
           topts[1]="0:30:00" ; popts[1]="12/3/" ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="12/9/" ; ropts[2]="/2"
        elif [[ "$machine" = "Cheyenne" ]]; then
           topts[1]="0:30:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS" ]]; then
           topts[1]="0:30:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_C" ]]; then
           topts[1]="0:30:00" ; popts[1]="36/4/" ; ropts[1]="1024/1"  # sub_wcoss_c popts are "#tasks/#nodes/"
           topts[2]="0:30:00" ; popts[2]="72/8/" ; ropts[2]="1024/2" 
        elif [[ "$machine" = "WCOSS_D" ]]; then
           topts[1]="0:30:00" ; popts[1]="28/2/" ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="28/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "s4" ]]; then
           topts[1]="1:45:00" ; popts[1]="20/4" ; ropts[1]="/1"
           topts[2]="1:45:00" ; popts[2]="40/2" ; ropts[2]="/2"
        elif [[ "$machine" = "discover" ]]; then
           topts[1]="0:30:00" ; popts[1]="36/2"  ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="72/3"  ; ropts[2]="/2"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:45:00"
        fi

        scaling[1]=10; scaling[2]=8; scaling[3]=4

    ;;

    global_T62_ozonly)

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:15:00" ; popts[1]="12/1/" ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="12/3/" ; ropts[2]="/2"
        elif [[ "$machine" = "Cheyenne" ]]; then
            topts[1]="0:15:00" ; popts[1]="16/1/" ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="16/2/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:15:00" ; popts[1]="16/1/" ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="16/2/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_C" ]]; then
            topts[1]="0:15:00" ; popts[1]="16/1/" ; ropts[1]="1024/1"  # sub_wcoss_c popts are "#tasks/#nodes/"
            topts[2]="0:15:00" ; popts[2]="12/2/" ; ropts[2]="1024/2"
        elif [[ "$machine" = "WCOSS_D" ]]; then
           topts[1]="0:15:00" ; popts[1]="28/2/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="28/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "discover" ]]; then
           topts[1]="0:30:00" ; popts[1]="16/2"  ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="16/1"  ; ropts[2]="/2"
        elif [[ "$machine" = "s4" ]]; then
           topts[1]="0:25:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:25:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:45:00"
        fi

        scaling[1]=10; scaling[2]=8; scaling[3]=4

    ;;

    global_4dvar_T62)

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:15:00" ; popts[1]="12/3/" ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="12/5/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:35:00" ; popts[1]="16/2/" ; ropts[1]="/1"
            topts[2]="0:25:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "Cheyenne" ]]; then
            topts[1]="0:35:00" ; popts[1]="16/2/" ; ropts[1]="/1"
            topts[2]="0:25:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_C" ]]; then
            topts[1]="1:35:00" ; popts[1]="48/12/" ; ropts[1]="1024/1"  # sub_wcoss_c popts are "#tasks/#nodes/"
            topts[2]="1:25:00" ; popts[2]="60/15/" ; ropts[2]="1024/2"
        elif [[ "$machine" = "s4" ]]; then
            topts[1]="0:55:00" ; popts[1]="16/2/" ; ropts[1]="/1"
            topts[2]="0:45:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_D" ]]; then
           topts[1]="0:15:00" ; popts[1]="28/2/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="28/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "discover" ]]; then
            topts[1]="2:00:00" ; popts[1]="48/2"  ; ropts[1]="/1"
            topts[2]="2:00:00" ; popts[2]="60/3"  ; ropts[2]="/2"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:45:00"
           if [[ "$machine" = "Theia" ]]; then
              popts[1]="12/5/"
           elif [[ "$machine" = "WCOSS" ]]; then
              popts[1]="16/4/"
           elif [[ "$machine" = "WCOSS_C" ]]; then
              popts[1]="48/12/"
              topts[1]="3:00:00"
           elif [[ "$machine" = "WCOSS_D" ]]; then
              popts[1]="56/14/"
              topts[1]="3:00:00"
           fi
        fi

        scaling[1]=5; scaling[2]=8; scaling[3]=2

    ;;

    global_hybrid_T126)

        if [[ "$machine" = "Theia" ]]; then
           topts[1]="0:15:00" ; popts[1]="12/3/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="12/5/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS" ]]; then
           topts[1]="0:15:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "Cheyenne" ]]; then
           topts[1]="0:15:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_C" ]]; then
           topts[1]="0:15:00" ; popts[1]="48/8/" ; ropts[1]="1024/1"  # sub_wcoss_c popts are "#tasks/#nodes/"
           topts[2]="0:15:00" ; popts[2]="60/10/" ; ropts[2]="1024/2"
        elif [[ "$machine" = "s4" ]]; then
           topts[1]="0:25:00" ; popts[1]="20/4/" ; ropts[1]="/1"
           topts[2]="0:25:00" ; popts[2]="40/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_D" ]]; then
           topts[1]="0:15:00" ; popts[1]="28/2/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="28/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "discover" ]]; then
           topts[1]="0:30:00" ; popts[1]="48/2"  ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="60/3"  ; ropts[2]="/2"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:45:00"
        fi

        scaling[1]=10; scaling[2]=8; scaling[3]=4

    ;;

    global_4denvar_T126)

        if [[ "$machine" = "Theia" ]]; then
           topts[1]="0:15:00" ; popts[1]="6/8/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="6/10/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS" ]]; then
           topts[1]="1:59:00" ; popts[1]="6/8/" ; ropts[1]="/1"
           topts[2]="0:35:00" ; popts[2]="6/10/" ; ropts[2]="/2"
        elif [[ "$machine" = "discover" ]]; then
           topts[1]="0:30:00" ; popts[1]="48/2"  ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="60/3"  ; ropts[2]="/2"
        elif [[ "$machine" = "Cheyenne" ]]; then
           topts[1]="1:59:00" ; popts[1]="6/8/" ; ropts[1]="/1"
           topts[2]="0:35:00" ; popts[2]="6/10/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_C" ]]; then
           topts[1]="0:35:00" ; popts[1]="48/8/" ; ropts[1]="1024/1"  # sub_wcoss_c popts are "#tasks/#nodes/"
           topts[2]="0:35:00" ; popts[2]="60/10/" ; ropts[2]="1024/2"
        elif [[ "$machine" = "WCOSS_D" ]]; then
           topts[1]="0:15:00" ; popts[1]="6/8/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="6/10/" ; ropts[2]="/2"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="1:30:00"
        fi

        scaling[1]=10; scaling[2]=8; scaling[3]=4

    ;;

    global_lanczos_T62)

        if [[ "$machine" = "Theia" ]]; then
           topts[1]="0:20:00" ; popts[1]="12/3/" ; ropts[1]="/1"
           topts[2]="0:20:00" ; popts[2]="12/5/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS" ]]; then
           topts[1]="0:20:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:20:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "Cheyenne" ]]; then
           topts[1]="0:20:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:20:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_C" ]]; then
           topts[1]="0:20:00" ; popts[1]="48/8/" ; ropts[1]="1024/1"  # sub_wcoss_c popts are "#tasks/#nodes/"
           topts[2]="0:20:00" ; popts[2]="60/10/" ; ropts[2]="1024/2"
        elif [[ "$machine" = "WCOSS_D" ]]; then
           topts[1]="0:20:00" ; popts[1]="28/2/" ; ropts[1]="/1"
           topts[2]="0:20:00" ; popts[2]="28/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "discover" ]]; then
           topts[1]="0:30:00" ; popts[1]="48/2"  ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="60/3"  ; ropts[2]="/2"
        elif [[ "$machine" = "s4" ]]; then
           topts[1]="0:30:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:45:00"
        fi

        scaling[1]=10; scaling[2]=8; scaling[3]=4

    ;;

    global_nemsio_T62)

        if [[ "$machine" = "Theia" ]]; then
           topts[1]="0:15:00" ; popts[1]="12/3/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="12/9/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS" ]]; then
           topts[1]="0:15:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "Cheyenne" ]]; then
           topts[1]="0:15:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_C" ]]; then
           topts[1]="0:30:00" ; popts[1]="48/8/" ; ropts[1]="1024/1"  # sub_wcoss_c popts are "#tasks/#nodes/"
           topts[2]="0:30:00" ; popts[2]="60/10/" ; ropts[2]="1024/2"
        elif [[ "$machine" = "s4" ]]; then
           topts[1]="0:25:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:25:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_D" ]]; then
           topts[1]="0:15:00" ; popts[1]="28/2/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="28/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "discover" ]]; then
           topts[1]="0:30:00" ; popts[1]="48/2"  ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="60/3"  ; ropts[2]="/2"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:45:00"
        fi

        scaling[1]=10; scaling[2]=8; scaling[3]=4

    ;;

    arw_binary | arw_netcdf)

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:15:00" ; popts[1]="4/4/"  ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="6/6/"  ; ropts[2]="/1"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:15:00" ; popts[1]="16/1/" ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="16/2/" ; ropts[2]="/1"
        elif [[ "$machine" = "Cheyenne" ]]; then
            topts[1]="0:15:00" ; popts[1]="16/1/" ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="16/2/" ; ropts[2]="/1"
        elif [[ "$machine" = "WCOSS_C" ]]; then
            topts[1]="0:15:00" ; popts[1]="20/2/" ; ropts[1]="1024/1"  # sub_wcoss_c popts are "#tasks/#nodes/"
            topts[2]="0:15:00" ; popts[2]="16/4/" ; ropts[2]="1024/1"
        elif [[ "$machine" = "WCOSS_D" ]]; then
           topts[1]="0:15:00" ; popts[1]="28/1/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="28/2/" ; ropts[2]="/2"
        elif [[ "$machine" = "discover" ]]; then
           topts[1]="0:30:00" ; popts[1]="16/1"  ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="20/2"  ; ropts[2]="/2"
        elif [[ "$machine" = "s4" ]]; then
           topts[1]="0:25:00" ; popts[1]="16/1/" ; ropts[1]="/1"
           topts[2]="0:25:00" ; popts[2]="16/2/" ; ropts[2]="/1"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:30:00"
        fi

        scaling[1]=4; scaling[2]=10; scaling[3]=4

    ;;

    nmm_binary )

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:30:00" ; popts[1]="6/6/"  ; ropts[1]="/1"
            topts[2]="0:30:00" ; popts[2]="8/8/"  ; ropts[2]="/1"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:30:00" ; popts[1]="7/12/" ; ropts[1]="/1"
            topts[2]="0:30:00" ; popts[2]="9/12/" ; ropts[2]="/2"
        elif [[ "$machine" = "Cheyenne" ]]; then
            topts[1]="0:30:00" ; popts[1]="7/12/" ; ropts[1]="/1"
            topts[2]="0:30:00" ; popts[2]="9/12/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_C" ]]; then
            topts[1]="0:30:00" ; popts[1]="48/8/" ; ropts[1]="1024/1"  # sub_wcoss_c popts are "#tasks/#nodes/"
            topts[2]="0:30:00" ; popts[2]="60/10/" ; ropts[2]="1024/2"
        elif [[ "$machine" = "s4" ]]; then
            topts[1]="0:50:00" ; popts[1]="7/12/" ; ropts[1]="/1"
            topts[2]="0:50:00" ; popts[2]="9/12/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_D" ]]; then
           topts[1]="0:30:00" ; popts[1]="7/24/" ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="9/24/" ; ropts[2]="/2"
        elif [[ "$machine" = "discover" ]]; then
            topts[1]="0:30:00" ; popts[1]="48/2"  ; ropts[1]="/1"
            topts[2]="0:30:00" ; popts[2]="60/3"  ; ropts[2]="/2"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="1:00:00"
        fi

        scaling[1]=8; scaling[2]=10; scaling[3]=8

    ;;

    nmm_netcdf)

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:15:00" ; popts[1]="4/2/"  ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="4/4/"  ; ropts[2]="/1"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:15:00" ; popts[1]="8/1/"  ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="16/1/" ; ropts[2]="/2"
        elif [[ "$machine" = "Cheyenne" ]]; then
            topts[1]="0:15:00" ; popts[1]="8/1/"  ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="16/1/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_C" ]]; then
            topts[1]="0:15:00" ; popts[1]="8/2/"  ; ropts[1]="1024/1"  # sub_wcoss_c popts are "#tasks/#nodes/"
            topts[2]="0:15:00" ; popts[2]="16/4/" ; ropts[2]="1024/2"
        elif [[ "$machine" = "s4" ]]; then
            topts[1]="0:25:00" ; popts[1]="8/1/"  ; ropts[1]="/1"
            topts[2]="0:25:00" ; popts[2]="16/1/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_D" ]]; then
           topts[1]="0:15:00" ; popts[1]="14/1/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="28/2/" ; ropts[2]="/2"
        elif [[ "$machine" = "discover" ]]; then
            topts[1]="0:30:00" ; popts[1]="8/1"  ; ropts[1]="/1"
            topts[2]="0:30:00" ; popts[2]="16/1"  ; ropts[2]="/2"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:30:00"
        fi

        scaling[1]=5; scaling[2]=10; scaling[3]=2

;;

    nmmb_nems_4denvar)

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:30:00" ; popts[1]="7/10/"  ; ropts[1]="/1"
            topts[2]="0:30:00" ; popts[2]="9/10/"  ; ropts[2]="/1"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:30:00" ; popts[1]="7/10/" ; ropts[1]="/1"
            topts[2]="0:30:00" ; popts[2]="9/10/" ; ropts[2]="/2"
        elif [[ "$machine" = "Cheyenne" ]]; then
            topts[1]="0:30:00" ; popts[1]="7/10/" ; ropts[1]="/1"
            topts[2]="0:30:00" ; popts[2]="9/10/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_C" ]]; then
            topts[1]="1:30:00" ; popts[1]="72/9/" ; ropts[1]="1024/1"  # sub_wcoss_c popts are "#tasks/#nodes/"
            topts[2]="1:30:00" ; popts[2]="96/12/" ; ropts[2]="1024/2"
        elif [[ "$machine" = "s4" ]]; then
            topts[1]="0:50:00" ; popts[1]="7/10/" ; ropts[1]="/1"
            topts[2]="0:50:00" ; popts[2]="9/10/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_D" ]]; then
           topts[1]="0:15:00" ; popts[1]="7/14/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="9/14/" ; ropts[2]="/2"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="1:00:00"
        fi

        scaling[1]=8; scaling[2]=10; scaling[3]=8

;;

    rtma)

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:30:00" ; popts[1]="6/12/"  ; ropts[1]="/1"
            topts[2]="0:30:00" ; popts[2]="8/12/"  ; ropts[2]="/1"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:15:00" ; popts[1]="8/6/"  ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="8/8/"  ; ropts[2]="/1"
        elif [[ "$machine" = "Cheyenne" ]]; then
            topts[1]="0:15:00" ; popts[1]="8/6/"  ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="8/8/"  ; ropts[2]="/1"
        elif [[ "$machine" = "WCOSS_C" ]]; then
            topts[1]="1:15:00" ; popts[1]="48/6/"  ; ropts[1]="1024/1"  # sub_wcoss_c popts are "#tasks/#nodes/"
            topts[2]="1:15:00" ; popts[2]="64/8/"  ; ropts[2]="1024/1"
        elif [[ "$machine" = "s4" ]]; then
            topts[1]="0:25:00" ; popts[1]="8/6/"  ; ropts[1]="/1"
            topts[2]="0:25:00" ; popts[2]="8/8/"  ; ropts[2]="/1"
        elif [[ "$machine" = "WCOSS_D" ]]; then
           topts[1]="2:30:00" ; popts[1]="14/8/" ; ropts[1]="/1"
           topts[2]="2:30:00" ; popts[2]="14/14/" ; ropts[2]="/2"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="3:00:00"
           if [[ "$machine" = "WCOSS_C" ]]; then
               popts[1]="64/8/"
           fi
        fi

        scaling[1]=10; scaling[2]=10; scaling[3]=2

    ;;

    hwrf_nmm_d2 | hwrf_nmm_d3)

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:20:00" ; popts[1]="6/6/"  ; ropts[1]="/1"
            topts[2]="0:20:00" ; popts[2]="8/8/"  ; ropts[2]="/1"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:20:00" ; popts[1]="6/6/"  ; ropts[1]="/1"
            topts[2]="0:20:00" ; popts[2]="8/8/"  ; ropts[2]="/1"
        elif [[ "$machine" = "Cheyenne" ]]; then
            topts[1]="0:20:00" ; popts[1]="6/6/"  ; ropts[1]="/1"
            topts[2]="0:20:00" ; popts[2]="8/8/"  ; ropts[2]="/1"
        elif [[ "$machine" = "WCOSS_C" ]]; then
            topts[1]="1:20:00" ; popts[1]="48/8/"  ; ropts[1]="1024/1"  # sub_wcoss_c popts are "#tasks/#nodes/"
            topts[2]="1:20:00" ; popts[2]="60/10/"  ; ropts[2]="1024/1"
        elif [[ "$machine" = "WCOSS_D" ]]; then
           topts[1]="0:15:00" ; popts[1]="10/10/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="14/14/" ; ropts[2]="/2"
        elif [[ "$machine" = "s4" ]]; then
            topts[1]="0:40:00" ; popts[1]="6/6/"  ; ropts[1]="/1"
            topts[2]="0:40:00" ; popts[2]="8/8/"  ; ropts[2]="/1"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="1:00:00"
        fi

        scaling[1]=5; scaling[2]=10; scaling[3]=2

    ;;

    global_enkf_T62)

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:15:00" ; popts[1]="12/3/" ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="12/5/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:15:00" ; popts[1]="16/2/" ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "Cheyenne" ]]; then
            topts[1]="0:15:00" ; popts[1]="16/2/" ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_C" ]]; then
            topts[1]="0:15:00" ; popts[1]="20/4/" ; ropts[1]="1024/1"  # sub_wcoss_c popts are "#tasks/#nodes/"
            topts[2]="0:15:00" ; popts[2]="20/5/" ; ropts[2]="1024/2"
        elif [[ "$machine" = "s4" ]]; then
            topts[1]="0:25:00" ; popts[1]="32/2/" ; ropts[1]="/1"
            topts[2]="0:25:00" ; popts[2]="32/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "WCOSS_D" ]]; then
           topts[1]="0:15:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="1:00:00"
        fi

        scaling[1]=10; scaling[2]=8; scaling[3]=2

    ;;

    *) # EXIT out for unresolved regtest

        echo "unknown $regtest"
        exit 1

esac

job[1]=${regtest}_loproc_updat
job[2]=${regtest}_hiproc_updat
job[3]=${regtest}_loproc_contrl
job[4]=${regtest}_hiproc_contrl

topts[3]=${topts[1]} ; popts[3]=${popts[1]} ; ropts[3]=${ropts[1]}
topts[4]=${topts[2]} ; popts[4]=${popts[2]} ; ropts[4]=${ropts[2]}

tmpregdir="tmpreg_$regtest"
rcname="return_code_${regtest}.out"
result="${regtest}_regression_results.txt"

export sub_cmd
export job
export topts
export popts
export ropts
export rcname
export tmpregdir
export result
export scaling

if [[ "$machine" = "Theia" ]]; then
   export OMP_STACKSIZE=1024M
   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   export APRUN="mpirun -v -np \$PBS_NP"
elif [[ "$machine" = "Cheyenne" ]]; then
   export OMP_STACKSIZE=1024M
   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   export APRUN="mpirun -v -np \$NCPUS"
elif [[ "$machine" = "WCOSS" ]]; then
   export MP_USE_BULK_XFER=yes
   export MP_BULK_MIN_MSG_SIZE=64k
   export APRUN="mpirun.lsf"
   if [ "$debug" = ".true." ] ; then
      export MP_DEBUG_NOTIMEOUT=yes
   fi
elif [[ "$machine" = "WCOSS_C" ]]; then
   export KMP_AFFINITY=disabled
   export OMP_STACKSIZE=2G
   export FORT_BUFFERED=true
   export APRUN="mpirun -v -np \$PBS_NP"
elif [[ "$machine" = "WCOSS_D" ]]; then
   export OMP_STACKSIZE=1024M
   export PATH=$PATH:$NS/taumpi/x86_64/bin
   export LD_LIBRARY_PATH=/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/lib:/usrx/local/prod/intel/2018UP01/itac_latest/slib:/usrx/local/prod/intel/2018UP01/compilers_and_libraries/linux/lib/intel64_lin:/usrx/local/prod/intel/2018UP01/compilers_and_libraries/linux/mpi/lib64:/usrx/local/prod/intel/2018UP01/compilers_and_libraries/linux/ipp/lib/intel64_lin:/usrx/local/prod/intel/2018UP01/compilers_and_libraries/linux/mkl/lib/intel64_lin:/usrx/local/prod/intel/2018UP01/compilers_and_libraries/linux/tbb/lib/intel64_lin/gcc4.7:/usrx/local/prod/intel/2018UP01/debugger_2018/iga/lib:/usrx/local/prod/intel/2018UP01/debugger_2018/libipt/intel64/lib:/usrx/local/prod/intel/2018UP01/compilers_and_libraries/linux/daal/lib/intel64_lin:/usrx/local/prod/intel/2018UP01/compilers_and_libraries/linux/tbb/lib/intel64_lin/gcc4.4:/gpfs/lsf/10.1/linux3.10-glibc2.17-x86_64/lib
   export TAU_MAKEFILE=/gpfs/dell2/emc/modeling/noscrub/Mark.Potts/taumpi/x86_64/lib/Makefile.tau-icpc-papi-mpi-openmp-opari
   export TAU_OPTIONS="-optKeepFiles -optVerbose -optRevert -optTauSelectFile=$NS/select.tau -optPdtF90Parser=/scratch/mpotts/pdt/x86_64/bin/f95parse -optMemDbg -optDetectMemoryLeaks "
   export TAU_PROFILE=1
   export TAU_PROFILE_FORMAT=merged
   export APRUN="mpirun tau_exec -io "
elif [[ "$machine" = "s4" ]]; then
   export APRUN="srun"
   export MPI_BUFS_PER_PROC=2048
   export MPI_BUFS_PER_HOST=2048
   export MPI_GROUP_MAX=256
   export MPI_MEMMAP_OFF=1
   export MP_STDOUTMODE=ORDERED
   export KMP_STACKSIZE=512MB 
   export KMP_AFFINITY=scatter
elif [[ "$machine" = "discover" ]]; then
   export APRUN="mpiexec_mpt -np \$SLURM_NTASKS"
fi
