#!/bin/bash
# wersja: 1.0

############### Preferencje ###############
plik_exe=Interpret

folder_z_testami='./tests/bad/'
rozszerzenie_dane='.in'
rozszerzenie_wynik='.out'
rozszerzenie_wyj_bledow='.err'
folder_na_bledy='./Bledy/'
###########################################

clear

# Obsługa opcji
while getopts ":C:c:Vnd:b" opt; do
  case $opt in
    c)
        plik_exe=$OPTARG
      ;;
    \?)
      echo "Niepoprawna opcja: -$OPTARG"
      exit 1
      ;;
    :)
      echo "Opcja -$OPTARG potrzebuje argumentu."
      exit 1
      ;;
  esac
done
shift $((OPTIND-1))


# Funkcja testująca
testuj () {
	test_nazw=`tr '/' '_' <<< $1`
	# $1 - ścieżka do testu z '/'
	# $test_nazw - ścieżka do testu z '_' zamiast '/'
	# $2 - stdin / file
	zle=0

	echo -n " $1 $2: "

	rm -f $folder_na_bledy$test_nazw.$2.twoj{$rozszerzenie_wynik,$rozszerzenie_wyj_bledow}
	rm -f $folder_na_bledy$test_nazw.$2.diff{$rozszerzenie_wynik,$rozszerzenie_wyj_bledow}

	[ -w $folder_z_testami ] || { echo -e "\e[31mBłąd: nie mogę zapisywać do $folder_z_testami\033[0m"; return 0; }
	[ -r $folder_z_testami$1$rozszerzenie_dane ] || { echo -e "\e[31mBłąd: nie ma $folder_z_testami$1$rozszerzenie_dane\033[0m"; return 0; }
	[ -r $folder_z_testami$1$rozszerzenie_wynik ] || { echo -e "\e[31mBłąd: nie ma $folder_z_testami$1$rozszerzenie_wynik\033[0m"; return 0; }

	if [ $2 == "stdin" ];
		then
			./$plik_exe < $folder_z_testami$1$rozszerzenie_dane > $folder_na_bledy$test_nazw.$2.twoj$rozszerzenie_wynik 2> $folder_na_bledy$test_nazw.$2.twoj$rozszerzenie_wyj_bledow
		else
			./$plik_exe $folder_z_testami$1$rozszerzenie_dane > $folder_na_bledy$test_nazw.$2.twoj$rozszerzenie_wynik 2> $folder_na_bledy$test_nazw.$2.twoj$rozszerzenie_wyj_bledow
	fi

	diff $folder_z_testami$1$rozszerzenie_wynik $folder_na_bledy$test_nazw.$2.twoj$rozszerzenie_wynik &> $folder_na_bledy$test_nazw.$2.diff$rozszerzenie_wynik
	if [ $? -ne 0 ];
		then
			echo -e -n "\e[31mBłąd: wynik  \033[0m"
			zle=1
	fi

	if [ $zle -eq 0 ];
		then
			rm -f $folder_na_bledy$test_nazw.$2.twoj{$rozszerzenie_wynik,$rozszerzenie_wyj_bledow}
			rm -f $folder_na_bledy$test_nazw.$2.diff{$rozszerzenie_wynik,$rozszerzenie_wyj_bledow}
			echo -e "\e[32mok\033[0m"
			return 1
		else
			echo ''
			return 0;
	fi

}


# Testy
echo '################ Testowanie ################'
echo
if [ ! -d $folder_na_bledy ];
	then mkdir $folder_na_bledy
fi

[ `find $folder_z_testami -name "*$rozszerzenie_dane" | wc -l` -ne 0 ] &> /dev/null || { echo -e "\e[31mBłąd: nie ma żadnych testów\033[0m"; exit 1; }
if [ $# -gt 0 ];
	then testy=$@
	else
		for i in `find $folder_z_testami -name "*$rozszerzenie_dane"`; do
			test=${i%$rozszerzenie_dane}
			test=${test#$folder_z_testami}
			testy="$testy $test "
		done
fi


ilosc=0
dobrze=0
[ -x ./$plik_exe ] || { echo -e "\e[31mBłąd: nie mogę uruchomić: ./$plik_exe\033[0m"; exit 1; }
for i in $testy; do
	ilosc=$((ilosc + 1))
	testuj $i 'file'
	kod=$?
	dobrze=$((dobrze + kod))

	ilosc=$((ilosc + 1))
	testuj $i 'stdin'
	kod=$?
	dobrze=$((dobrze + kod))
done

echo


# Podsumowanie
echo
if [ $ilosc -eq $dobrze ];
		then
			echo -e "\e[32mWszystko OK!\033[0m"
			echo
			exit 0
		else
			echo -e "\e[31mSą błędy!\033[0m"
			echo -e "\e[31mPoprawnie $dobrze/$ilosc\033[0m"
			echo
			exit 1
fi


# by Mateusz Banaszek
