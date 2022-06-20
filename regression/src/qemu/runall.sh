if [ -z "$1" ]
then
	echo "Executing without loader"
	for f in no_loader/*
	do
		echo "-------- Running $f"
		$f
		if [ "$?" != "0" ]; 
		then 
			echo "ERROR: FAILED TEST" 
			exit
		fi
	done
else
	echo "Executing with loader"
	for f in with_loader/*
	do
		echo "-------- Running $f"
		./loader $f
		if [ "$?" != "0" ]; 
		then 
			echo "ERROR: FAILED TEST" 
			exit
		fi
	done
fi
