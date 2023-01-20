
	 ! Calculation of the number of lines in a file "Name".
	 SUBROUTINE Nline(Name,N)
	 CHARACTER *15 Name
	 INTEGER N 

       N=0
	 OPEN(10,FILE=Name)
       DO WHILE(.true.)
	    READ(10,*,iostat=ios)
		IF(ios>0) THEN
			STOP 'Error: check the format of the input files'
		ELSE
			IF(ios<0) THEN
				EXIT
			ELSE
				N=N+1
			END IF
	     END IF
       END DO
       CLOSE(10)

	 RETURN

	 END
