include ../make.inc
F2CLIBS = ../F2CLIBS/libF77.a ../F2CLIBS/libI77.a

all:  testlsame testslamch testdlamch testsecond testdsecnd 

testlsame:  lsame.o lsametst.o
	$(LOADER) -o testlsame lsame.o lsametst.o $(F2CLIBS)

testslamch: slamch.o lsame.o slamchtst.o
	$(LOADER) -o testslamch slamch.o lsame.o slamchtst.o $(F2CLIBS)

testdlamch: dlamch.o lsame.o dlamchtst.o
	$(LOADER) -o testdlamch dlamch.o lsame.o dlamchtst.o $(F2CLIBS)

testsecond: second.o secondtst.o
	$(LOADER) -o testsecond second.o secondtst.o $(F2CLIBS)

testdsecnd: dsecnd.o dsecndtst.o
	$(LOADER) -o testdsecnd dsecnd.o dsecndtst.o $(F2CLIBS)

slamch.o: slamch.c ; $(CC) -c $<
dlamch.o: dlamch.c ; $(CC) -c $<

clean:
	rm -f *.o test*

.c.o: ; $(CC) $(CFLAGS) -c $<

