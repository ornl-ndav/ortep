# Makefile for the ORTEP-III

FC=gfortran
TARGET= ortep3
SRC= ortep.f
OBJ= ortep.o
FFLAGS= -O2
LFLAGS= -lpgplot

all: ${OBJ}
	${FC} ${LFLAGS} -o ${TARGET} ${OBJ}

ortep.o: ortep.f
	${FC} ${FFLAGS} -c $<

ortep-1.0.3.tar.gz:
	 tar -c -z -f ortep-1.0.3.tar.gz --transform='s,^,ortep-1.0.3/,' Makefile *.f

install: ortep3
	install -c -m 755 ortep3 ${DESTDIR}/usr/bin/ortep3

clean:
	rm -f *.o ${TARGET}
