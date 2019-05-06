SOURCES = $(wildcard sources/foreign-interfaces/*.scm) \
          $(wildcard sources/units/*.scm)

OBJECTS = $(SOURCES:.scm=.o)

TYPES = $(SOURCES:.scm=.types)

TYPES_CHECK = $(SOURCES:.scm=.types.check)

BUILD = build/object.o

all : $(BUILD) main tags

%.o : %.scm
	csc5 -I/usr/local/include -emit-types-file $(subst .scm,.types,$<) -c $< -o $@

%.types.check : %.scm build/types
	csc5 -I/usr/local/include -types build/types -c $< -o $@

$(BUILD) : | build-directory

build-directory :
	@mkdir -p build

build/object.o : $(OBJECTS)
	ld -r $(OBJECTS) -o build/object.o

build/types : $(OBJECTS)
	cat $(TYPES) > $@

main : build/object.o build/types sources/main.scm
	csc5 -L -lb64 -types build/types build/object.o \
	sources/main.scm -o main

tags : $(OBJECTS)
	uctags -R --languages=scheme

check-types : $(TYPES_CHECK)

clean :
	find . -name '*.o' | xargs /bin/rm -f
	find . -name '*.types' | xargs /bin/rm -f
	rm -rf build
	rm -f tags
	rm -f main
