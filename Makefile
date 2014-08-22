all:
	cd common && $(MAKE)
	cd pre_processing/src && $(MAKE)
	cd src && $(MAKE)

clean tidy:
	cd common && $(MAKE) clean
	cd pre_processing/src && $(MAKE) clean
	cd src && $(MAKE) clean
