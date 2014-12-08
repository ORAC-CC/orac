all:
	cd common && $(MAKE)
	cd pre_processing && $(MAKE)
	cd src && $(MAKE)
	cd post_processing && $(MAKE)

clean tidy:
	cd common && $(MAKE) clean
	cd pre_processing && $(MAKE) clean
	cd src && $(MAKE) clean
	cd post_processing && $(MAKE) clean