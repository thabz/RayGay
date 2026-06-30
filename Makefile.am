
SUBDIRS = src test scenes docs
ACLOCAL_AMFLAGS = -I m4
if OS_DARWIN
export MACOSX_DEPLOYMENT_TARGET
AM_MAKEFLAGS = MACOSX_DEPLOYMENT_TARGET=$(MACOSX_DEPLOYMENT_TARGET)
endif
dist_bin_SCRIPTS=animation-render.sh

doc:
	cd @top_distdir@ && doxygen 

dist-win-zip:
	rm -fr raygay        
	mkdir -p raygay/scenes
	cp -r scenes/windows/* raygay/scenes/
	cp src/tracer.exe raygay
	strip raygay/tracer.exe
	rm -fr raygay.zip
	zip -r raygay.zip raygay
	rm -fr raygay
