
SUBDIRS = src test

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

