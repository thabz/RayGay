
epub:
	xsltproc ~/docbook-xsl-1.76.1/epub/docbook.xsl reference.docbook 
	echo "application/epub+zip" > mimetype
	rm reference.epub
	zip -0Xq  reference.epub mimetype
	zip -Xr9D reference.epub OEBPS
	zip -Xr9D reference.epub META-INF
	rm -fr OEBPS
	rm -fr META-INF 
	rm -fr mimetype

html:
	xsltproc --stringparam html.stylesheet reference.css ~/docbook-xsl-1.76.1/html/docbook.xsl reference.docbook > ref.html

chunked:
	xsltproc --stringparam html.stylesheet reference.css http://docbook.sourceforge.net/release/xsl/current/html/chunk.xsl reference.docbook 
pdf:
	xsltproc ~/docbook-xsl-1.76.1/pdf/docbook.xsl reference.docbook 

