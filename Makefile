
run-basics:
	slideshow slideshow-basics.rkt

print-basics:
	slideshow --pdf slideshow-basics.rkt

clean:
	rm -rf *~
	rm -rf slideshow-basics.pdf
