Hey, this is a simple template for making elm presentations, designed for you guys taking
COMPSCI 1JC3 at McMaster University. Hopefully this will speed up the process significantly!

I'm also including my original presentation from when I took the course (updated so it works in
Elm 0.17, hopefully it gives you some inspiration.)

If you are unlike me and like low 30 FPS on the presentation because you live in 1996, you can go into
GraphicSVG.elm and around line 85 you will find the following:

Sub.batch ([ Time.every (1000/60*millisecond) (createTimeMessage)

Decrease the number for better performance on weaker computers.

September 26th: Ray speaking again, please use pres-template.elm or jobs-pres as your default 
presentation, as I have added navigation function using the left and right arrow keys, as well
as pause, fast-forward and rewind.
