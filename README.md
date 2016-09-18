Hey, this is a simple template for making elm presentations, designed for you guys taking
COMPSCI 1JC3 at McMaster University. Hopefully this will speed up the process significantly!

I'm also including my original presentation from when I took the course (updated so it works in
Elm 0.17, hopefully it gives you some inspiration.)

If you are like me and dislike the default low FPS on the presentation, you can go into
GraphicSVG.elm and around line 85 you will find the following:

Sub.batch ([ Time.every (1000/30*millisecond) (createTimeMessage)

Increase the number '30' for better frames, but it will affect performance on poorer computers. 
