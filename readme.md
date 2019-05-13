# OWO PIC Demo Board
This is a very simple demo board I designed around one of my favorite microcontrollers: The PIC 16F628A from Microchip.
I also wrote a small "framework" in asm for it, that handles the display, buttons, serial comm, text scrolling, etc, and I've included a simple implementation of Tetris as an example of its usage.

You'll find the schematic and board layout (KiCad) of the *OWO board* in the **/board** folder. There's a lot that can be improved but it works as it is. The demo code and framework/boilerplate is in the **/asm_demo_code** directory.

Here's a video of one the early builds of the Tetris demo:
<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Tetris implemented in assembly on the PIC16F628A demo board I designed a few months ago. <a href="https://twitter.com/hashtag/electronics?src=hash&amp;ref_src=twsrc%5Etfw">#electronics</a> <a href="https://twitter.com/hashtag/DIY?src=hash&amp;ref_src=twsrc%5Etfw">#DIY</a> <a href="https://twitter.com/hashtag/assemblycode?src=hash&amp;ref_src=twsrc%5Etfw">#assemblycode</a> <a href="https://twitter.com/hashtag/microchip?src=hash&amp;ref_src=twsrc%5Etfw">#microchip</a> <a href="https://t.co/Uh5Tp1Ro5d">pic.twitter.com/Uh5Tp1Ro5d</a></p>&mdash; Elias Zacarias (@battlecoder) <a href="https://twitter.com/battlecoder/status/1109868608677707781?ref_src=twsrc%5Etfw">March 24, 2019</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

