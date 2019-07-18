The trunk folder contains the following files:
BasicGUI    
Execute.hs           
BasicGUI.hs  
MyParser.hs  
factorial.L  
static/

To run the project from the executable providied (BasicGUI), it can be run from the terminal like a normal linux executable, so from the trunk folder you can just type "./BasicGUI" to run the project. The static folder contains some css and js files used in the program, the code will still run without it but without any css
To actually use the program, the user must navigate to the "127.0.0.1:8023" address in a modern web browser (Chromium/Google Chrome prefered), which will load up the GUI.

To build the code from the source, the Haskell Platform must be installed which can be found at https://www.haskell.org/downloads.
The require packages can be installed from the terminal using cabal which is installed as part of the Haskell Platform. The command to install packages is cabal install and the package names are:
threepenny-gui
parsec
text

Once the required packages are installed the code can be built from command line by typing "ghc BasicGUI.hs". This should generate an executable file which can be ran with the instructions above.

Tested on Ubuntu 16.04 with Chromium browser although the instructions are nearly exactly the same for building and running the executable on Windows aswell, except that the executable generated would have ".exe" as a suffix


