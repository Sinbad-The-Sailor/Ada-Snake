# Ada-Snake
The classic game snake recreated in Ada for an introductory course in programming at my university. The game is written with a proprietary library TJa which enables one to use graphics in the terminal. This library has been kept out of the git repository when made public, as it is owned by the university. 

The game is written such that the logical and graphical components of the game are separate, as per usual standard in games. This also means the game can easily be modified or expanded. 

# Pictures 
![Snake game](https://user-images.githubusercontent.com/62723280/94471876-2c1c3600-01ca-11eb-8a83-6845280c05e8.png)

# Scope
Because of the time constraints of the project it was most important to make sure the program was made such that it could easily be modified or expanded. With this said, the game still functions as one expecteds Snake to function. There is also a highscore mechanic implemented, however, it is limited in many aspects. 

Note: the game is only playable on a Linux based system.

# Commands
If one has access to the TJa library, compiling the program is done by running the following command:
```
gnatmake -I/path/TJa main.adb 
```
Then running the program.
```
./main 
```
Finally, removing the compiled extraneous files after the game has been played:
```
gnatclean main.adb 
```
I usually add -q afterwards in the first command. "-q" stands for quiet compilation and is prefered since it does not clutter the terminal which is important since the game is played inside the terminal window.
