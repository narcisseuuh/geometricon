# geometricon

## Idea

I am currently studying "Introduction to Static Analysis" by Xavier Rival and Kwangyeuk Yi. In the first parts of the book, they focus on analysis by abstract interpretation of a language for geometry in $RR^2$. In this repository, I am trying to implement a basic analyzer for it, following the details presented in the book.
The name of the project was inspired by a mathematical cartoon written by Jean Pierre Petit that one of my teachers of mathematics always praised to us, and also because the language presented didn't have a name.

## Building the project

To build the project, you can run :
```sh
make
```
and then to install it, you can run :
```sh
make install
```
You will then have installed the binaries `interpreter` and `analyzer` which provides both the interpretation for the language and its analysis.

## Documentation

A documentation has been written in `doc/` to clarify the choices I made and the techniques I tried to implement.