CC = gcc
CFLAGS = -g -O -W -Wall

default: qr1-foregone qr2-maze

qr1-foregone: qr1-foregone.o
	$(CC) -o qr1-foregone qr1-foregone.o

qr2-maze: qr2-maze.o
	$(CC) -o qr2-maze qr2-maze.o
