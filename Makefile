CC = gcc
CFLAGS = -Wall -Wextra -std=c11 -D_DEFAULT_SOURCE
TARGET = compiler

$(TARGET): main.c
	$(CC) $(CFLAGS) main.c -o $(TARGET)

clean:
	rm -f $(TARGET)

.PHONY: clean
