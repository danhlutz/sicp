# python 3.6

import sys

def move(layers, from_post, to_post, spare_post):
    if layers == 0:
        pass
    else:
        move(layers - 1, from_post, spare_post, to_post)
        print(f'Move {layers} from {from_post} to {to_post}')
        move(layers - 1, spare_post, to_post, from_post)

def hanoi(layers):
    move(layers, "START", "FINISH", "SPARE")


if __name__ == '__main__':
    layers = int(sys.argv[1])
    hanoi(layers)
