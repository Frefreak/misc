#!/usr/bin/env python3

import sys
import sqlite3
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('cdb')
parser.add_argument('deck')
parser.add_argument('--hand', type=lambda x: [int(n) for n in x.split(',')])
# parser.add_argument('--grave', type=lambda x: x.split(','))
parser.add_argument('-o', '--out', default=None)

f = None

def pb(s: str):
    if f is None:
        print(s)
        print()
    else:
        f.write(s)
        f.write('\n\n')

def load_all_cards(cdb):
    db = sqlite3.connect(cdb)
    cursor = db.cursor()
    return {c[0]:c[1] for c in cursor.execute('select id, name from texts')}


def parse_deck(file):
    main_cards = []
    extra_cards = []
    s = 0 # 0 - nothing, 1 - main, 2 - extra
    with open(file) as f:
        for l in f.read().splitlines():
            match l:
                case '#main':
                    s = 1
                case '#extra':
                    s = 2
                case '!side':
                    s = 0
                case _:
                    if l.startswith('#'):
                        pass
                    else:
                        if s == 1:
                            main_cards.append(int(l))
                        elif s == 2:
                            extra_cards.append(int(l))
    return main_cards, extra_cards

def main():
    args = parser.parse_args()
    id_maps = load_all_cards(args.cdb)
    if args.out is not None:
        global f
        f = open(args.out, 'w')
    main_cards, extra_cards = parse_deck(args.deck)
    if args.hand is not None:
        for c in args.hand:
            if c not in main_cards:
                print(f"invalid hand: {c}")
                sys.exit(1)
            main_cards.remove(c)

    pb("""Debug.SetAIName("高性能电子头脑")
Debug.ReloadFieldBegin(DUEL_ATTACK_FIRST_TURN+DUEL_SIMPLE_AI,5)
Debug.SetPlayerInfo(0,8000,0,0)
Debug.SetPlayerInfo(1,8000,0,0)""")

    main_strs = []
    for c in main_cards:
        main_strs.append(f'Debug.AddCard({c:10},0,0,LOCATION_DECK,0,POS_FACEDOWN) -- {id_maps[c]}')
    pb("\n".join(main_strs))

    extra_strs = []
    for c in extra_cards:
        extra_strs.append(f'Debug.AddCard({c:10},0,0,LOCATION_EXTRA,0,POS_FACEDOWN) -- {id_maps[c]}')
    pb("\n".join(extra_strs))

    hand_strs = []
    for c in args.hand:
        hand_strs.append(f'Debug.AddCard({c:10},0,0,LOCATION_HAND,0,POS_FACEDOWN) -- {id_maps[c]}')
    pb("\n".join(hand_strs))

    pb("""Debug.ReloadFieldEnd()
Debug.ShowHint("GAME START!")
aux.BeginPuzzle()""")

if __name__ == "__main__":
    main()
