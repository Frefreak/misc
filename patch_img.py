import numpy as np
import argparse
from PIL import Image
import cv2

parser = argparse.ArgumentParser()

parser.add_argument('orig')
parser.add_argument('mask')
parser.add_argument('-x', default=149, type=int)
parser.add_argument('-y', default=433, type=int)

def main():
    args = parser.parse_args()
    patch(args.orig, args.mask, args.x, args.y)

def patch(orig, mask, x, y):
    cv2.namedWindow('Preview', cv2.WINDOW_NORMAL)
    orig = Image.open(orig)
    mask = Image.open(mask).convert('RGB')
    w, h = mask.size
    orig_arr = np.array(orig)
    mask_arr = np.array(mask)
    while True:
        new_arr = orig_arr.copy()
        new_arr[y:y+h,x:x+w] = mask_arr
        cv2.imshow('Preview', cv2.cvtColor(new_arr, cv2.COLOR_RGB2BGR))
        cmd = cv2.waitKey(0)
        print(cmd)
        cmd = chr(cmd)
        print(cmd)
        match cmd:
            case 'h':
                x -= 1
            case 'l':
                x += 1
            case 'j':
                y += 1
            case 'k':
                y -= 1
            case 'a':
                x -= 10
            case 'd':
                x += 10
            case 's':
                y += 10
            case 'w':
                y -= 10
            case 'q':
                print(x, y)
                cv2.destroyAllWindows()
                break
    new_img = Image.fromarray(new_arr)
    new_img.save('new.png')

if __name__ == "__main__":
    main()


