#!/usr/bin/env python
import subprocess
import argparse
from shlex import quote
import yaml
import jinja2

template = r"""
[Script Info]
Title: {{title}}
ScriptType: v4.00+
PlayResX: 800
PlayResY: 600

[V4+ Styles]
Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding
Style: Default,Wenquanyi Micro Hei,20,&H00FFFFFF,&H00000000,&H00000000,&H0075757B,0,0,0,0,100,100,0,0,1,0.75,0.5,2,10,10,10,1

[Events]
Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text
{%- for line in lines %}
Dialogue: 0,{{line[0]}},{{line[1]}},Default, 1,0000,0000,0000,,{\pos(400,570)}{{line[2]}}
{%- endfor %}
"""

DFT_ASS_OUT = "/tmp/out.ass"


def mk_subtitle(args):
    print("make subtitle")
    write_ass(args)
    cmd = f"""mpv -vf 'ass="{args.out}"' {quote(args.video)}"""
    print(cmd)
    subprocess.run(cmd, shell=True)

def find_vid_length(fp: str) -> str:
    ss = subprocess.getoutput(f'ffprobe -i {fp} |& grep Duration')
    t = ss.splitlines()[0].split(', ')[0].split(': ', 1)[1]
    print(f'video length: \x1b[31;1m{t}\x1b[0m')
    [h, m, s] = t.split(':', 2)
    return str(int(h) * 3600 + int(m) * 60 + float(s))

def mk_final(args):
    length = find_vid_length(args.video)
    print(f'video length in secs: \x1b[31;1m{length}\x1b[0m')
    print("make final")
    cmds = ["ffmpeg", "-i", args.video, "-filter_complex"]
    # ass & trim
    if args.from_time is None:
        from_time = "0"
    else:
        from_time = args.from_time
    if args.to_time is None:
        to_time = length
    else:
        to_time = args.to_time

    first_filter = f"[0]ass={args.subtitle},trim={from_time}:{to_time}[t]; "

    # mask
    second_filter = f"color=black,crop=212:20:0:0,trim={from_time}:{to_time}[mask]; "

    # overlay
    third_filter = "[t][mask]overlay=1270:18[out]"

    cmds.append(first_filter + second_filter + third_filter)
    cmds.extend(["-map", "[out]", "-c:v", "libx264", "-c:a", "copy", args.out])
    print(cmds)
    print(" ".join(cmds))
    subprocess.run(cmds)


parser = argparse.ArgumentParser()

subparsers = parser.add_subparsers(dest="command")
subtitle_cmd = subparsers.add_parser("subtitle", aliases=["s", "sub"])
subtitle_cmd.add_argument("video")
subtitle_cmd.add_argument("config")
subtitle_cmd.add_argument("-o", "--out", default=DFT_ASS_OUT)
subtitle_cmd.set_defaults(func=mk_subtitle)

final_cmd = subparsers.add_parser("final", aliases=["f", "fi"])
final_cmd.add_argument("video")
final_cmd.add_argument("config")
final_cmd.add_argument("-s", "--subtitle", default=DFT_ASS_OUT)
final_cmd.add_argument("-f", "--from-time")
final_cmd.add_argument("-t", "--to-time")
final_cmd.add_argument("-o", "--out", default='out.mkv')
final_cmd.set_defaults(func=mk_final)


def write_ass(args):
    with open(args.config) as f:
        c = yaml.safe_load(f)
    env = jinja2.Environment()
    with open(args.out, "w") as f:
        f.write(
            env.from_string(template).render(
                title=c["title"], lines=[x.split(" ", 2) for x in c["subtitles"]]
            )
        )


def main():
    args = parser.parse_args()
    if hasattr(args, 'func') and args.func:
        args.func(args)
    else:
        parser.print_help()


if __name__ == "__main__":
    main()
