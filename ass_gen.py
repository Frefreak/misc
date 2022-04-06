import argparse
import yaml
import jinja2

template = """
[Script Info]
Title: {{title}}
ScriptType: v4.00+
PlayResY: 800
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
parser = argparse.ArgumentParser()
parser.add_argument("-c", "--config", help="config yaml")
parser.add_argument("-t", "--title", help="title")


def main():
    args = parser.parse_args()
    with open(args.config) as f:
        c = yaml.safe_load(f)
    env = jinja2.Environment()
    print(
        env.from_string(template).render(
            title=c["title"], lines=[x.split(" ", 2) for x in c["lines"]]
        )
    )


# mpv --vf="ass=test.ass" test.mkv

if __name__ == "__main__":
    main()
