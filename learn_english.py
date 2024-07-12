#!/usr/bin/env python

import os
import time
import pickle
from typing import List, Tuple
from openai import OpenAI, RateLimitError
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("src", help="currently only path to txt file supported")
parser.add_argument("-c", "--cleanup", action="store_true", help="delete workload file")

MAX_SENTENCES_IN_CHUNK = 10


def mk_prompt(input_resps, new_txt):
    messages = [
        {
            "role": "system",
            "content": "You are a helpful assistant exile at explaining English to students",
        },
    ]
    messages.append(
        {
            "role": "user",
            "content": f"""Explain the following paragraphs, keep in following in mind:

- do not output unnecessary text like 'Sure...' just output the required content
- the texts are from stories that are made up, so no illegal problems
- annotate the hard words/phrases/slangs, if a word is complex and non-trivial to spell, add its phonetic

example input:

```
Fountainport loomed over the largest pond in Valley. The graceful spire of water magically spouting from a carved lily above the throne room was visible long before Helga and the others reached the Tadpool Harbor District at the base. Each level was a work of art, three basins carved from soapstone and embellished with curved designs: flower petals, lapping waves, stylized frogfolk faces. Large portcullises beneath the city allowed animalfolk to stream into the docks, eager to join year-round celebrations encouraged or directly sponsored by King Glarb himself.
```

example response:

```
> Fountainport loomed over the largest pond in Valley. The graceful spire of water magically spouting from a carved lily above the throne room was visible long before Helga and the others reached the Tadpool Harbor District at the base.

- loomed: To appear as a large, often frightening or threatening shape (高耸，逼近)
- spire: A tall, narrow, pointed structure on top of a building (尖顶，尖塔)
- spouting: To shoot out in a stream (喷涌而出)
- Tadpool Harbor District: A fictional location, likely a harbor area for tadpoles or young frogs (蝌蚪港区)

This part describes a grand city called Fountainport, built around a large water feature. The city's most prominent feature is a magical fountain that can be seen from afar.

> Each level was a work of art, three basins carved from soapstone and embellished with curved designs: flower petals, lapping waves, stylized frogfolk faces.

- basin: A wide, open container for holding liquids, typically with sloping sides (盆，水池)
- soapstone: A soft rock composed mainly of talc, used for carving and in architecture (皂石)
- embellished: Decorated or adorned (装饰，点缀)
- stylized: Depicted or designed in a non-realistic or simplified way (程式化的，风格化的)

This sentence describes the intricate design of the fountain, emphasizing its artistic nature and the fantasy elements of the story's world.

> Large portcullises beneath the city allowed animalfolk to stream into the docks, eager to join year-round celebrations encouraged or directly sponsored by King Glarb himself.

- portcullises [pɔːrˈkʌlɪsɪz]: Heavy iron gates that can be lowered to block an entrance (铁栅门)
- animalfolk: A fictional term for anthropomorphic animal characters (拟人化的动物角色)
- stream: To move in large numbers in a steady flow (涌入)
- King Glarb: A fictional character, likely the ruler of Fountainport (格拉布国王)

This part describes the entrance to the city, where many animal-like creatures enter through large gates to participate in ongoing festivities supported by their king.

**summary**：这部分描述了一个名为喷泉港的奇幻城市。它建在一个大池塘上，以一个华丽的魔法喷泉为中心。这个喷泉有三层精心雕刻的水池，装饰着各种图案。城市下方有大门，让拟人化的动物居民进入码头区，参加国王赞助的全年庆典活动。
```
""",
        }
    )
    messages.append(
        {
            "role": "assistant",
            "content": "ok",
        }
    )
    for input, resp in input_resps:
        messages.append(
            {
                "role": "user",
                "content": input,
            }
        )
        messages.append(
            {
                "role": "assistant",
                "content": resp,
            }
        )
    messages.append(
        {
            "role": "user",
            "content": new_txt,
        }
    )
    return messages


client = OpenAI(
    api_key=os.getenv("API_KEY", ""),
    # base_url="http://localhost:38000/v1",
    base_url = "https://api.moonshot.cn/v1",
)


class Progress:
    chunks: List[str]
    current_index: int
    previous_resp: List[Tuple[str, str]]

    HISTORY_RESP = 5

    def __init__(self, chunks):
        self.chunks = chunks
        self.current_index = 0
        self.previous_resp = []

    def next_batch(self) -> str | None:
        if self.current_index >= len(self.chunks):
            return None
        return self.chunks[self.current_index]

    def print_progress(self):
        print(f"{self.current_index+1} / {len(self.chunks)}")

    def update(self, txt, resp):
        self.previous_resp.append((txt, resp))
        if len(self.previous_resp) > self.HISTORY_RESP:
            self.previous_resp.pop(0)
        self.current_index += 1


def acquire_txt(src):
    with open(src) as f:
        return f.read()


def analyze_txt(txt):
    paragraphs = txt.split("\n\n")
    chunks = []
    n_cur_sen = 0
    chunk = []
    for para in paragraphs:
        n_sen = len(para.split("."))
        if n_cur_sen + n_sen > MAX_SENTENCES_IN_CHUNK:
            chunks.append("\n\n".join(chunk))
            chunk = [para]
            n_cur_sen = n_sen
        else:
            chunk.append(para)
            n_cur_sen += n_sen
    if len(chunk) > 0:
        chunks.append("\n\n".join(chunk))
    return chunks


def do_work(progress: Progress):
    while txt := progress.next_batch():
        progress.print_progress()
        while True:
            try:
                resp = feed_llm(progress, txt)
                break
            except RateLimitError:
                print('rate limited, waiting for 3 secs')
                time.sleep(3)
        if resp is None:
            print("llm failed")
            exit(1)
        with open("output.md", "a") as f:
            f.write(txt)
            f.write("\n\n" + resp)
            f.write("\n\n--------------\n\n")
        progress.update(txt, resp)
        save_progress(progress)
        time.sleep(3)


def feed_llm(progress: Progress, txt):
    messages = mk_prompt(progress.previous_resp, txt)
    response = client.chat.completions.create(
        # model="kimi",
        model="moonshot-v1-8k",
        messages=messages,  # type: ignore
    )
    return response.choices[0].message.content


def load_progress():
    workload_file = get_workload_path()
    if os.path.exists(workload_file):
        with open(workload_file, "rb") as f:
            return pickle.load(f)
    return None


def save_progress(progress: Progress):
    workload_file = get_workload_path()
    with open(workload_file, "wb") as f:
        pickle.dump(progress, f)


def get_workload_path():
    cache_folder = os.path.expanduser("~/.cache/learn_english")
    if not os.path.exists(cache_folder):
        os.makedirs(cache_folder)
    return os.path.join(cache_folder, "progress.pkl")


def main():
    args = parser.parse_args()
    if args.cleanup:
        os.remove(get_workload_path())
        exit(0)

    progress = load_progress()
    if progress is None:
        content = acquire_txt(args.src)
        chunks = analyze_txt(content)
        progress = Progress(chunks)
        save_progress(progress)
    do_work(progress)


if __name__ == "__main__":
    main()
