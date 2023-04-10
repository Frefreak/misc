import os
import time
import openai

# Load your API key from an environment variable or secret management service
openai.api_key = os.getenv("OPENAI_API_KEY")

logs = []

def read_to_eof():
    print('\x1b[32;1m\n\nYou: \x1b[0m', end='')
    lines = []
    while True:
        try:
            line = input()
            lines.append(line)
        except EOFError:
            break
    return '\n'.join(lines)

def send(inp):
    logs.append({
        "role": "user",
        "content": inp,
    })
    response = openai.ChatCompletion.create(model="gpt-3.5-turbo", messages=logs)
    choice = response['choices'][0] # type: ignore
    print('\n\x1b[33;1massistant: \x1b[0m', end='')
    print('\x1b[36;1m' + choice['message']['content'] + '\x1b[0m')
    logs.append(choice['message'])

initial_inp = f"""You are ChatGPT, a large language model trained by OpenAI. Answer as concisely as possible. Current date: {time.strftime("%m/%d/%Y")}. Now please wait for my query/instruction."""

send(initial_inp)
while True:
    inp = read_to_eof()
    send(inp)
