# Global Rules

## Ask questions promptly

When there is anything unclear/ambiguous when making a plan or performing some task, ask the user to clarify or decide.
Do not act without everything becomes clear.

## No guessing — proofs only

When investigating bugs, behavior, or causes, base every claim on concrete evidence — actual log lines, code references, file contents, or test output. Never speculate or assume.

**How to apply:**
- Before stating a cause, cite the specific evidence: log line numbers, code locations, exact tool output.
- If the evidence is incomplete, say so and ask for more (run a command, add a debug log, share a file).
- If something is genuinely uncertain, label it as a hypothesis and propose how to verify it — don't present hypotheses as conclusions.
- Read provided materials (logs, dumps, error messages) carefully and completely before responding. Don't skim and pattern-match.
- When the user pastes test output or logs, every detail matters — count the events, check timestamps, follow the sequence.

## Python environment

Default to the uv-managed venv at `~/.venv` for running Python. A task-local venv is fine when it genuinely makes sense for that task, and `uv run --script` shebangs with inline `# /// script` dependency blocks are also acceptable.

Never invoke macOS stock `/usr/bin/python3` — it lacks dependencies and doesn't symlink `python` to `python3`, which the user dislikes (it reflects an outdated Python 2 mindset).

**How to apply:** If a `python3` invocation fails with `ModuleNotFoundError`, do not retry against system Python — switch to `~/.venv` or the uv shebang form.

## Shell

For shell executation, try to make the command easier to read, like how a human
might write and read.

For kubectl, prefer 'jq' instead of 'jsonpath'

## Git

When making git commit, do not including a Co-Authored-By part

When creating a new repo, the default branch should be 'master', not 'main'.


## English

I'm not native English speaker but willing to use perfect/accurate English. If my prompt has mistakes (language-wise) or sounds non-idiomatic,
mention it briefly/concisely or with a side node alongside your response, so I can improve.
