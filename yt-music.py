#!/usr/bin/env python3

"""
This script automates downloading a YouTube video as a high-quality MP3
with embedded cover art, and then prompts the user to add ID3v2 chapter
markers.

It requires 'yt-dlp', 'ffmpeg', and 'ffprobe' to be installed and
available in your system's PATH.

Workflow:
1.  Takes a YouTube URL as a mandatory argument.
2.  (Optional) A '--skip-download' flag to bypass downloading and use an
    existing local file.
3.  Determines the correct output filename from the URL.
4.  If not skipping, downloads the audio using yt-dlp with your specified
    settings (best audio, mp3, embedded thumbnail).
5.  If skipping, verifies the local file already exists.
6.  Gets the total duration of the MP3 file using ffprobe.
7.  Prompts you to paste your chapter list (e.g., "00:00:00.000 Title - Artist").
8.  Parses your input and converts it into an ffmpeg metadata file,
    calculating the end time for each chapter.
9.  Uses ffmpeg to create a new MP3 file containing the original audio,
    the cover art, and the new chapter metadata.
"""

import argparse
import sys
import os
import subprocess
from pathlib import Path
from shutil import which

# --- Configuration ---
# Executable names. Change these if they are different in your PATH.
FFMPEG_EXE = "ffmpeg"
FFPROBE_EXE = "ffprobe"
YT_DLP_EXE = "yt-dlp"
METADATA_FILENAME = "temp_ffmpeg_metadata.txt"
# ---------------------

def check_dependencies():
    """Checks if all required command-line tools are in the PATH."""
    deps = [FFMPEG_EXE, FFPROBE_EXE, YT_DLP_EXE]
    missing = []
    for dep in deps:
        if not which(dep):
            missing.append(dep)

    if missing:
        print("Error: Required dependencies are not found in your PATH:", file=sys.stderr)
        for dep in missing:
            print(f"- {dep}", file=sys.stderr)
        print("\nPlease install them to use this script.", file=sys.stderr)
        sys.exit(1)

def run_command(command, capture_output=False, description="command"):
    """Helper function to run a subprocess, with error handling."""
    print(f"Running: {' '.join(command)}")
    try:
        # Using text=True for automatic encoding/decoding
        result = subprocess.run(
            command,
            check=True,
            capture_output=capture_output,
            text=True,
            encoding='utf-8'
        )
        return result
    except subprocess.CalledProcessError as e:
        print(f"\nError running {description}.", file=sys.stderr)
        if e.stdout:
            print("--- STDOUT ---", file=sys.stderr)
            print(e.stdout, file=sys.stderr)
        if e.stderr:
            print("--- STDERR ---", file=sys.stderr)
            print(e.stderr, file=sys.stderr)
        sys.exit(1)
    except FileNotFoundError:
        print(f"Error: Command not found: {command[0]}", file=sys.stderr)
        print("Please ensure it is installed and in your PATH.", file=sys.stderr)
        sys.exit(1)

def get_mp3_filename(url):
    """
    Uses yt-dlp to determine what the final filename will be
    without downloading the file.
    """
    print("Determining output filename...")
    command = [
        YT_DLP_EXE,
        "--cookies-from-browser", "firefox",
        "-x",  # Extract audio
        "--audio-format", "mp3",
        "--get-filename",
        "-o", "%(title)s",
        url
    ]
    result = run_command(command, capture_output=True, description="get_filename")

    # The output filename will have '.mp3' as the extension
    # because of the --audio-format flag.
    filename = result.stdout.strip()

    # Handle potential edge cases where filename might be empty
    if not filename:
        print("Error: Could not determine filename from yt-dlp.", file=sys.stderr)
        sys.exit(1)

    return filename

def download_audio(url, output_filename):
    """
    Downloads the audio using the user's specified yt-dlp command.
    """
    print(f"\nStarting download for '{output_filename}'...")
    command = [
        YT_DLP_EXE,
        "--cookies-from-browser", "firefox",
        "-x",  # Extract audio
        "--audio-format", "mp3",
        "--audio-quality", "0",  # VBR 0 (highest)
        "--embed-thumbnail",
        "--convert-thumbnails", "jpg",  # For max compatibility
        "-o", output_filename,
        url
    ]
    run_command(command, description="download")
    print("Download complete.")

def get_user_chapters():
    """
    Prompts the user to paste their chapter list multiline.
    Returns a list of strings, one for each line.
    """
    print("\n--- Enter Chapter Data ---")
    print("Paste your chapter timestamps (e.g., '00:00:00.000 Title - Artist').")
    print("Press Enter on an empty line (or Ctrl+D) to finish:")

    lines = []
    try:
        while True:
            line = input()
            if not line:
                break
            lines.append(line)
    except EOFError:
        # User pressed Ctrl+D
        pass

    if not lines:
        print("No chapters entered. Exiting.", file=sys.stderr)
        sys.exit(1)

    return lines

def to_milliseconds(timestamp):
    """Converts 'HH:MM:SS.mmm' or 'MM:SS.mmm' to total milliseconds."""
    try:
        # Split time from milliseconds
        parts = timestamp.split('.')
        time_part = parts[0]
        ms = int(parts[1].ljust(3, '0')[:3]) if len(parts) > 1 else 0

        time_parts = [int(i) for i in time_part.split(':')]

        if len(time_parts) == 3:  # HH:MM:SS
            h, m, s = time_parts
        elif len(time_parts) == 2:  # MM:SS
            h = 0
            m, s = time_parts
        else:
            raise ValueError("Timestamp in unexpected format")

        total_ms = (h * 3600 + m * 60 + s) * 1000 + ms
        return total_ms
    except Exception as e:
        print(f"Error parsing timestamp '{timestamp}': {e}", file=sys.stderr)
        return None

def get_mp3_duration_ms(mp3_filename):
    """Gets the total duration of the MP3 file in milliseconds using ffprobe."""
    print("Getting audio duration...")
    command = [
        FFPROBE_EXE,
        "-v", "error",
        "-show_entries", "format=duration",
        "-of", "default=noprint_wrappers=1:nokey=1",
        mp3_filename
    ]
    result = run_command(command, capture_output=True, description="ffprobe")
    try:
        duration_sec = float(result.stdout.strip())
        return int(duration_sec * 1000)
    except Exception as e:
        print(f"Error parsing audio duration: {e}", file=sys.stderr)
        print(f"ffprobe output: {result.stdout.strip()}", file=sys.stderr)
        sys.exit(1)

def create_ffmpeg_metadata(chapter_lines, total_duration_ms):
    """
    Parses user input and creates a string in the ffmpeg
    metadata file format.
    """
    chapters = []
    for line in chapter_lines:
        line = line.strip()
        if not line:
            continue

        parts = line.split(' ', 1)
        if len(parts) != 2:
            print(f"Warning: Skipping malformed line: '{line}'")
            continue

        timestamp_str, title = parts
        start_ms = to_milliseconds(timestamp_str)

        if start_ms is not None:
            chapters.append({
                "start_ms": start_ms,
                "title": title.strip()
            })

    if not chapters:
        print("Error: No valid chapter lines were parsed.", file=sys.stderr)
        sys.exit(1)

    # Sort chapters by start time, just in case
    chapters.sort(key=lambda x: x['start_ms'])

    # Build the metadata file content
    metadata_content = ";FFMETADATA1\n\n"

    for i, chap in enumerate(chapters):
        start_ms = chap['start_ms']

        # Determine end time
        if i + 1 < len(chapters):
            # End time is the start time of the next chapter
            end_ms = chapters[i+1]['start_ms']
        else:
            # Last chapter goes to the end of the file
            end_ms = total_duration_ms

        # Ensure end time is at least the start time
        if end_ms < start_ms:
            end_ms = start_ms

        title = chap['title']

        metadata_content += "[CHAPTER]\n"
        metadata_content += "TIMEBASE=1/1000\n"
        metadata_content += f"START={start_ms}\n"
        metadata_content += f"END={end_ms}\n"
        # We put the full "Title - Artist" string in the title,
        # as it's the most compatible way.
        metadata_content += f"title={title}\n\n"

    return metadata_content

def embed_chapters(mp3_filename, metadata_content):
    """
    Uses ffmpeg to create a new file with the embedded chapters.
    """
    print("Embedding chapters with ffmpeg...")

    # Write metadata to a temporary file
    try:
        with open(METADATA_FILENAME, "w", encoding="utf-8") as f:
            f.write(metadata_content)
    except IOError as e:
        print(f"Error writing temp metadata file: {e}", file=sys.stderr)
        sys.exit(1)

    # Create the new output filename
    p = Path(mp3_filename)
    output_filename = p.with_name(f"{p.stem}_chapters{p.suffix}")

    command = [
        FFMPEG_EXE,
        "-i", mp3_filename,       # Input 0: original MP3 (with audio + cover)
        "-i", METADATA_FILENAME,  # Input 1: our new chapter metadata
        "-map", "0",              # Map ALL streams from input 0 (audio, image)
        "-map_metadata", "1",     # Map metadata (chapters) from input 1
        "-codec", "copy",         # Copy audio/video streams, don't re-encode
        "-id3v2_version", "3",    # Use ID3v2.3 for max compatibility
        "-y",                     # Overwrite output file if it exists
        str(output_filename)
    ]

    run_command(command, description="ffmpeg chapter embedding")

    # Clean up the temporary metadata file
    try:
        os.remove(METADATA_FILENAME)
    except OSError as e:
        print(f"Warning: Could not remove temp file {METADATA_FILENAME}: {e}")

    print("\n--- Success! ---")
    print(f"New file created: {output_filename}")


def main():
    # 1. Check dependencies first
    check_dependencies()

    # 2. Set up argument parser
    parser = argparse.ArgumentParser(
        description="Download YouTube audio and add chapter markers.",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        "url",
        help="The YouTube video URL to process."
    )
    parser.add_argument(
        "--skip-download",
        action="store_true",
        help="Skip the yt-dlp download. Assumes a file with the"
             " expected name (based on the URL's title) "
             "already exists in the current directory."
    )
    args = parser.parse_args()

    # 3. Get the expected filename
    mp3_filename = get_mp3_filename(args.url) + '.mp3'

    # 4. Handle download (or skip)
    if not args.skip_download:
        download_audio(args.url, mp3_filename)
    else:
        print(f"\n--skip-download specified. Using existing file: {mp3_filename}")
        if not os.path.exists(mp3_filename):
            print(f"Error: File not found! '{mp3_filename}'", file=sys.stderr)
            print("Please run without --skip-download first.", file=sys.stderr)
            sys.exit(1)

    # 5. Get total duration of the file
    total_ms = get_mp3_duration_ms(mp3_filename)
    print(f"Total audio duration: {total_ms / 1000.0} seconds")

    # 6. Get chapter input from user
    chapter_lines = get_user_chapters()

    # 7. Create ffmpeg metadata
    metadata_content = create_ffmpeg_metadata(chapter_lines, total_ms)

    # 8. Embed chapters into a new file
    embed_chapters(mp3_filename, metadata_content)

if __name__ == "__main__":
    main()
