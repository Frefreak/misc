#!/bin/bash

for zip_file in *.zip; do
  dir_name="${zip_file%.zip}"
  mkdir -p "$dir_name"
  unzip -q "$zip_file" -d "$dir_name"
  echo "Extracted $zip_file to $dir_name"
done

echo "Finished extracting all zip files."
