#!/bin/bash


if [ "$#" -ne 2 ]; then
    echo "Please supply an output directory and target directory"
    exit 1
fi

echo "minify html/css/js"

RESOURCE_DIR='resources'
OUTPUT_DIR="$1"
TMP_DIR="/tmp"
MANIFEST_FILE_NAME="manifest.txt"
MANIFEST_FILE_PATH="$OUTPUT_DIR/$MANIFEST_FILE_NAME"
TARGET_DIR="$2"

function minifyHashAndCopyResource() {
  for file in "$RESOURCE_DIR"/$1/* ; do
    echo "minifying $file"
    file_name=`basename $file`
    file_name_without_ext=`basename $file ".$1"`
    minified_output_file="$TMP_DIR/$file_name"

    # Minify
    minify "$file" > "$minified_output_file"

    # Hash
    hash=$(openssl md5 "$minified_output_file" | cut -d' ' -f2)
    output_file="$OUTPUT_DIR/$1/$file_name_without_ext"

    # Copy
    hashed_file_name="$output_file-$hash.$1"
    cp "$minified_output_file" "$hashed_file_name"
    hashed_file_name_only=`basename $hashed_file_name`

    echo "$file_name:$hashed_file_name_only" >> "$MANIFEST_FILE_PATH"
  done
}

function replaceWithHashed(){
  for html_file in "$RESOURCE_DIR"/html/*; do
    file_name=$(basename $html_file)
    output_file_name="$OUTPUT_DIR/html/$file_name"
    cp "$html_file" "$output_file_name"
    while IFS= read -r line; do
      existing=$(echo "$line" | cut -d':' -f1)
      hashed=$(echo "$line" | cut -d':' -f2)
      echo "mapping $existing -> $hashed"
      # use strings ending with " to workaround replacing anything else that may match
      sed -i "s/$existing\"/$hashed\"/g" "$output_file_name"
    done  < "$MANIFEST_FILE_PATH"
  done
}

function resetOutputDirectory() {
  # rm -rf "$OUTPUT_DIR/html"
  # rm -rf "$OUTPUT_DIR/css"
  # rm -rf "$OUTPUT_DIR/js"
  if [ -d "$OUTPUT_DIR" ]; then
    rm -rf $OUTPUT_DIR
  fi

  mkdir -p "$OUTPUT_DIR"/{html,css,js}
}

function copyVerbatim() {
  cp version "$OUTPUT_DIR/version"
  cp -r "$RESOURCE_DIR/favicon" "$OUTPUT_DIR/favicon"
}

function copyProdFiles() {
  cp "$OUTPUT_DIR/version" "$TARGET_DIR"

  # Copied with contents only
  cp "$OUTPUT_DIR"/favicon/* "$TARGET_DIR"
  cp "$OUTPUT_DIR"/html/* "$TARGET_DIR"

  # Copied with parent folder
  # cp -r resources/fonts "$TARGET_DIR"
  cp -r "$OUTPUT_DIR/js" "$TARGET_DIR"
  cp -r "$OUTPUT_DIR/css" "$TARGET_DIR"

  echo "copied resources"
}

resetOutputDirectory
copyVerbatim
minifyHashAndCopyResource 'js'
minifyHashAndCopyResource 'css'
replaceWithHashed
copyProdFiles

